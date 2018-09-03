extern crate buffer;
extern crate libc;
extern crate memchr;
extern crate mio;
extern crate nix;
extern crate readline_sys as sys;

use buffer::ReadBuffer;
use memchr::memchr;
use memchr::memrchr;
use mio::Poll;
use mio::PollOpt;
use mio::unix::EventedFd;
use std::ffi::CStr;
use std::ffi::CString;
use std::fs::File;
use std::io::Write;
use std::io;
use std::mem::ManuallyDrop;
use std::mem;
use std::ops;
use std::os::raw::c_char;
use std::os::raw::c_int;
use std::os::unix::io::AsRawFd;
use std::os::unix::io::FromRawFd;
use std::ptr;
use std::str;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::thread;

mod nix2 {
    pub use nix::fcntl::*;
    pub use nix::unistd::*;
}

#[derive(Debug)]
enum ErrorType {
    AlreadySetUp,
    NotSetUpYet,
    AlreadyShuttingDown,
    Nix(nix::Error),
    Std(io::Error),
}

#[derive(Debug)]
pub struct Error {
    e: ErrorType,
    operation: &'static str,
}

trait ResultExt {
    type T;
    fn o(self, operation: &'static str) -> Result<Self::T, Error>;
}

impl<T> ResultExt for Result<T, nix::Error> {
    type T = T;
    fn o(self, operation: &'static str) -> Result<T, Error> {
        self.map_err(|e| Error { e: ErrorType::Nix(e), operation })
    }
}

impl<T> ResultExt for Result<T, io::Error> {
    type T = T;
    fn o(self, operation: &'static str) -> Result<T, Error> {
        self.map_err(|e| Error { e: ErrorType::Std(e), operation })
    }
}

struct Free<T>(*mut T);

impl<T> ops::Deref for Free<T> {
    type Target = *mut T;
    fn deref(&self) -> &*mut T {
        &self.0
    }
}

impl<T> ops::DerefMut for Free<T> {
    fn deref_mut(&mut self) -> &mut *mut T {
        &mut self.0
    }
}

impl<T> Drop for Free<T> {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.0 as *mut _);
        }
    }
}

fn pipe() -> nix::Result<(File, File)> {
    let (fd1, fd2) = nix2::pipe2(nix2::OFlag::O_CLOEXEC)?;
    unsafe {
        Ok((File::from_raw_fd(fd1), File::from_raw_fd(fd2)))
    }
}

fn fd_as_file(fd: c_int) -> ManuallyDrop<File> {
    unsafe {
        ManuallyDrop::new(File::from_raw_fd(fd))
    }
}

static SIGWINCH_RECEIVED: AtomicBool = AtomicBool::new(false);
extern "C" fn sigwinch(_signal: c_int) {
    SIGWINCH_RECEIVED.store(true, Ordering::Relaxed);
}

fn install_sigwinch_handler() -> Result<(), Error> {
    use nix::sys::signal as s;
    let handler = s::SigHandler::Handler(sigwinch);
    let action = s::SigAction::new(handler, s::SaFlags::empty(), s::SigSet::empty());
    unsafe {
        s::sigaction(s::Signal::SIGWINCH, &action).o("sigaction(SIGWINCH, ...)")?;
    }
    Ok(())
}

static RUNNING: AtomicBool = AtomicBool::new(true);

extern "C" fn linehandler(raw_line: *mut c_char) {
    unsafe {
        let raw_line = Free(raw_line);
        let line;
        if !raw_line.is_null() {
            line = CStr::from_ptr(*raw_line).to_bytes();
        } else {
            line = b"exit";
        }
        if line == b"exit" {
            //RUNNING.store(false, Ordering::Relaxed);
            //sys::rl_callback_handler_remove();
        }
        if !raw_line.is_null() {
            sys::add_history(*raw_line);
        }
        let mut stdout = fd_as_file(1);
        if raw_line.is_null() {
            stdout.write_all(b"exit\n").o("write(1, \"exit\\n\")").unwrap();
        } else {
            writeln!(stdout, "input line: {}", String::from_utf8_lossy(line)).o("write(1, ...)").unwrap();
        }
    }
}

fn cstr(s: &'static str) -> *const c_char {
    CStr::from_bytes_with_nul(s.as_bytes()).unwrap().as_ptr()
}

fn output(mut file: &File, bytes: &[u8]) {
    unsafe {
        let result;
        {
            let saved_point = sys::rl_point;
            let saved_line = Free(sys::rl_copy_text(0, sys::rl_end));
            sys::rl_save_prompt();
            sys::rl_replace_line(cstr("\0"), 0);
            sys::rl_redisplay();

            result = file.write_all(bytes);

            sys::rl_restore_prompt();
            sys::rl_replace_line(*saved_line, 0);
            sys::rl_point = saved_point;
            sys::rl_redisplay();
        }

        result.o("write(real_stdout, ...)").unwrap();
    }
}

struct Data {
    real_stdin: File,
    real_stdout: File,
    pipe_stdin_write: File,
    pipe_stdout_read: File,
    shutdown: mio::Registration,
}

fn loop_(data: Data) -> Result<(), Error> {
    const TOKEN_REAL_STDIN: mio::Token = mio::Token(0);
    const TOKEN_PIPE_STDOUT_READ: mio::Token = mio::Token(2);
    const TOKEN_SHUTDOWN: mio::Token = mio::Token(3);

    let poll = Poll::new().o("Poll::new")?;
    poll.register(&EventedFd(&data.real_stdin.as_raw_fd()), TOKEN_REAL_STDIN, mio::Ready::readable(), PollOpt::level()).o("Poll::register(real_stdin, ...)")?;
    poll.register(&EventedFd(&data.pipe_stdout_read.as_raw_fd()), TOKEN_PIPE_STDOUT_READ, mio::Ready::readable(), PollOpt::level()).o("Poll::register(pipe_stdout_read, ...)")?;
    poll.register(&data.shutdown, TOKEN_SHUTDOWN, mio::Ready::readable(), PollOpt::oneshot()).o("Poll::register(shutdown, ...)")?;
    let mut events = mio::Events::with_capacity(4);
    let mut buffer = Vec::with_capacity(8192);
    while RUNNING.load(Ordering::Relaxed) {
        poll.poll(&mut events, None).o("Poll::poll")?;
        if SIGWINCH_RECEIVED.swap(false, Ordering::Relaxed) {
            unsafe {
                sys::rl_resize_terminal();
            }
        }
        for ev in &events {
            match ev.token() {
                TOKEN_REAL_STDIN => unsafe {
                    sys::rl_callback_read_char();
                },
                TOKEN_PIPE_STDOUT_READ => {
                    if buffer.len() == buffer.capacity() {
                        buffer.reserve(1);
                    }
                    (&data.pipe_stdout_read).read_buffer(&mut buffer).o("read(pipe_stdout_read, ...)")?;
                    if let Some(pos) = memrchr(b'\n', &buffer) {
                        let pos = pos + 1;
                        output(&data.real_stdout, &buffer[..pos]);
                        buffer.drain(..pos);
                    }
                },
                TOKEN_SHUTDOWN => {},
                _ => unreachable!(),
            }
        }
    }
    let result;
    unsafe {
        // Empty line -> remove prompt.
        if *sys::rl_line_buffer == 0 {
            sys::rl_save_prompt();
            sys::rl_redisplay();
            result = Ok(());
        } else {
            result = (&data.real_stdout).write_all(b"\n").o("write(real_stdout, \"\\n\")");
        }
        sys::rl_callback_handler_remove();
    }
    result
}

#[derive(Debug, Default)]
pub struct Chatread<'a> {
    prompt: Option<&'a str>,
}

impl<'a> Chatread<'a> {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn prompt(&mut self, prompt: &'a str) -> &mut Self {
        assert!(memchr(0, prompt.as_bytes()).is_none());
        self.prompt = Some(prompt);
        self
    }
    pub fn setup(self) -> Result<(), Error> {
        setup_impl(&self)
    }
}

pub fn setup() -> Result<(), Error> {
    Chatread::new().setup()
}

fn make_c_streams_linebuffered() {
    extern "C" {
        // Missing from the `libc` crate for some unknown reason...
        pub static mut stdin: *mut libc::FILE;
        pub static mut stdout: *mut libc::FILE;
    }
    unsafe {
        libc::setvbuf(stdin, ptr::null_mut(), libc::_IOLBF, libc::BUFSIZ as usize);
        libc::setvbuf(stdout, ptr::null_mut(), libc::_IOLBF, libc::BUFSIZ as usize);
    }
}

static ALREADY_SET_UP: AtomicBool = AtomicBool::new(false);
static COMPLETELY_SET_UP: AtomicBool = AtomicBool::new(false);
static mut THREAD_HANDLE: Option<thread::JoinHandle<()>> = None;
static mut SHUTDOWN_READINESS: Option<mio::SetReadiness> = None;
fn setup_impl<'a>(options: &Chatread<'a>) -> Result<(), Error> {
    if ALREADY_SET_UP.compare_and_swap(false, true, Ordering::SeqCst) {
        return Err(Error { e: ErrorType::AlreadySetUp, operation: "Chatread::setup" });
    }

    install_sigwinch_handler()?;
    let stdin = fd_as_file(0);
    let stdout = fd_as_file(1);
    let stdin_clone = stdin.try_clone().o("fcntl(0, F_DUPFD_CLOEXEC, 0)")?;
    let stdout_clone = stdout.try_clone().o("fcntl(1, F_DUPFD_CLOEXEC, 0)")?;
    let pipe_stdin = pipe().o("pipe2(..., O_CLOEXEC), stdin")?;
    let pipe_stdout = pipe().o("pipe2(..., O_CLOEXEC), stdout")?;
    nix2::dup2(pipe_stdin.0.as_raw_fd(), stdin.as_raw_fd()).o("dup2(..., 0)")?;
    nix2::dup2(pipe_stdout.1.as_raw_fd(), stdout.as_raw_fd()).o("dup2(..., 1)")?;
    mem::drop(pipe_stdout.1);
    mem::drop(pipe_stdin.0);

    let (shutdown_registration, shutdown_readiness) = mio::Registration::new2();

    // getline(stdin_clone) -- pipe_stdin -> program
    // program -- pipe_stdout -> stdout_clone

    unsafe {
        sys::rl_instream = libc::fdopen(stdin_clone.as_raw_fd(), cstr("r\0"));
        sys::rl_outstream = libc::fdopen(stdout_clone.as_raw_fd(), cstr("w\0"));
        sys::rl_catch_signals = 0;
        sys::rl_catch_sigwinch = 0;
        let cprompt = CString::new(options.prompt.unwrap_or("> ")).unwrap();
        sys::rl_callback_handler_install(cprompt.as_ptr(), Some(linehandler));
    }

    make_c_streams_linebuffered();

    let data = Data {
        real_stdin: stdin_clone,
        real_stdout: stdout_clone,
        pipe_stdout_read: pipe_stdout.0,
        pipe_stdin_write: pipe_stdin.1,
        shutdown: shutdown_registration,
    };

    let thread_handle = thread::spawn(|| {
        match loop_(data) {
            Ok(()) => {},
            e @ Err(_) => {
                unsafe {
                    sys::rl_callback_handler_remove();
                }
                e.unwrap();
            },
        }
    });
    unsafe {
        THREAD_HANDLE = Some(thread_handle);
        SHUTDOWN_READINESS = Some(shutdown_readiness);
        COMPLETELY_SET_UP.store(true, Ordering::SeqCst);
    }
    Ok(())
}

pub fn teardown() -> Result<(), Error> {
    if !COMPLETELY_SET_UP.load(Ordering::SeqCst) {
        return Err(Error { e: ErrorType::NotSetUpYet, operation: "teardown" });
    }
    if !RUNNING.compare_and_swap(true, false, Ordering::SeqCst) {
        return Err(Error { e: ErrorType::AlreadyShuttingDown, operation: "teardown" });
    }
    unsafe {
        SHUTDOWN_READINESS.take().expect("readiness").set_readiness(mio::Ready::readable()).expect("setting readiness");
        THREAD_HANDLE.take().expect("thread handle").join().unwrap();
    }
    Ok(())
}

#[no_mangle]
pub extern "C" fn chatread_options_new() -> *mut Chatread<'static> {
    Box::into_raw(Box::new(Chatread::new()))
}

fn result_to_c<T>(res: Result<(), T>) -> c_int {
    if res.is_ok() {
        0
    } else {
        1
    }
}

#[no_mangle]
pub extern "C" fn chatread_options_prompt(chatread: *mut Chatread<'static>, prompt: *const c_char) -> c_int {
    let chatread = unsafe { &mut *chatread };
    let prompt = unsafe { CStr::from_ptr(prompt).to_bytes() };
    result_to_c(str::from_utf8(prompt).map(|p| chatread.prompt = Some(p)))
}

#[no_mangle]
pub extern "C" fn chatread_options_setup(chatread: *mut Chatread<'static>) -> c_int {
    let chatread = unsafe { ptr::read(chatread) };
    result_to_c(chatread.setup())
}

#[no_mangle]
pub extern "C" fn chatread_setup() -> c_int {
    result_to_c(setup())
}

#[no_mangle]
pub extern "C" fn chatread_teardown() -> c_int {
    result_to_c(teardown())
}
