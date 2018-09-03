extern crate chatread;

use std::thread;
use std::time::Duration;

fn main() -> Result<(), chatread::Error> {
    chatread::setup()?;
    for i in (0..10).rev() {
        thread::sleep(Duration::from_secs(1));
        println!("{}", i);
    }
    chatread::teardown()?;
    Ok(())
}
