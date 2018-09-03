#ifndef CHATREAD_H
#define CHATREAD_H
struct chatread_options;

int chatread_options_prompt(struct chatread_options *chatread, const char *prompt);

struct chatread_options *chatread_options_new();
int chatread_options_setup(struct chatread_options *chatread);
int chatread_setup(void);
int chatread_teardown(void);
#endif
