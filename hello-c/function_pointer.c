#include <stdio.h>

#define THE_BIGGEST 100

typedef int (*my_function_type)(int, void *);
typedef struct {
  int color_spec;
  char *phrase;
} data_needed;
enum color { RED, GREEN, BLUE };

static data_needed silly_stuff = { BLUE, "Watcha talkin 'bout Willis?" };

int do_something_nice(int value, my_function_type func, void *data) {
  int rv = 0;
  if (value < THE_BIGGEST) {
    rv = (*func)(value, data);
  } else {
    rv = func(value, data);
  }
  return rv;
}

int talk_jive(int number, void *some_stuff) {
  data_needed *my_data = some_stuff;
  return 5;
}

int main(void) {
  int rv = do_something_nice(41, &talk_jive, &silly_stuff);
  printf("%d\n", rv);
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc function_pointer.c -o hello.exe && hello" */
/* End: */
