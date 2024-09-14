#include <stdarg.h>

float average(int num_args, ...) {
  va_list args;
  va_start(args, num_args);

  int sum = 0;
  int count = 0;

  while (count < num_args) {
    sum += va_arg(args, int);
    ++count;
  }

  va_end(args);

  return (float) sum / count;
}
