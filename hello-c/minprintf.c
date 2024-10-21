#include <stdio.h>
#include <stdarg.h>

void minprintf(char *fmt, ...);

int main() {
  minprintf("hello %s %d\n", "world", 123);
}

void minprintf(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  for (char *p = fmt; *p; ++p) {
    if (*p != '%') {
      putchar(*p);
      continue;
    }

    switch (*++p) {
    case 'd':
      printf("%d", va_arg(ap, int));
      break;
    case 'f':
      printf("%f", va_arg(ap, double));
      break;
    case 's':
      for (char *sval = va_arg(ap, char *); *sval; ++sval) {
        putchar(*sval);
      }
      break;
    default:
      putchar(*p);
      break;
    }
  }

  va_end(ap);
}


/* Local Variables: */
/* compile-command: "gcc minprintf.c -o hello && hello" */
/* End: */
