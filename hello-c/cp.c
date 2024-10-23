#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <fcntl.h>
#include <unistd.h>

#define PERMS 0644

void error(char *, ...);

int main(int argc, char *argv[]) {
  if (argc != 3) {
    error("Usage: cp from to");
  }

  int from = open(argv[1], O_RDONLY, 0);
  if (from == -1) {
    error("cp: can't open %s", argv[1]);
  }

  int to = creat(argv[2], PERMS);
  if (to == -1) {
    error("cp: can't create %s, mode %03o", argv[2], PERMS);
  }

  int n;
  char buf[BUFSIZ];
  while ((n = read(from, buf, BUFSIZ)) > 0) {
    if (write(to, buf, n) != n) {
      error("cp: write error on file %s", argv[2]);
    }
  }

  return EXIT_SUCCESS;
}

void error(char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
  exit(EXIT_FAILURE);
}

/* Local Variables: */
/* compile-command: "gcc cp.c -o hello" */
/* End: */
