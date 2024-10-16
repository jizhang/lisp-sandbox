#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 1000

int main(int argc, char *argv[]) {
  int except = 0;
  int number = 0;

  while (--argc > 0 && (*++argv)[0] == '-') {
    char c;
    while (c = *++argv[0]) {
      switch (c) {
      case 'x':
        except = 1;
        break;
      case 'n':
        number = 1;
        break;
      default:
        printf("find: illegal option %c\n", c);
        return EXIT_FAILURE;
      }
    }
  }

  if (argc != 1) {
    printf("Usage: find -x -n pattern\n");
    return EXIT_FAILURE;
  }

  int lineno = 0;
  char line[MAXLINE];
  while (fgets(line, MAXLINE, stdin) != NULL) {
    ++lineno;
    if ((strstr(line, *argv) != NULL) != except) {
      if (number) {
        printf("%d:", lineno);
      }
      printf("%s", line);
    }
  }

  return EXIT_SUCCESS;
}

/* Local Variables: */
/* compile-command: "gcc find.c -o hello && cat find.c | hello -n NULL" */
/* End: */
