#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "USAGE: %s string\n", argv[0]);
    return 1;
  }

  char buffer[10];
  strncpy(buffer, argv[1], sizeof(buffer));
  buffer[sizeof(buffer) - 1] = '\0';

  printf("%s\n", buffer);
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc command_args.c -o hello && hello" */
/* End: */
