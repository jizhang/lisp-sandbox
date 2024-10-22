#include <stdio.h>
#include <stdlib.h>

void filecopy(FILE *in, FILE *out);

int main(int argc, char *argv[]) {
  if (argc == 1) {
    filecopy(stdin, stdout);
    return EXIT_SUCCESS;
  }

  while (--argc > 0) {
    FILE *in = fopen(*++argv, "r");
    if (in == NULL) {
      fprintf(stderr, "cat: can't open %s\n", *argv);
      return EXIT_FAILURE;
    }

    filecopy(in, stdout);
    fclose(in);
  }

  if (ferror(stdout)) {
    fprintf(stderr, "cat: error writing stdout\n");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

void filecopy(FILE *in, FILE *out) {
  int c;
  while ((c = getc(in)) != EOF) {
    putc(c, out);
  }
}

/* Local Variables: */
/* compile-command: "gcc cat.c -o hello && hello cat.c hello.c" */
/* End: */
