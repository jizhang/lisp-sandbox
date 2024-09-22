#include <stdio.h>
#include <string.h>
#include "getopt.h"

int opterr = 1;
int optind = 1;
int optopt;
char *optarg;

int getopt(int argc, char **argv, char *opts) {
  static int sp = 1;
  register char *cp;

  if (sp == 1) {
    if (optind >= argc || argv[optind][0] != '-' || argv[optind][1] == '\0') {
      return EOF;
    }

    if (strcmp(argv[optind], "--") == 0) {
      ++optind;
      return EOF;
    }
  }

  optopt = argv[optind][sp];

  if (optopt == ':' || (cp = strchr(opts, optopt)) == NULL) {
    if (opterr) {
      fprintf(stderr, "%s: invalid option -- '%c'\n", argv[0], optopt);
    }

    if (argv[optind][++sp] == '\0') {
      ++optind;
      sp = 1;
    }

    optarg = NULL;
    return '?';
  }

  if (*++cp == ':') {
    if (argv[optind][sp + 1] != '\0') {
      optarg = argv[optind++] + (sp + 1);
    } else if (++optind >= argc) {
      if (opterr) {
        fprintf(stderr, "%s: option requires an argument -- '%c'\n", argv[0], optopt);
      }
      sp = 1;
      optarg = NULL;
      return '?';
    } else {
      optarg = argv[optind++];
    }

    sp = 1;

  } else {
    if (argv[optind][++sp] == '\0') {
      sp = 1;
      ++optind;
    }
    optarg = NULL;
  }

  return optopt;
}

int main(void) {
  char *argv[] = { "getopt", "-i", "-jhello", "-j", "world", "-kj" };
  int argc = 6;
  char *opts = "ij:";

  int res;
  while ((res = getopt(argc, argv, opts)) != EOF) {
    printf("%c %s\n", res, optarg);
  }

  return 0;
}

/* Local Variables: */
/* compile-command: "gcc getopt.c -o hello && hello" */
/* End: */
