#include <stdio.h>

#define MAXLINE 1000

int readline(char line[], int maxline) {
  int c;
  int i = 0;
  while (i < maxline - 1 && (c = getchar()) != EOF && c != '\n') {
    line[i++] = c;
  }

  if (c == '\n') {
    line[i++] = c;
  }

  line[i] = '\0';
  return i;
}

void copy(char to[], char from[]) {
  int i = 0;
  while ((to[i] = from[i]) != '\0') {
    ++i;
  }
}

int main() {
  char line[MAXLINE];
  char longest[MAXLINE];
  int len;
  int max = 0;

  while ((len = readline(line, MAXLINE)) > 0) {
    if (len > max) {
      max = len;
      copy(longest, line);
    }
  }

  if (max > 0) {
    printf("%s", longest);
  }

  return 0;
}


/* Local Variables: */
/* compile-command: "gcc longest_line.c -o hello && cat hello.c | hello" */
/* End: */
