#include <stdio.h>
#include <ctype.h>

#define BUFSIZE 100

static char buf[BUFSIZE];
static int bufp = 0;

int getch(void) {
  return bufp > 0 ? buf[--bufp] : getchar();
}

void ungetch(int c) {
  if (bufp < BUFSIZE) {
    buf[bufp++] = c;
  } else {
    printf("ungetch: too many characters\n");
  }
}

int getword(char *word, int lim) {
  int getch(void);
  void ungetch(int);

  int c;
  while (isspace(c = getch()));

  char *w = word;
  if (c != EOF) {
    *w++ = c;
  }

  if (!isalpha(c)) {
    *w = '\0';
    return c;
  }

  for (; --lim > 0; ++w) {
    if (!isalnum(*w = getch())) {
      ungetch(*w);
      break;
    }
  }

  *w = '\0';
  return word[0];
}
