#include <stdio.h>
#include <ctype.h>

#define SIZE 100

int getch(void);
void ungetch(int);

int getint(int *pn) {
  int c;
  while (isspace(c = getch()));

  if (!isdigit(c) && c != EOF && c != '+' && c != '-') {
    ungetch(c);
    return 0;
  }

  int sign = c == '-' ? -1 : 1;
  if (c == '+' || c == '-') {
    c = getch();
  }

  for (*pn = 0; isdigit(c); c = getch()) {
    *pn = *pn * 10 + (c - '0');
  }
  *pn *= sign;

  if (c != EOF) {
    ungetch(c);
  }

  return c;
}

int main(void) {
  int n, array[SIZE];
  for (n = 0; n < SIZE && getint(&array[n]) != EOF; ++n);
  for (int i = 0; i < n; ++i) {
    printf("%d ", array[i]);
  }
  printf("\n");
}

/* Local Variables: */
/* compile-command: "gcc getint.c getch.c -o hello && cat getint.txt | hello" */
/* End: */
