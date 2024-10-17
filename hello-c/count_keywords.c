#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define MAXWORD 100
#define NKEYS (sizeof keytab / sizeof(keytab[0]))

struct key {
  char *word;
  int count;
};

int getword(char *, int);
int binsearch(char *, struct key *, int);

struct key keytab[] = {
  "auto", 0,
  "break", 0,
  "case", 0,
  "char", 0,
  "const", 0,
  "continue", 0,
  "default", 0,
  /* ... */
  "unsigned", 0,
  "void", 0,
  "volatile", 0,
  "while", 0
};

int main() {
  char word[MAXWORD];
  while (getword(word, MAXWORD) != EOF) {
    if (isalpha(word[0])) {
      int n = binsearch(word, keytab, NKEYS);
      if (n != -1) {
        ++keytab[n].count;
      }
    }
  }

  for (int n = 0; n < NKEYS; ++n) {
    if (keytab[n].count > 0) {
      printf("%4d %s\n", keytab[n].count, keytab[n].word);
    }
  }

  return 0;
}

int binsearch(char *word, struct key tab[], int n) {
  int low = 0;
  int high = n - 1;
  while (low <= high) {
    int mid = (low + high) / 2;
    int cond = strcmp(word, tab[mid].word);
    if (cond < 0) high = mid - 1;
    else if (cond > 0) low = mid + 1;
    else return mid;
  }
  return -1;
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

/* Local Variables: */
/* compile-command: "gcc count_keywords.c getch.c -o hello && cat count_keywords.c | hello" */
/* End: */
