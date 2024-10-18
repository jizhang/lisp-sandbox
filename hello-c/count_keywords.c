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
struct key *binsearch(char *, struct key *, int);

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
      struct key *p = binsearch(word, keytab, NKEYS);
      if (p != NULL) {
        ++p->count;
      }
    }
  }

  for (struct key *p = keytab; p < keytab + NKEYS; ++p) {
    if (p->count > 0) {
      printf("%4d %s\n", p->count, p->word);
    }
  }

  return 0;
}

struct key *binsearch(char *word, struct key tab[], int n) {
  if (n == 0) return NULL;

  struct key *low = &tab[0];
  struct key *high = &tab[n];
  while (low < high) {
    struct key *mid = low + (high - low) / 2;
    int cond = strcmp(word, mid->word);
    if (cond < 0) high = mid;
    else if (cond > 0) low = mid + 1;
    else return mid;
  }

  return NULL;
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
