#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define MAXWORD 100
#define NKEYS (sizeof(keywords) / sizeof(*keywords))

struct key {
  char *word;
  int count;
};

int getword(char *, int);
struct key *generate_keytab();
struct key *binsearch(char *, struct key *, int);

char *keywords[] = {
  "auto", "double", "int", "struct",
  "break", "else", "long", "switch",
  "case", "enum", "register", "typedef",
  "char", "extern", "return", "union",
  "const", "float", "short", "unsigned",
  "continue", "for", "signed", "void",
  "default", "goto", "sizeof", "volatile",
  "do", "if", "static", "while"
};

int main() {
  struct key *keytab = generate_keytab();
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

struct key *generate_keytab() {
  struct key *keytab = calloc(NKEYS, sizeof(struct key));

  for (int i = 0; i < NKEYS; ++i) {
    keytab[i].word = keywords[i];
    keytab[i].count = 0;
  }

  return keytab;
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

/* Local Variables: */
/* compile-command: "gcc count_keywords.c getch.c -o hello && cat count_keywords.c | sh -c ./hello" */
/* End: */
