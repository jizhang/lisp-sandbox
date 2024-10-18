#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define MAXWORD 100

struct tnode {
  char *word;
  int count;
  struct tnode *left;
  struct tnode *right;
};
struct tnode *addtree(struct tnode *, char *);
void treeprint(struct tnode *);
int getword(char *, int);

int main() {
  struct tnode *root = NULL;
  char word[MAXWORD];
  while (getword(word, MAXWORD) != EOF) {
    if (isalpha(word[0])) {
      root = addtree(root, word);
    }
  }

  treeprint(root);
  return 0;
}


/* Binary tree */
struct tnode *talloc(void);
char *strdup1(char *);

struct tnode *addtree(struct tnode *p, char *w) {
  if (p == NULL) {
    p = talloc();
    p->word = strdup1(w);
    p->count = 1;
    p->left = p->right = NULL;
  } else {
    int cond = strcmp(w, p->word);
    if (cond == 0) {
      ++p->count;
    } else if (cond < 0) {
      p->left = addtree(p->left, w);
    } else {
      p->right = addtree(p->right, w);
    }
  }

  return p;
}

void treeprint(struct tnode *p) {
  if (p != NULL) {
    treeprint(p->left);
    printf("%4d %s\n", p->count, p->word);
    treeprint(p->right);
  }
}


/* Allocation */
struct tnode *talloc(void) {
  return (struct tnode *) malloc(sizeof(struct tnode));
}

char *strdup1(char *s) {
  char *p = (char *) malloc(strlen(s) + 1);
  if (p != NULL) {
    strcpy(p, s);
  }
  return p;
}

/* Local Variables: */
/* compile-command: "gcc binary_tree.c getch.c -o hello && cat binary_tree.c | hello" */
/* End: */
