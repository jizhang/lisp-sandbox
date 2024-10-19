#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HASHSIZE 101

struct nlist {
  char *name;
  char *defn;
  struct nlist *next;
};
struct nlist *hashtab[HASHSIZE];
struct nlist *install(char *, char *);
struct nlist *lookup(char *);
void undef(char *);

int main() {
  install("hello", "world");
  install("emacs", "rocks");
  install("hello", "jerry");
  printf("%s\n", lookup("hello")->defn);

  undef("hello");
  printf("%d\n", lookup("hello") == NULL);
}

unsigned hash(char *s) {
  unsigned hashval;
  for (hashval = 0; *s != '\0'; ++s) {
    hashval = *s + 31 * hashval;
  }
  return hashval % HASHSIZE;
}

struct nlist *lookup(char *s) {
  for (struct nlist *np = hashtab[hash(s)]; np != NULL; np = np->next) {
    if (strcmp(s, np->name) == 0) {
      return np;
    }
  }
  return NULL;
}

struct nlist *install(char *name, char *defn) {
  struct nlist *np = lookup(name);
  if (np == NULL) {
    np = (struct nlist *) malloc(sizeof(*np));
    if (np == NULL) {
      return NULL;
    }

    np->name = strdup(name);
    np->defn = strdup(defn);
    if (np->name == NULL || np->defn == NULL) {
      return NULL;
    }

    unsigned hashval = hash(name);
    np->next = hashtab[hashval];
    hashtab[hashval] = np;
  } else {
    free(np->defn);
    np->defn = strdup(defn);
    if (np->defn == NULL) {
      return NULL;
    }
  }

  return np;
}

void undef(char *name) {
  unsigned hashval = hash(name);
  struct nlist *prev = NULL;
  struct nlist *np = hashtab[hashval];
  while (np != NULL) {
    if (strcmp(name, np->name) == 0) {
      if (prev == NULL) {
        hashtab[hashval] = np->next;
      } else {
        prev->next = np->next;
      }
      free(np->name);
      free(np->defn);
      free(np);
      return;
    }
    prev = np;
    np = np->next;
  }
}

/* Local Variables: */
/* compile-command: "gcc hash_table.c -o hello && hello" */
/* End: */
