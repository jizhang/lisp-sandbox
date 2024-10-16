#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINES 5000
char *lineptr[MAXLINES];
int readlines(char *lineptr[], int nlines);
void writelines(char *lineptr[], int nlines);
void qsort1(void *lineptr[], int left, int right, int (*comp)(void *, void *));
int numcmp(char *, char *);

int main(int argc, char *argv[]) {
  int numeric = argc > 1 && strcmp(argv[1], "-n") == 0;
  int nlines = readlines(lineptr, MAXLINES);
  if (nlines == -1) {
    printf("error: input too big to sort\n");
    return 1;
  }

  qsort1((void **) lineptr, 0, nlines - 1,
         numeric ? (int (*)(void *, void *)) numcmp : (int (*)(void *, void *)) strcmp);
  writelines(lineptr, nlines);
  return 0;
}

int numcmp(char *s1, char *s2) {
  double v1 = atof(s1);
  double v2 = atof(s2);
  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  return 0;
}


/* I/O */
#define MAXLEN 1000
int getline1(char *, int);
char *alloc(int);

int readlines(char *lineptr[], int maxlines) {
  int nlines = 0;

  int len;
  char line[MAXLEN];
  while ((len = getline1(line, MAXLEN)) > 0) {
    char *p;
    if (nlines >= maxlines || (p = alloc(len)) == NULL) {
      return -1;
    }

    line[len - 1] = '\0'; // Delete \n
    strcpy(p, line);
    lineptr[nlines++] = p;
  }

  return nlines;
}

void writelines(char *lineptr[], int nlines) {
  while (nlines-- > 0) {
    printf("%s\n", *lineptr++);
  }
}

int getline1(char s[], int lim) {
  int i, c;
  for (i = 0; i < lim - 1 && (c = getchar()) != EOF && c != '\n'; ++i) {
    s[i] = c;
  }

  if (c == '\n') {
    s[i] = c;
    ++i;
  }

  s[i] = '\0';
  return i;
}


/* Alloc */
#define ALLOCSIZE 10000
char allocbuf[ALLOCSIZE];
char *allocp = allocbuf;

char *alloc(int n) {
  if (allocbuf + ALLOCSIZE - allocp >= 0) {
    allocp += n;
    return allocp - n;
  }
  return NULL;
}

void afree(char *p) {
  if (p >= allocbuf && p < allocbuf + ALLOCSIZE) {
    allocp = p;
  }
}


/* Sort */
void swap(void *v[], int i, int j);

void qsort1(void *v[], int left, int right, int (*comp)(void *, void *)) {
  if (left >= right) {
    return;
  }

  swap(v, left, (left + right) / 2);
  int last = left;
  for (int i = left + 1; i <= right; ++i) {
    if ((*comp)(v[i], v[left]) < 0) {
      swap(v, ++last, i);
    }
  }
  swap(v, left, last);
  qsort1(v, left, last - 1, comp);
  qsort1(v, last + 1, right, comp);
}

void swap(void *v[], int i, int j) {
  void *temp = v[i];
  v[i] = v[j];
  v[j] = temp;
}

/* Local Variables: */
/* compile-command: "gcc sort_lines.c -o hello && cat sort_lines.txt | hello" */
/* End: */
