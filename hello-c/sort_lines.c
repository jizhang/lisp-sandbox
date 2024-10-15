#include <stdio.h>
#include <string.h>

#define MAXLINES 5000
char *lineptr[MAXLINES];
int readlines(char *lineptr[], int nlines);
void writelines(char *lineptr[], int nlines);
void qsort(char *lineptr[], int left, int right);

int main() {
  int nlines = readlines(lineptr, MAXLINES);
  if (nlines == -1) {
    printf("error: input too big to sort\n");
    return 1;
  }

  qsort(lineptr, 0, nlines - 1);
  writelines(lineptr, nlines);
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
void swap(char *v[], int i, int j);

void qsort(char *v[], int left, int right) {
  if (left >= right) {
    return;
  }

  swap(v, left, (left + right) / 2);
  int last = left;
  for (int i = left + 1; i <= right; ++i) {
    if (strcmp(v[i], v[left]) < 0) {
      swap(v, ++last, i);
    }
  }
  swap(v, left, last);
  qsort(v, left, last - 1);
  qsort(v, last + 1, right);
}

void swap(char *v[], int i, int j) {
  char *temp = v[i];
  v[i] = v[j];
  v[j] = temp;
}

/* Local Variables: */
/* compile-command: "gcc sort_lines.c -o hello && cat sort_lines.txt | hello" */
/* End: */
