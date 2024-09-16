#include <stdio.h>

// https://en.wikibooks.org/wiki/C_Programming/Pointers_and_arrays#sizeof
#define NUM_ELEMENT(x) (sizeof(x) / sizeof(*(x)))

void print_string_with_length(char s[], size_t len) {
  for (int i = 0; i < len; ++i) {
    printf("%c", s[i]);
  }
}

void print_null_terminated_string(char s[]) {
  for (int i = 0; s[i] != '\0'; ++i) {
    printf("%c", s[i]);
  }
}

typedef struct {
  char *s;
  size_t len;
} my_str;

void print_structured_string(my_str *s) {
  print_string_with_length(s->s, s->len);
}

int main(void) {
  char s1[] = "hello";
  print_string_with_length(s1, NUM_ELEMENT(s1) - 1);
  printf("\n");

  char s2[10] = "world";
  print_null_terminated_string(s2);
  printf("\n");

  my_str s3 = { "emacs", 5 };
  print_structured_string(&s3);
  printf("\n");

  return 0;
}

/* Local Variables: */
/* compile-command: "gcc print_string.c -o hello && hello" */
/* End: */
