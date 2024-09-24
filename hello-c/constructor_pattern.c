#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define FREE(p) do { free(p); (p) = NULL; } while(0)

// https://en.wikibooks.org/wiki/C_Programming/Memory_management#Write_constructor/destructor_functions
typedef struct {
  int *member;
} widget;

void widget_constructor(widget *this, int x) {
  this->member = calloc(1, sizeof(int));
  *this->member = x;
}

void widget_destructor(widget *this) {
  free(this->member);
}

widget *create_widget(int m) {
  widget *x = calloc(1, sizeof(widget));
  if (x == NULL) {
    abort();
  }

  widget_constructor(x, m);
  return x;
}

void destroy_widget(widget *this) {
  widget_destructor(this);
  free(this);
}

// https://en.wikibooks.org/wiki/C_Programming/Common_practices#Constructors_and_destructors
struct string {
  size_t size;
  char *data;
};

struct string *create_string(const char *initial) {
  assert(initial != NULL);
  struct string *new_string = calloc(1, sizeof(struct string));
  if (new_string != NULL) {
    new_string->size = strlen(initial);
    new_string->data = strdup(initial);
  }
  return new_string;
}

void free_string(struct string **s) {
  assert(s != NULL && *s != NULL);
  FREE((*s)->data);
  FREE(*s);
}

int main(void) {
  widget *x = create_widget(100);
  printf("%d\n", *x->member);
  destroy_widget(x);

  struct string *s = create_string("hello world");
  printf("%s %d\n", s->data, s->size);
  free_string(&s);

  return 0;
}

/* Local Variables: */
/* compile-command: "gcc constructor_pattern.c -o hello && hello" */
/* End: */
