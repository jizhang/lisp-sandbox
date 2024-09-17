#include <stdio.h>
#include <stdlib.h>

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

int main(void) {
  widget *x = create_widget(100);
  printf("%d\n", *x->member);
  destroy_widget(x);
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc widget_constructor.c -o hello && hello" */
/* End: */
