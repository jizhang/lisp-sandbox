#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_ELEMENTS(x) (sizeof(x) / sizeof(*(x)))

void print_array(int arr[], size_t len) {
  printf("[");
  for (int i = 0; i < len; ++i) {
    printf("%d", arr[i]);
    if (i + 1 != len) {
      printf(" ");
    }
  }
  printf("]\n");
}

void delete_element(int arr[], size_t len, int index) {
  if (index < 0 || index >= len) {
    return;
  }

  for (int i = index; i < len; ++i) {
    if (i + 1 != len) {
      arr[i] = arr[i + 1];
    }
  }
}

/* Given an array of 10 integers, remove all duplicate elements from the array,
   and create a new array without duplicate elements (a set). */
int *remove_duplicates(int arr[], size_t len, size_t *result_length) {
  int *result = calloc(len, sizeof(*arr));
  memcpy(result, arr, len * sizeof(*arr));

  for (int i = 0; i < len; ++i) {
    for (int j = i + 1; j < len; ++j) {
      if (result[i] == result[j]) {
        delete_element(result, len, j);
        --len;
        --j;
      }
    }
  }

  result = reallocarray(result, len, sizeof(*arr));
  *result_length = len;
  return result;
}

int main(void) {
  int arr[] = { 1, 2, 2, 3, 4, 4, 5, 6, 5, 7 };
  size_t result_length;
  int *result = remove_duplicates(arr, NUM_ELEMENTS(arr), &result_length);
  print_array(result, result_length);
  free(result);
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc remove_duplicates.c -o hello && hello" */
/* End: */
