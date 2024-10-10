#include <stdio.h>

int binary_search(int x, int values[], size_t length) {
  int low = 0;
  int high = length - 1;
  while (low <= high) {
    int mid = low + (high - low) / 2;
    if (x < values[mid]) {
      high = mid - 1;
    } else if (x > values[mid]) {
      low = mid + 1;
    } else {
      return mid;
    }
  }
  return -1;
}

int main() {
  int values[] = { -1, 0, 3, 5, 9, 12 };
  int pos = binary_search(9, values, sizeof(values) / sizeof(*values));
  printf("%d\n", pos);
}

/* Local Variables: */
/* compile-command: "gcc binary_search.c -o hello && hello" */
/* End: */
