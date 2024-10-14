#include <stdio.h>

#define LEN(x) (sizeof(x) / sizeof(*(x)))

void swap(int nums[], int a, int b) {
  int tmp = nums[a];
  nums[a] = nums[b];
  nums[b] = tmp;
}

void shell_sort(int nums[], int len) {
  for (int gap = len / 2; gap > 0; gap /= 2) {
    for (int i = gap; i < len; ++i) {
      for (int j = i - gap; j >= 0 && nums[j] > nums[j + gap]; j -= gap) {
        swap(nums, j, j + gap);
      }
    }
  }
}

void quick_sort(int nums[], int left, int right) {
  if (left >= right) {
    return;
  }

  swap(nums, left, (left + right) / 2);
  int last = left;
  for (int i = left + 1; i <= right; ++i) {
    if (nums[i] < nums[left]) {
      swap(nums, ++last, i);
    }
  }
  swap(nums, left, last);
  quick_sort(nums, left, last - 1);
  quick_sort(nums, last + 1, right);
}

void print_array(int nums[], int len) {
  printf("[");
  for (int i = 0; i < len; ++i) {
    printf("%d", nums[i]);
    if (i < len - 1) {
      printf(" ");
    }
  }
  printf("]\n");
}

int main() {
  int nums[] = { 11, 8, 3, 9, 7, 1, 2, 5 };
  // shell_sort(nums, LEN(nums));
  quick_sort(nums, 0, LEN(nums) - 1);
  print_array(nums, LEN(nums));
}

/* Local Variables: */
/* compile-command: "gcc sort.c -o hello && hello" */
/* End: */
