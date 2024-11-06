#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

volatile int counter = 0;
int loops;

void *worker(void *);

int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: threads <value>\n");
    return EXIT_FAILURE;
  }

  printf("Initial value: %d\n", counter);

  loops = atoi(argv[1]);
  pthread_t p1, p2;
  pthread_create(&p1, NULL, worker, NULL);
  pthread_create(&p2, NULL, worker, NULL);
  pthread_join(p1, NULL);
  pthread_join(p2, NULL);

  printf("Final value: %d\n", counter);
  return EXIT_SUCCESS;
}

void *worker(void *arg) {
  for (int i = 0; i < loops; ++i) {
    ++counter;
  }
  return NULL;
}

/* Local Variables: */
/* compile-command: "gcc threads.c -o hello && sh -c './hello 1000'" */
/* End: */
