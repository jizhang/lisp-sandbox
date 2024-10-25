#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

#define MAX_PATH 1024

int main(int argc, char *argv[]) {
  void fsize(char *);

  if (argc == 1) {
    fsize(".");
  } else {
    while (--argc > 0) {
      fsize(*++argv);
    }
  }
  return 0;
}

void fsize(char *name) {
  void dirwalk(char *, void (*)(char *));

  struct stat stbuf;
  if (stat(name, &stbuf) == -1) {
    fprintf(stderr, "fsize: can't access %s\n", name);
    return;
  }

  if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
    dirwalk(name, fsize);
  } else {
    printf("%8ld %s\n", stbuf.st_size, name);
  }
}

void dirwalk(char *dir, void (*f)(char *)) {
  int endswith(char *, char *);

  DIR *dfd = opendir(dir);
  if (dfd == NULL) {
    fprintf(stderr, "dirwalk: can't open %s\n", dir);
    return;
  }

  char name[MAX_PATH];
  struct dirent *dp;
  while ((dp = readdir(dfd)) != NULL) {
    if (strcmp(dp->d_name, ".") == 0
        || strcmp(dp->d_name, "..") == 0
        || strcmp(dp->d_name, ".git") == 0
        || endswith(dp->d_name, "~")
        || endswith(dp->d_name, "#")
        || endswith(dp->d_name, ".bak")) {
      continue;
    }

    if (strlen(dir) + strlen(dp->d_name) + 2 > MAX_PATH) {
      fprintf(stderr, "dirwalk: name %s/%s too long\n", dir, dp->d_name);
      continue;
    }

    sprintf(name, "%s/%s", dir, dp->d_name);
    (*f)(name);
  }

  closedir(dfd);
}

int endswith(char *s, char * suffix) {
  int s_len = strlen(s);
  int suffix_len = strlen(suffix);
  return s_len >= suffix_len && strcmp(s + s_len - suffix_len, suffix) == 0;
}

/* Local Variables: */
/* compile-command: "gcc fsize.c -o hello && hello" */
/* End: */
