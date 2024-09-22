#ifndef GETOPT_H
#define GETOPT_H

extern int opterr, optind, optopt;
extern char *optarg;

int getopt(int, char **, char *);

#endif
