#ifndef ERROR_H
#define ERROR_H
#include <stdio.h>
#include <stdlib.h>
#define fatal_error(...) do {                                           \
    fprintf(stderr, "[%s:%s():%d]\t", __FILE__, __func__, __LINE__);      \
    fprintf(stderr, __VA_ARGS__);                                       \
    fprintf(stderr, "\n");                                              \
    exit(EXIT_FAILURE);                                                 \
  } while (0)

#endif
