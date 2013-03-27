#ifndef ERROR_H
#define ERROR_H
#include <stdio.h>
#include <stdlib.h>
#define warning(...) do {                                           \
    fprintf(stderr, "[%s:%s():%d]\tError: ", __FILE__, __func__, __LINE__);      \
    fprintf(stderr, __VA_ARGS__);                                       \
    fprintf(stderr, "\n");                                              \
  } while (0)

#define fatal(...) do {                         \
  warning(__VA_ARGS__); \
  exit(EXIT_FAILURE); \
  } while (0)

#define debug(...) do {                         \
  fprintf(stderr, __VA_ARGS__);                 \
} while (0)

#endif
