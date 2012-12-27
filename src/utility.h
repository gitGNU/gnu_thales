#ifndef UTILITY_H
#define UTILITY_H
#include <xalloc.h>
#include <xstrndup.h>

#define countof(x) sizeof(x)/sizeof(0[x])
static inline void *
xrangedup (const void *begin, const void *end)
{
  return xstrndup (begin, (char *) end - (char *) begin);
}
static inline char*
xstrdup_safe(const char *ptr)
{
  return ptr ? xstrdup(ptr) : NULL;
}

#endif
