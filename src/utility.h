#ifndef UTILITY_H
#define UTILITY_H
#include <xalloc.h>
#include <xstrndup.h>

static inline void *
xrangedup (const void *begin, const void *end)
{
  return xstrndup (begin, (char *) end - (char *) begin);
}

#endif
