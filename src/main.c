#include <config.h>
#include <stdio.h>
#include "cmd.h"
int
main(int argc, char **argv)
{
  struct cmd_options opts;
  parse_cmdopts(&opts, argc, argv);
  return 0;
}
