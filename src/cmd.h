#ifndef CMD_H
#define CMD_H
struct cmd_options {
  const char *conf_filename;
  int debug;
};
void parse_cmdopts(struct cmd_options *opts, int argc, char **argv);

#endif
