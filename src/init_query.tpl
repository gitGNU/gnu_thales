[+autogen5 template h c +][+DEFINE
GPL +]/*
[+ (gpl "GNU Thales" " * " ) +]
 */
[+ENDDEF +][+DEFINE
DO_NOT_EDIT +]/*
[+(dne " * ") +]
*/
[+ENDDEF +][+
CASE (suffix) +][+
==  h  +][+ DO_NOT_EDIT +]
[+GPL+]
[+ (make-header-guard "") +]

#define INIT_QUERIES_COUNT [+ (count "query") +]
extern char const *init_queries[ INIT_QUERIES_COUNT ];
extern char const *init_queries_error_messages[ INIT_QUERIES_COUNT];
#endif /* [+ (. header-guard) +] */ [+
==  c  +][+ DO_NOT_EDIT +]
[+ GPL +]

#include "[+ (. header-file)  +]"

char const *init_queries[] = { [+
FOR query "," +]
  "[+ value +]"[+
ENDFOR query +]
};

char const *init_queries_error_messages[] = { [+
FOR query "," +]
  "[+ error_msg +]"[+
ENDFOR query +]
};

[+ESAC +]
