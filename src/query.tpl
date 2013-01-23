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

[+FOR query "" +]
/* [+ comment +] */
#define QUERY_[+def+] [+ (for-index) +]
[+ENDFOR query +]

#define QUERIES_COUNT [+ (count "query") +]
extern char const *queries[ QUERIES_COUNT ];
extern char const *queries_error_messages[ QUERIES_COUNT];
#endif /* [+ (. header-guard) +] */ [+
==  c  +][+ DO_NOT_EDIT +]
[+ GPL +]

#include "[+ (. header-file)  +]"

char const *queries[] = { [+
FOR query "," +]
  "[+ value +]"[+
ENDFOR query +]
};

char const *queries_error_messages[] = { [+
FOR query "," +]
  "[+ error_msg +]"[+
ENDFOR query +]
};

[+ESAC +]
