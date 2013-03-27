[+ autogen5 template h +]
/*
[+ (gpl "GNU Thales" " * " ) +]
*/
/*
[+ (dne " * ") +]
*/
[+ (make-header-guard "") +]

[+ FOR query "" +]
#define WRITER_[+ (string-upcase (get "action")) +] "{[+ (get "action") +]}(%s)[+ (get "format") +]"
[+ ENDFOR query +]
#endif /* [+ (. header-guard) +] */
