#define IN 1
#define OUT 0 // there are in-line comments

#define STR_OUT 0
#define STR_IN 1
#define CHR_IN 2
#define CHR_OUT STR_OUT
#define MAXLINE 1000  /* wow */

int cpline(char* s, int lim);
int rmcomments(char* line, int* len);
