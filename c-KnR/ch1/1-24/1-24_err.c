// a program that checks a C program for rudimentary syntax errors
// like unbalanced parentheses, brackets and braces

#include <stdio.h>
#include <string.h>
#include "rmcmnt.h"

#define MAXLINE 1000

#define OPEN 1
#define CLSD 0

#define Q_OUT 0
#define Q_IN 1

#undef STR_IN
#define STR_IN 2

#define min(A,B) (A < B ? A : B)

int cpin(char* buff, int lnlim);
int syntax_errors(char* ncbuff, int* ln, int* chr);

int main() {
    char buff[MAXLINE * MAXLINE];

    cpin(buff, MAXLINE);

    char ncbuff[MAXLINE * MAXLINE]; // no comments buff

    rmcomments(buff, ncbuff);
    //printf("%s", ncbuff);

    int lnc, chrc;
    int errors = syntax_errors(ncbuff, &lnc, &chrc);
    printf("Found %d syntax errors\n", errors);
    if (errors > 0)
        printf("First syntax error at line %d char %d\n", lnc, chrc);
}

// read input to buffer
int cpin(char* buff, int lnlim) {{
    int len, buffp;
    char line[MAXLINE]];

    buffp = 0;

    while ((len = cpline(line, lnlim)) > 0) {
        strcpy(buff + buffp, line);
        buffp += len;
    }
    buff[buffp] = '\0';

    return buffp;
}

// int* ln - first error line number (indexed from 0)
// int* chr - first error line character number (indexed from 0)
// return - the total number of errors
int syntax_errors(char* ncbuff, int* ln, int* chr) {
    int par, brkt, brc; // counter of parentheses, brackets and braces
    int st_q; // state of quotes
    int i, errs, first, lnc;
    char c, cprev;

    st_q = Q_OUT;
    par = brkt = brc = 0;
    errs = 0;
    first = 0;
    lnc = 1;

    for (i = 0; (c = ncbuff[i]) != '\0'; i++, cprev = c) {
        // handle double quotes
        if (c == '"') {
            // '"'
            if (st_q == Q_IN);
            else if (st_q == STR_IN && i > 0 && cprev != '\\') {
                st_q = Q_OUT;
            }
            // st_q = Q_OUT
            else if (st_q == Q_OUT) {
                st_q = STR_IN;
            }
        }
        // handle single quotes
        else if (c == '\'') {
            if (st_q == STR_IN);
            else if (st_q == Q_OUT)
                st_q = Q_IN;
            else if (st_q == Q_IN && i > 0 && cprev == '\\' 
                    && ncbuff[i+1] == '\'') // '\'' ? ('\\')
                continue;
            else
                st_q = Q_OUT;
        }

        // ignore everything else inside quotes
        if (st_q != Q_OUT)
            continue;

        // parentheses
        else if (c == '(')
            par++;
        else if (c == ')')
            par--;
        // brackets
        else if (c == '[')
            brkt++;
        else if (c == ']')
            brkt--;
        // braces
        else if (c == '{')
            brc++;
        else if (c == '}')
            brc--;

        // check for errors
        if (par < 0) {
            if (first == 0) {
                first = 1;
                *ln = lnc;
                *chr = i;
            }
            par = 0;
            errs++;
        }
        if (brkt < 0) {
            if (first == 0) {
                first = 1;
                *ln = lnc;
                *chr = i;
            }
            brkt = 0;
            errs++;
        }
        if (brc < 0) {
            if (first == 0) {
                first = 1;
                *ln = lnc;
                *chr = i;
            }
            brc = 0;
            errs++;
        }

        // update line counter
        if (c == '\n' && (i == 0 || cprev != '\\'))
            lnc++;

    }

    errs += par + brkt + brc;

    return errs;
}


