// A program that checks a C program for rudimentary syntax errors
// like unbalanced parentheses, brackets and braces.

// Without using structs !!!
// Thus very ugly

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

#define UINT_MAX 4294967295

int cpin(char* buff, int lnlim);
int syntax_errors(char* ncbuff, int* ln, int* chr);
int getlinen(char* buff, int n, char** line);
void printline(char* s);
void errmsg(char* buff, int n, int len);
int earlst_occur(int* occurs, int len, int size);
char* strfind(char* s, char chr);

int main() {
    char buff[MAXLINE * MAXLINE];

    cpin(buff, MAXLINE);

    char ncbuff[MAXLINE * MAXLINE];
    // strip comments
    rmcomments(buff, ncbuff);

    int lnc, chrc;
    int errors = syntax_errors(ncbuff, &lnc, &chrc);
    printf("Found %d syntax errors\n", errors);

    if (errors > 0) {
        printf("First syntax error at line %d char %d\n", lnc, chrc);
        char* lnptr;
        int lnlen = getlinen(ncbuff, lnc, &lnptr);
        printline(lnptr);
        // print a new line with a pointer (^) to error occurence
        char errstr[lnlen];
        errmsg(errstr, chrc, lnlen);
        printf("%s\n", errstr);
    }
}

// read input to buffer
int cpin(char* buff, int lnlim) {
    int len, buffp;
    char line[MAXLINE];

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
    int parlln, brktlln, brclln; // last line number opened at
    int parlchr, brktlchr, brclchr; // last character number opened at

    // first closed (but no matching opened, thus error)
    int parceln, brktceln, brcceln; 
    int parcechr, brktcechr, brccechr;

    int st_q; // state of quotes
    int i, errs, lnc, lnchrc;
    char c, cprev, errln[MAXLINE];

    st_q = Q_OUT;
    parlln = brktlln = brclln = -1;
    parlchr = brktlchr = brclchr = -1;
    parceln = brktceln = brcceln = -1;
    parcechr = brktcechr = brccechr = -1;
    par = brkt = brc = 0;
    errs = 0;
    lnc = 1;
    lnchrc = 0;

    for (i = 0; (c = ncbuff[i]) != '\0'; i++, lnchrc++, cprev = c) {
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
        else if (c == '(') {
            if (par == 0) {
                parlln = lnc;
                parlchr = lnchrc;
            }
            par++;
        }
        else if (c == ')')
            par--;
        // brackets
        else if (c == '[') {
            if (brkt == 0) {
                brktlln = lnc;
                brktlchr = lnchrc;
            }
            brkt++;
        }
        else if (c == ']')
            brkt--;
        // braces
        else if (c == '{') {
            if (brc == 0) {
                brclln = lnc;
                brclchr = lnchrc;
            }
            brc++;
        }
        else if (c == '}')
            brc--;

        // check for closed without a matching opened
        if (par < 0) {
            if (parceln == 0 && parcechr == 0) {
                parceln = lnc;
                parcechr = lnchrc;
            }
            par = 0;
            errs++;
        }
        if (brkt < 0) {
            if (brktceln == 0 && brktcechr == 0) {
                brktceln = lnc;
                brktcechr = lnchrc;
            }
            brkt = 0;
            errs++;
        }
        if (brc < 0) {
            if (brcceln == 0 && brccechr == 0) {
                brcceln = lnc;
                brccechr = lnchrc;
            }
            brc = 0;
            errs++;
        }

        // update line counter and line char counter
        if (c == '\n' && (i == 0 || cprev != '\\')) {
            lnc++;
            lnchrc = -1;
        }

    }

    errs += par + brkt + brc;

    // unclosed opening ( [ {
    // if it was closed then get rid of last opened
    if (par == 0)
        parlln = parlchr = -1;
    if (brkt == 0)
        brktlln = brktlchr = -1;
    if (brc == 0)
        brclln = brclchr = -1;

    if (errs > 0) {
        // find the earliest occurence (line char)
        int occurs[6][2] = {
            {parceln, parcechr},
            {parlln, parlchr},
            {brktceln, brktcechr},
            {brktlln, brktlchr},
            {brcceln, brccechr},
            {brclln, brclchr}
        };
        int earl = earlst_occur(occurs, 6, 2);
        *ln = occurs[earl][0];
        *chr = occurs[earl][1];
    }

    return errs;
}

// return line length and store pointer to start of line number n (indexed from 1)
int getlinen(char* buff, int n, char** line) {
    char c;
    int i, lnc, lnlen;

    lnc = 1;
    lnlen = 0;

    for (i = 0; (c = buff[i]) != '\0' && lnc <= n; i++, lnlen++) {
        if (c == '\n' && (i == 0 || buff[i-1] != '\\')) {
            if (lnc == n) {
                *line = buff + i - lnlen;
                return lnlen + 1;
            }

            lnc++;
            lnlen = -1;
        }
    }

    return -1;
}

void printline(char* s) {
    char* lnend = strfind(s, '\n');
    for(char* cptr = s; cptr <= lnend; cptr++)
        putchar(*cptr);
}

// find first occurence of a character in a string
char* strfind(char* s, char chr) {
    char c;
    int i;

    for (i = 0; (c = s[i]) != chr && c != '\0'; i++);

    if (c == '\0')
        return NULL;

    return s + i;
}

// fill buff with ' ' and '^' at n
void errmsg(char* buff, int n, int len) {
    int i;

    for (i = 0; i < len; i++) {
        buff[i] = ' ';
    }

    buff[n] = '^';
    buff[len] = '\0';
}

int earlst_occur(int* occurs, int len, int size) {
    unsigned int minln;
    unsigned int minchr;
    int ind;

    minln = UINT_MAX;
    minchr = UINT_MAX;
    ind = -1;

    int ln, chr;
    for (int i = 0; i < len; i++) {
        ln = occurs[i*size];
        chr = occurs[(i*size)+1];

        if (ln >= 0 && ln < minln) {
            minln = ln;
            minchr = chr;
            ind = i;
        }
        else if (ln == minln && chr < minchr) {
            minchr = chr;
            ind = i;
        }
    }

    return ind;
}
