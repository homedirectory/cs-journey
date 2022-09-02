// a program that removes comments from a C program

#include <stdio.h>

#define IN 1
#define OUT 0 // there are in-line comments

#define STR_OUT 0
#define STR_IN 1
#define CHR_IN 2
#define CHR_OUT STR_OUT
#define MAXLINE 1000  /* wow */

int cpline(char* s, int lim); /* this is 
                                 crazy
                                 */
int rmcomments(char* line, int* len);

int main() {
    int state = OUT;
    char line[MAXLINE];
    int len;

    while (cpline(line, MAXLINE) > 0) {
        int isc = rmcomments(line, &len);

        if (state == OUT && (isc == 0 || len > 1)) {
            // printf("%2d:%d:%d: %s", len, state, isc, line); // debug
            printf("%s", line);
        }

        if (state == IN && isc == 4) // end of multi-line comment
            state = OUT;
        else if (state == OUT && isc == 3) // start of multi-line comment
            state = IN;
    }
}

int cpline(char* s, int lim) {
    char c;
    int i;

    for (i = 0; i < lim - 1 && ((c = getchar()) != EOF) && c != '\n'; i++) {
        s[i] = c;
    }

    if (c == '\n' && i < lim - 1) {
        s[i++] = '\n';
    }

    s[i] = '\0';

    return i;
}

// returns:
//  0 - not a comment
//  1 - ... // ...
//  2 - ... // ... */ (special case to avoid confusion with 4)
//  3 - ... /* ...
//  4 - ... */
//  5 - ... /* ... */
int rmcomments(char *s, int* outlen) { 
    char c;
    int i, len, state;

    state = STR_OUT;
    // search for /
    for (i = 0; s[i] != '\0'; i++) {
        // handle state
        if (s[i] == '"') {
            if (state == CHR_IN) // '"'
                continue;
            else if (state == STR_OUT) {
                state = STR_IN;
                continue;
            }
            // state = STR_IN
            else if (i > 0 && s[i-1] != '\\') { // not " \" "
                state = STR_OUT;
                continue;
            }
        } else if (s[i] == '\'') {
            if (state == STR_IN) // " ' "
                continue;
            else if (state == CHR_IN && s[i-1] == '\\') { // '\''
                continue;
            } else // either opening or closing '
                state = state == CHR_IN ? CHR_OUT : CHR_IN;
        } else if (s[i] == '/' && state == STR_OUT) {
            break;
        }
    }

    int hasnl = 0;
    // calculate length and find newline
    for (len = i; s[len] != '\0'; len++) {
        if (s[len] == '\n')
            hasnl = 1;
    }
    *outlen = len;

    if (s[i] == '\0') // no comments found
        return 0;

    // 1. // ?
    if (s[i+1] == '/') {
        *outlen = i;
        // strip trailing whitespace and newline
        int j;
        for (j = len - 1; s[j] == '\n' || s[j] == ' ' || s[j] == '\t'; j--);

        if (hasnl == 1)
            s[(*outlen)++] = '\n';
        s[*outlen] = '\0';

        // // ... */ ?
        if (s[j] == '/' && s[j-1] == '*') {
            return 2;
        }
        return 1;
    }
    // 2. /* ?
    if (s[i+1] == '*') {
        *outlen = i;
        // strip trailing whitespace and newline
        int j;
        for (j = len - 1; s[j] == '\n' || s[j] == ' ' || s[j] == '\t'; j--);

        if (hasnl == 1)
            s[(*outlen)++] = '\n';
        s[*outlen] = '\0';

        // /* ... */ ?
        if (s[j] == '/' && s[j-1] == '*') {
            return 5;
        }
        // /* ...
        return 3;
    }
    // 3. ... */ ?
    // strip trailing whitespace and newline
    int j;
    for (j = len - 1; s[j] == '\n' || s[j] == ' ' || s[j] == '\t'; j--);

    if (s[j] == '/' && s[j-1] == '*') {
        *outlen = 0; // remove the whole string
        if (hasnl == 1)
            s[(*outlen)++] = '\n';
        s[*outlen] = '\0';
        return 4;
    }

    // not a comment
    return 0;
}
//
//*/
/* */
/*
 *
 */


