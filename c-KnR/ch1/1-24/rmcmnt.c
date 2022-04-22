// a program that removes comments from a C program

#include <stdio.h>
#include <string.h>
#include "rmcmnt.h"

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
int rmcomments_ln(char *s, int* outlen) { 
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

char* rmcomments(char* src, char* dst) {
    char c, line[MAXLINE];
    int state, len, i, l, dstp;

    state = OUT;
    len = dstp = 0;

    for (i = 0, l = 0; (c = src[i]) != '\0'; i++, l++) {
        line[l] = c;
        if (c  == '\n') {
            line[++l] = '\0';

            int isc = rmcomments_ln(line, &len);

            if (state == OUT && (isc == 0 || len > 1)) {
                strcpy(dst + dstp, line);
                dstp += len;
            }

            if (state == IN && isc == 4) // end of multi-line comment
                state = OUT;
            else if (state == OUT && isc == 3) // start of multi-line comment
                state = IN;

            l = -1; // reset line index
        }
    }
    dst[dstp] = '\0';

    return dst;
}
