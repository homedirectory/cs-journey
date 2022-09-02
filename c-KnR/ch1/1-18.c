#include <stdio.h>

#define MAXLINE 1000

int cpline_strp(char buff[], int lim);

int main() {
    char line[MAXLINE];
    int c, len;

    while ((len = cpline_strp(line, MAXLINE)) > 0) {
        // remove entirely blank lines
        if (len > 1)
            printf("%3d: %s", len, line);
    }
}

int cpline_strp(char buff[], int lim) {
    int c, i;

    for (i = 0; i < lim - 2 && (c = getchar()) != EOF && c != '\n'; i++)
        buff[i] = c;

    // remove trailing blanks and tabs
    for (--i; i > 0 && (buff[i] == ' ' || buff[i] == '\t'); i--);
    ++i;

    if (c == '\n')
        buff[i++] = '\n';

    buff[i] = '\0';

    return i;
}
