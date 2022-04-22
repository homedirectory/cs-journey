#include <stdio.h>

#define MAXLINE 1000

void reverse(char* s);

int main() {
    char s[MAXLINE], c;

    int i;
    for(i = 0; i < MAXLINE - 1 && (c = getchar()) != EOF; i++) {
        if (c == '\n') {
            s[i++] = c;
            s[i] = '\0';
            reverse(s);
            printf("%s", s);
            // reset counter
            i = -1;
            continue;
        }
        s[i] = c;
    }

    if (i > 1) {
        s[i] = '\0';
        reverse(s);
        printf("%s", s);
    }
}

void reverse(char* s) {
    int end;

    for (end = 0; s[end] != '\0'; end++);
    --end;

    // skip newline
    if (s[end] == '\n')
        end--;

    char c;
    for (int i = 0; i < end; i++, end--) {
        c = s[i];
        s[i] = s[end];
        s[end] = c;
    }
}
