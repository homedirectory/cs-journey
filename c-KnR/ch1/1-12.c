#include <stdio.h>

#define OUT 0
#define IN 1

int main() {
    int c, state;

    state = OUT;

    while ((c = getchar()) != EOF) {
        if (c == '\n' || c == '\t' || c == ' ') {
            if (state == IN)
                putchar('\n');
            state = OUT;
        } else {
            putchar(c);
            if (state == OUT) 
                state = IN;
        }
    }
}
