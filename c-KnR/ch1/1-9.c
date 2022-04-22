#include <stdio.h>

int main() {
    int c, pc;
    pc = 0;

    while ((c = getchar()) != EOF) {
        if ((c == ' ') && (pc == ' '));
        else
            putchar(c);

        pc = c;
    }
}
