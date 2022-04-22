#include <stdio.h>

int main() {
    int c;

    while ((c = getchar()) != EOF)
        printf("%x ", c);
    putchar('\n');
}
