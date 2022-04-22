#include <stdio.h>

int main() {
    int c;

    while((c = getchar()) != EOF) {
        //putchar(c);
        if (c == 0x0a) {
            printf("%x\n", c);
        } else {
            printf("%x ", c);
        }
    }
}
