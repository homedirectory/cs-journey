#include <stdio.h>

int main() {
    int c;
    int n1 = 0;

    while ((c = getchar()) != EOF) {
        if (c == '\n')
            ++n1;
    }
    printf("%d\n", n1);
}
