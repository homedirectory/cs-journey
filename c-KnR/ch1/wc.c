#include <stdio.h>

#define IN 1
#define OUT 0

int main() {
    int c, state, nc, nw, nl;

    nc = nw = nl = 0;
    state = OUT;

    while ((c = getchar()) != EOF) {
        ++nc;
        if (c == '\n')
            ++nl;
        if (c == '\n' || c == '\t' || c == ' ') {
            if (state == IN)
                ++nw;
            state = OUT;
        } else if (state == OUT)
            state = IN;
    }

    printf("c: %d, w: %d, l: %d\n", nc, nw, nl);
}
