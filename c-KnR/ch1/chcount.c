#include <stdio.h>

int main() {
    double n;
    int c;

    for (n = 0; (c = getchar()) != EOF; ++n);
    printf("%.0f\n", n);
}
