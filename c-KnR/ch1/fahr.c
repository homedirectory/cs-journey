#include <stdio.h>

int main() {
    float fahr, celcius;
    int lower = 0;
    int upper = 300;
    int step = 20;

    printf("%s\t%2s\n", "F", "C");
    fahr = lower;
    while (fahr <= upper) {
        celcius = 5.0 / 9.0 * (fahr - 32);
        printf("%3.1f\t%4.1f\n", fahr, celcius);
        fahr += step;
    }

}
