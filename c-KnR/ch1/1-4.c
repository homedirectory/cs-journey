#include <stdio.h>

int main() {
    float fahr, celcius;
    int lower = -60;
    int upper = 120;
    int step = 10;

    printf("%s\t%2s\n", "C", "F");
    celcius = lower;
    while (celcius <= upper) {
        fahr = (celcius / 5.0 * 9.0) + 32;
        printf("%4.1f\t%3.1f\n", celcius, fahr);
        celcius += step;
    }

}
