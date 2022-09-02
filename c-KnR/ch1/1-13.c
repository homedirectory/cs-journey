#include <stdio.h>

#define HIST_SIZE 15        // number of consequent length
#define HIST_ADD_SIZE 5     // number of additional length
#define IN 1
#define OUT 0

void add_wlen(int n, int* wlens, int* wcounts);

int main() {
    int c, state, wlen;
    int size = HIST_SIZE + HIST_ADD_SIZE;
    int wlens[HIST_ADD_SIZE];
    int wcounts[size];

    for (int i = 0; i < HIST_ADD_SIZE; wlens[i++] = 0); 
    for (int i = 0; i < size; wcounts[i++] = 0); 

    state = OUT;
    wlen = 0;
    while ((c = getchar()) != EOF) {
        if (c == '\n' || c == ' ' || c == '\t') {
            if (state == IN)
                state = OUT;
                // store word length
                if (wlen > 0) {
                    add_wlen(wlen, wlens, wcounts);
                    // reset word length
                    wlen = 0;
                }
        } else {
            if (state == OUT)
                state = IN;
            wlen++;
        }
    }

    // normalize counted values
    int maxlen = 0;
    for (int i = 0; i < size; i++)
        maxlen = wcounts[i] > maxlen ? wcounts[i] : maxlen;

    if (maxlen > 30) {
        for (int i = 0; i < size; i++) {
            wcounts[i] /= 2; 
        }
    }

    // print horizontal histogram
    printf("Word len  Frequency\n");
    for (int i = 0; i < size; i++) {
        if (wcounts[i] > 0) {
            int wordlen = i < HIST_SIZE ? (i + 1) : wlens[i - HIST_SIZE];
            printf("%3d       ", wordlen);
            for (int j = 0; j < wcounts[i]; j++)
                putchar('-');
            putchar('\n');
        }
    }

    // print vertical histogram
    printf("\nVertical histogram\n");
    for (int i = maxlen; i > 0; i--) {
        for (int j = 0; j < size; j++) {
            if (wcounts[j] == 0)
                continue;
            else if (wcounts[j] >= i)
                printf("%3c", '|');
            else
                printf("%3c", ' ');
            }
        putchar('\n');
    }
    for (int i = 0; i < size; i++) {
        if (wcounts[i] > 0) {
            int wordlen = i < HIST_SIZE ? (i + 1) : wlens[i - HIST_SIZE];
            printf("%3d", wordlen);
        }
    }
    putchar('\n');
}

void add_wlen(int n, int* wlens, int* wcounts) {
    if (n <= HIST_SIZE)
        wcounts[n-1]++;
    else {
        for (int i = 0; i < HIST_ADD_SIZE; i++) {
            int wcounts_ind = HIST_SIZE - 1 + i;
            if (wlens[i] == n) {
                wcounts[wcounts_ind]++;
                return;
            } else if (wlens[i] == 0) {
                wlens[i] = n;
                wcounts[wcounts_ind]++;
                return;
            }
        }
    }
}
