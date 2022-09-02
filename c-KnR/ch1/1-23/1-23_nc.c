
#include <stdio.h>

#define IN 1
#define OUT 0 

#define STR_OUT 0
#define STR_IN 1
#define CHR_IN 2
#define CHR_OUT STR_OUT
#define MAXLINE 1000  

int cpline(char* s, int lim); 
int rmcomments(char* line, int* len);

int main() {
    int state = OUT;
    char line[MAXLINE];
    int len;

    while (cpline(line, MAXLINE) > 0) {
        int isc = rmcomments(line, &len);

        if (state == OUT && (isc == 0 || len > 1)) {
            
            printf("%s", line);
        }

        if (state == IN && isc == 4) 
            state = OUT;
        else if (state == OUT && isc == 3) 
            state = IN;
    }
}

int cpline(char* s, int lim) {
    char c;
    int i;

    for (i = 0; i < lim - 1 && ((c = getchar()) != EOF) && c != '\n'; i++) {
        s[i] = c;
    }

    if (c == '\n' && i < lim - 1) {
        s[i++] = '\n';
    }

    s[i] = '\0';

    return i;
}

int rmcomments(char *s, int* outlen) { 
    char c;
    int i, len, state;

    state = STR_OUT;
    
    for (i = 0; s[i] != '\0'; i++) {
        
        if (s[i] == '"') {
            if (state == CHR_IN) 
                continue;
            else if (state == STR_OUT) {
                state = STR_IN;
                continue;
            }
            
            else if (i > 0 && s[i-1] != '\\') { // not " \" "
                state = STR_OUT;
                continue;
            }
        } else if (s[i] == '\'') {
            if (state == STR_IN) 
                continue;
            else if (state == CHR_IN && s[i-1] == '\\') { // '\''
                continue;
            } else 
                state = state == CHR_IN ? CHR_OUT : CHR_IN;
        } else if (s[i] == '/' && state == STR_OUT) {
            break;
        }
    }

    int hasnl = 0;
    
    for (len = i; s[len] != '\0'; len++) {
        if (s[len] == '\n')
            hasnl = 1;
    }
    *outlen = len;

    if (s[i] == '\0') 
        return 0;

    
    if (s[i+1] == '/') {
        *outlen = i;
        
        int j;
        for (j = len - 1; s[j] == '\n' || s[j] == ' ' || s[j] == '\t'; j--);

        if (hasnl == 1)
            s[(*outlen)++] = '\n';
        s[*outlen] = '\0';

        
        if (s[j] == '/' && s[j-1] == '*') {
            return 2;
        }
        return 1;
    }
    
    if (s[i+1] == '*') {
        *outlen = i;
        
        int j;
        for (j = len - 1; s[j] == '\n' || s[j] == ' ' || s[j] == '\t'; j--);

        if (hasnl == 1)
            s[(*outlen)++] = '\n';
        s[*outlen] = '\0';

        
        if (s[j] == '/' && s[j-1] == '*') {
            return 5;
        }
        
        return 3;
    }
    
    
    int j;
    for (j = len - 1; s[j] == '\n' || s[j] == ' ' || s[j] == '\t'; j--);

    if (s[j] == '/' && s[j-1] == '*') {
        *outlen = 0; 
        if (hasnl == 1)
            s[(*outlen)++] = '\n';
        s[*outlen] = '\0';
        return 4;
    }

    
    return 0;
}


