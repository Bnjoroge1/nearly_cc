#include <stdio.h>

void print_i64(long n) {
    printf("%ld", n);
}

void print_nl(void) {
    printf("\n");
}

long read_i64(void) {
    long val;
    scanf("%ld", &val);
    return val;
}