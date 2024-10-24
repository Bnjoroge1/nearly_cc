int main() {
    // Valid integer casts
    int i;
    i = 42;
    double d;
    d = (double)i; // Cast from int to double
    char c;
    c = (char)i; // Cast from int to char

    // Valid pointer casts
    int *p = &i;
    void *vp = (void *)p; // Cast from int* to void*
    int *p2 = (int *)vp; // Cast from void* to int*

    // Invalid casts (uncomment to test)
    // int *invalid_cast1 = (int *)d; // Invalid cast from double to int*
    // char *invalid_cast2 = (char *)d; // Invalid cast from double to char*

    
    return 0;
}