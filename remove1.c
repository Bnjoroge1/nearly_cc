

long read_i64(void);
void print_i64(long n);
void print_nl(void);

int main(void) {
    long n;
    n = 1000000000;  // Much larger number of iterations
    long prev;
    prev = 0L;
    long curr;
    curr = 1L;
    long next;
    long i;
    long sum;
    sum = 0L;  // Track running sum

    // Iteratively calculate Fibonacci numbers
    for (i = 2L; i <= n; i = i + 1L) {
        next = curr + prev;
        if (next > 1000000L) {  // Simple bounds check
            next = next - 1000000L;
        }
        prev = curr;
        curr = next;
        
        // Add extra computations
        sum = sum + curr;
        if (sum > 1000000L) {
            sum = sum - 1000000L;
        }
    }

    print_i64(sum);
    print_nl();
    print_i64(curr);
    print_nl();

    return 0;
}