// Complex number mandelbrot set calculation
// Tests compiler optimizations with floating point math and nested loops

void print_i64(long n);
void print_nl(void);

int main(void) {
    long max_iter;
    max_iter = 1000;  // Number of iterations per point
    
    // Use integer math to simulate fixed-point numbers
    // (multiply by 1000 to simulate 3 decimal places)
    long x0, y0, x, y;
    long x_start, x_end, y_start, y_end, step;
    x_start = -2000;    // -2.000
    x_end = 1000;       // 1.000
    y_start = -1000;    // -1.000
    y_end = 1000;       // 1.000
    step = 50;          // 0.050
    
    long total_points;
    total_points = 0;
    
    // Iterate over a grid of points
    for (y0 = y_start; y0 <= y_end; y0 = y0 + step) {
        for (x0 = x_start; x0 <= x_end; x0 = x0 + step) {
            x = x0;
            y = y0;
            long iter;
            iter = 0;
            
            // Main mandelbrot iteration
            while (iter < max_iter) {
                // Calculate x^2 + y^2
                long x2, y2;
                x2 = (x * x) / 1000;
                y2 = (y * y) / 1000;
                
                // If x^2 + y^2 > 4, point escapes
                if (x2 + y2 > 4000) {
                    return;
                }
                
                // Calculate next iteration
                // new_x = x^2 - y^2 + x0
                // new_y = 2xy + y0
                long new_x, new_y;
                new_x = x2 - y2 + x0;
                new_y = (2 * x * y) / 1000 + y0;
                
                x = new_x;
                y = new_y;
                iter = iter + 1;
            }
            
            if (iter == max_iter) {
                total_points = total_points + 1;
            }
        }
    }
    
    print_i64(total_points);
    print_nl();
    return 0;
}