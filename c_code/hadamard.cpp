#include <cmath>
#include <cstddef>

extern "C" {

// standard recursive WHT algorithm
static void fwht_recursive(double* x, std::size_t n) {
    if (n <= 1)
        return;

    std::size_t half = n / 2;
    fwht_recursive(x, half);
    fwht_recursive(x + half, half);

    for (std::size_t i = 0; i < half; ++i) {
        double a = x[i];
        double b = x[i + half];
        x[i]        = a + b;
        x[i + half] = a - b;
    }
}

void fwht(double* x, unsigned long n) {
    //n must be a power of 2
    if (n == 0 || (n & (n - 1)) != 0)
        return;

    fwht_recursive(x, n);

    // scale the WHT output to match the Haskell implementations
    double scale = 1.0 / std::sqrt((double)n);
    for (unsigned long i = 0; i < n; ++i)
        x[i] *= scale;
}

}
