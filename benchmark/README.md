Spiral code is from http://spiral.ece.cmu.edu/fft_scalar/.

FFTW3 results are based on the version of FFTW3 provided by the current distribution. For Ubuntu 16.04, that is 3.3.4.

CPU clock scaling was disabled for performance tests, although this shouldn't make a difference since we count clock cycles, not time.

Clock scaling was disabled with the following command:

```
cpufreq-set -g performance
```
