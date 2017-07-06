#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include <fftw3.h>

#include "hspiral/hspiral_fft.h"
#include "spiral/spiral_fft.h"

long double get_real_time(void)
{
    struct timespec ts;

    if (clock_gettime(CLOCK_MONOTONIC_RAW, &ts) == 0)
        return (long double) ts.tv_sec + (long double) ts.tv_nsec / 1e9;
    else
        return -1;
}

typedef unsigned long long ticks;

static __inline__ ticks getticks(void)
{
     unsigned a, d,t;
     __asm__ __volatile__("mfence":::"memory");
     __asm__ __volatile__ ("rdtsc" : "=a" (a), "=d" (d));
     t = ((ticks)a) | (((ticks)d) << 32);
     __asm__ __volatile__("mfence":::"memory");
     return t;
}

static __inline__ ticks elapsed(ticks t1, ticks t0)
{
     return t1 - t0;
}

#define TIMEIT(F) \
{ \
    F; \
    t1 = getticks(); \
    F; \
    t2 = getticks(); \
    t = elapsed(t2, t1); \
}

#define NREPEATS 100

#define REPEAT for(int i = 0; i < NREPEATS; ++i)

#define MAX_N 1024

fftw_plan p2;
fftw_plan p4;
fftw_plan p8;
fftw_plan p16;
fftw_plan p32;
fftw_plan p64;
fftw_plan p128;
fftw_plan p256;
fftw_plan p512;
fftw_plan p1024;

int main(int argc, char** argv)
{
    double *in = (double*) malloc(2*MAX_N*sizeof(double));
    double *out = (double*) malloc(2*MAX_N*sizeof(double));
    ticks t1, t2;
    ticks t;
    long is;
    long os;

    p2 = fftw_plan_dft_1d(2, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p4 = fftw_plan_dft_1d(4, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p8 = fftw_plan_dft_1d(8, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p16 = fftw_plan_dft_1d(16, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p32 = fftw_plan_dft_1d(32, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p64 = fftw_plan_dft_1d(64, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p128 = fftw_plan_dft_1d(128, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p256 = fftw_plan_dft_1d(256, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p512 = fftw_plan_dft_1d(512, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);
    p1024 = fftw_plan_dft_1d(1024, (double (*)[2]) in, (double (*)[2]) out, FFTW_FORWARD, FFTW_MEASURE | FFTW_NO_SIMD);

    for (int i = 0; i < 2*MAX_N; ++i)
        in[i] = 1.0;

    printf("platform,N,cycles\n");

    /* Size 2 */
    REPEAT {
      TIMEIT(hspiral_dft_2(in, out));
      printf("hspiral,%d,%llu\n", 2, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_2(in, out));
      printf("spiral,%d,%llu\n", 2, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p2));
      printf("fftw3,%d,%llu\n", 2, t);
    }

    /* Size 4 */
    REPEAT {
      TIMEIT(hspiral_dft_4(in, out));
      printf("hspiral,%d,%llu\n", 4, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_4(in, out));
      printf("spiral,%d,%llu\n", 4, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p4));
      printf("fftw3,%d,%llu\n", 4, t);
    }

    /* Size 8 */
    REPEAT {
      TIMEIT(hspiral_dft_8(in, out));
      printf("hspiral,%d,%llu\n", 8, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_8(in, out));
      printf("spiral,%d,%llu\n", 8, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p8));
      printf("fftw3,%d,%llu\n", 8, t);
    }

    /* Size 16 */
    REPEAT {
      TIMEIT(hspiral_dft_16(in, out));
      printf("hspiral,%d,%llu\n", 16, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_16(in, out));
      printf("spiral,%d,%llu\n", 16, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p16));
      printf("fftw3,%d,%llu\n", 16, t);
    }

    /* Size 32 */
    REPEAT {
      TIMEIT(hspiral_dft_32(in, out));
      printf("hspiral,%d,%llu\n", 32, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_32(in, out));
      printf("spiral,%d,%llu\n", 32, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p32));
      printf("fftw3,%d,%llu\n", 32, t);
    }

    /* Size 64 */
    REPEAT {
      TIMEIT(hspiral_dft_64(in, out));
      printf("hspiral,%d,%llu\n", 64, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_64(in, out));
      printf("spiral,%d,%llu\n", 64, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p64));
      printf("fftw3,%d,%llu\n", 64, t);
    }

    /* Size 128 */
    REPEAT {
      TIMEIT(hspiral_dft_128(in, out));
      printf("hspiral,%d,%llu\n", 128, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_128(in, out));
      printf("spiral,%d,%llu\n", 128, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p128));
      printf("fftw3,%d,%llu\n", 128, t);
    }

    /* Size 256 */
    REPEAT {
      TIMEIT(hspiral_dft_256(in, out));
      printf("hspiral,%d,%llu\n", 256, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_256(in, out));
      printf("spiral,%d,%llu\n", 256, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p256));
      printf("fftw3,%d,%llu\n", 256, t);
    }

    /* Size 512 */
    REPEAT {
      TIMEIT(hspiral_dft_512(in, out));
      printf("hspiral,%d,%llu\n", 512, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_512(in, out));
      printf("spiral,%d,%llu\n", 512, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p512));
      printf("fftw3,%d,%llu\n", 512, t);
    }

    /* Size 1024 */
    REPEAT {
      TIMEIT(hspiral_dft_1024(in, out));
      printf("hspiral,%d,%llu\n", 1024, t);
    }

    REPEAT {
      TIMEIT(spiral_fftfwd_double_1024(in, out));
      printf("spiral,%d,%llu\n", 1024, t);
    }

    REPEAT {
      TIMEIT(fftw_execute(p1024));
      printf("fftw3,%d,%llu\n", 1024, t);
    }

    fftw_destroy_plan(p2);
    fftw_destroy_plan(p4);
    fftw_destroy_plan(p8);
    fftw_destroy_plan(p16);
    fftw_destroy_plan(p32);
    fftw_destroy_plan(p64);
    fftw_destroy_plan(p128);
    fftw_destroy_plan(p256);
    fftw_destroy_plan(p512);
    fftw_destroy_plan(p1024);

    return 0;
}
