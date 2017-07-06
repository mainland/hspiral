#include <complex.h>
#include <stdint.h>
void hspiral_dft_2(const double X[restrict static 4],
                   double Y[restrict static 4])
{
    double t;
    double t1;
    double t2;
    double t3;
    double t4;
    double t5;
    double t6;
    double t7;
    
    t = X[0];
    t1 = X[2];
    t2 = t + t1;
    Y[0] = t2;
    t3 = X[1];
    t4 = X[3];
    t5 = t3 + t4;
    Y[1] = t5;
    t6 = t - t1;
    Y[2] = t6;
    t7 = t3 - t4;
    Y[3] = t7;
}
