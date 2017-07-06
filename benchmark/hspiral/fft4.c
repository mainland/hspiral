#include <complex.h>
#include <stdint.h>
void hspiral_dft_4(const double X[restrict static 8],
                   double Y[restrict static 8])
{
    double t;
    double t1;
    double t2;
    double t3;
    double t4;
    double t5;
    double t6;
    double t7;
    double t8;
    double t9;
    double t10;
    double t11;
    double t12;
    double t13;
    double t14;
    double t15;
    double t16;
    double t17;
    double t18;
    double t19;
    double t20;
    double t21;
    double t22;
    double t23;
    
    t = X[0];
    t1 = X[4];
    t2 = t + t1;
    t3 = X[1];
    t4 = X[5];
    t5 = t3 + t4;
    t6 = t - t1;
    t7 = t3 - t4;
    t8 = X[2];
    t9 = X[6];
    t10 = t8 + t9;
    t11 = X[3];
    t12 = X[7];
    t13 = t11 + t12;
    t14 = t8 - t9;
    t15 = t11 - t12;
    t16 = t2 + t10;
    Y[0] = t16;
    t17 = t5 + t13;
    Y[1] = t17;
    t18 = t2 - t10;
    Y[4] = t18;
    t19 = t5 - t13;
    Y[5] = t19;
    t20 = t6 + t15;
    Y[2] = t20;
    t21 = t7 - t14;
    Y[3] = t21;
    t22 = t6 - t15;
    Y[6] = t22;
    t23 = t7 + t14;
    Y[7] = t23;
}
