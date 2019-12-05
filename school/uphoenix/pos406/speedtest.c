/*
 * speedtest.c
 * Floating-point benchmarking tool
 * POS/406 - Computer Programming I
 * David C. Gibbons, dcgibbons@email.uophx.edu
 * November 20, 2005
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define PI 3.141592653589793

int main(int argc, char** argv)
{
    int n = 10;
    if (argc >= 2) {
        n = atoi(argv[1]);
    }

    clock_t start = clock();

    double result = PI;
    for (int i = 0; i < n; i++) {
        result += pow(i, PI);
    }

    long end = clock();

    double elapsed = (double)(end - start) / (double)CLOCKS_PER_SEC;
    printf("Result is %lf and took %.4f sec\n", result, elapsed);
    return EXIT_SUCCESS;
}
