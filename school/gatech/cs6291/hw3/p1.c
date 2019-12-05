#include <stdio.h>

int main(void)
{
    int a = 1;
    int b = 1;
    int c = 1;
    int d = 0;
    int e = 0;

    if (b > d) {

        a = e;
        printf("before loop: a=%d b=%d c=%d d=%d e=%d\n", a, b, c, d, e);
        do {
                c = a + d;
                a = c;
                d = b;
                printf("loop: a=%d b=%d c=%d d=%d\n", a, b, c, d);
        } while (d >= b);

    } else {

        d = b;
        if (b != d) {
            b = 2;
            d = b + 4;
            b = a;
        }

    }

    return 0;
}

