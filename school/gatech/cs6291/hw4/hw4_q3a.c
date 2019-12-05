#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    int vals[] = {10, 7};
    int b = vals[0];
    int c = vals[1];
    
    int a = b + c;
    int d = a;
    a += 3;
    printf("a=%d d=%d\n", a, d);
    d += 5;
    printf("d=%d", d);

    return d;
}

