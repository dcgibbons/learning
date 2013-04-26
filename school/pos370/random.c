#include <stdio.h>
#include <stdlib.h>

int main()
{
    int i = 0;
    for (i = 0; i < 10; i++) {
        printf("rand[%d]=%d\n", i, rand());
    }
}

