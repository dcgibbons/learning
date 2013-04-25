/*
 * typesizes.c
 * Utility to display sizes of various C data types
 *
 * Chad Gibbons
 * November 2, 2010
 * */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

int main()
{
  printf("sizeof(char)      = %lu 0x%02lx\n", sizeof(char), sizeof(char));
  printf("sizeof(short)     = %lu 0x%02lx\n", sizeof(short), sizeof(short));
  printf("sizeof(int)       = %lu 0x%02lx\n", sizeof(int), sizeof(int));
  printf("sizeof(long)      = %lu 0x%02lx\n", sizeof(long), sizeof(long));
  printf("sizeof(long long) = %lu 0x%02lx\n", sizeof(long long), sizeof(long long));
  printf("sizeof(float)     = %lu 0x%02lx\n", sizeof(float), sizeof(float));
  printf("sizeof(double)    = %lu 0x%02lx\n", sizeof(double), sizeof(double));
  exit(EXIT_SUCCESS);
}
