/*
 * rain_on_landscape.c
 *
 * == PROBLEM STATEMENT ==
 * Imgine an input like this, that represents the height of a landscape.
 *
 *    x    x
 *  xxx x xxx
 * xxxxxxxxxxx
 *
 * Calculate what the landscape would look like if rain fell and filled any
 * valley with water. In this example, the output would be:
 *
 *    xwwwwx
 *  xxxwxwxxx
 * xxxxxxxxxxx
 *
 * == SOLUTION DISCUSSION ==
 * We first need to look at how this input is represented. Two options would
 * be a 2D array of characters that represents a map, and another option would
 * be an 1D array that represents the height of the landscape at a given
 * location. For this problem, working with a 1D array of numeric heights is
 * a simpler and effective approach.
 *
 * While this example input shows only one valley, it is entirely possible
 * that a given input will have multiple valleys. As a result, the general
 * algorithm will be:
 *
 * 1. Walk the array of heights to build a list of all valleys.
 *   a. initialize left = INT_MIN
 *   b. initialize right = INT_MIN
 *   c. iterate over the landscape from [0..n]
 *   d. if current valley left height is less than current height, mark start of valley
 *   e. if current valley right height is less than current height, mark end of valley
 *       
 *
 * == QUESTIONS ==
 *
 * 1. Can there be multiple valleys in the landscape?
 * 2. Can there be valleys below sea level?
 *
 * == EDGE CASES ==
 *
 * 1. Central peak gives two valleys, but the central peak can be covered with water.
 *
 * x   x
 * x x x
 * xxxxx
 *
 * 2. No valleys
 *
 * 3. Consecutive valleys with higher left edge
 *
 * x
 * x x  
 * x x x
 * x x x
 * xxxxx
 * expected result:
 * 43322
 */

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))

void rain(int* landscape, const size_t n)
{
    int left = INT_MIN;
    int right = INT_MIN;
    int left_height = INT_MIN;
    int right_height = INT_MIN;
    int previous_left = INT_MIN;
    int previous_left_height = INT_MIN;
 
    // step 1 - find our first valley by looking for our first dip below the
    // previous point
    int i;
    for (i = 0; i < n; i++) {
        if (left_height < landscape[i]) {
            left_height = landscape[i];
            left = i;
        } else {
            right_height = landscape[i];
            right = i;
            break;
        }
    }
    // edge case: did we find a valley?
    if (i == n) {
        puts("no valleys found in landscape");
        return;
    }

    // step 2 - start looking for the right edge of valleys
    for (; i < n; i++) {
       if (right_height < landscape[i]) {
           right_height = landscape[i];
           right = i;
       } else {
           // end of valley! mark a new valley
           /*
           printf("end of valley? i=%d left=%d(%d) right=%d(%d)\n", i,
                   left, left_height,
                   right, right_height);
           */

           // check if the valley actually extends past the current left into the previous left
           if (previous_left_height > left_height) {
               left_height = previous_left_height;
               left = previous_left;
               }

           if (left_height > 0 && right_height > 0) {
               for (int j = left+1; j <= i-1; j++) {
                   landscape[j] = MIN(left_height, right_height);
               }
           }

           // stash our previous left, in case we need to return to it
           if (left_height > 0) {
                previous_left = left;
                previous_left_height = left_height;
           }

           // reset the current valley's left to the current right to start a new possible valley
           left_height = right_height;
           left = right;

           right_height = landscape[i];
           right = i;
        }
    }

    if (left_height > 0 && right_height > 0) {
        for (int i = left+1; i <= right-1; i++) {
           if (i != left && i != right) {
               landscape[i] = MIN(left_height, right_height);
           }
       }
   }
}

void print(int* landscape, const size_t n)
{
    for (int i = 0; i < n; i++) {
        printf("%d ", i);
    }
    printf("\n");
    for (int i = 0; i < n; i++) {
        printf("--");
    }
    printf("\n");
    for (int i = 0; i < n; i++) {
        printf("%d ", landscape[i]);
    }
    printf("\n");
}

void test(int* landscape, const size_t n)
{
    puts("before rain:");
    print(landscape, n);
    rain(landscape, n);
    puts("\nafter rain:");
    print(landscape, n);
}

void test1()
{
    /*
     * xxx
     */
    int landscape[] = {0, 0, 0};
    puts("\n=====\ntest1");
    test(landscape, sizeof(landscape)/sizeof(int));
}

void test2()
{
    /*
     *  x
     * xxx
     */
    int landscape[] = {0, 1, 0};
    puts("\n=====\ntest2");
    test(landscape, sizeof(landscape)/sizeof(int));
}

void test3()
{
    /*
     *   x
     *   xx
     * xxxxx
     */
    int landscape[] = {0, 0, 2, 1, 0};
    puts("\n=====\ntest3");
    test(landscape, sizeof(landscape)/sizeof(int));
}

void test4()
{
    /*
     *    x    x
     *  xxx x xxx
     * xxxxxxxxxxx
     * expected output:
     * 01122222210
     */
    int landscape1[] = {0, 1, 1, 2, 0, 1, 0, 1, 2, 1, 0};
    puts("\n=====\ntest4");
    test(landscape1, sizeof(landscape1)/sizeof(int));
}

void test5()
{
    /*
     * x   x  
     * x x x xx
     * xxxxxxxxx
     * expected output:
     * 222221110
     */
    int landscape2[] = {2, 0, 1, 0, 2, 0, 1, 1, 0};
    puts("\n=====\ntest5");
    test(landscape2, sizeof(landscape2)/sizeof(int));
}

void test6()
{

    int landscape[] = {2, 2};
    puts("\n=====\ntest6");
    test(landscape, sizeof(landscape)/sizeof(int));
}

void test7()
{
    /*
     * x
     * x x  
     * x x x
     * x x x
     * xxxxx
     * expected result:
     * 43322
     */
    int landscape3[] = {4, 0, 3, 0, 2};
    puts("\n=====\ntest7");
    test(landscape3, sizeof(landscape3)/sizeof(int));
}

int main(int argc, char* argv[])
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
}

