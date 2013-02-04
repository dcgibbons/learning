/*
 * Code Kata 2 - Karate Chop
 * http://codekata.pragprog.com/2007/01/code_kata_backg.html
 *
 * Chad Gibbons
 * dcgibbons@gmail.com
 * August 17, 2012
 */

#include <stdio.h>

/*
 * First implementation: a straight-forward non-recursive loop approach to a 
 * binary search. This type of design is highly efficient in a procedural 
 * language and is usually easy to read and understand.
 */
int chop(const int key, const int list[], const int length)
{
    int lower = 0;
    int upper = length - 1;

    while (upper >= lower)
    {
        int mid = (lower + upper) / 2;
        if (key < list[mid])
        {
            upper = mid - 1;
        }
        else if (key > list[mid])
        {
            lower = mid + 1;
        }
        else
        {
            return mid;
        }
    }

    return -1;
}

/*
 * Second implementation: a recursive implementation. Since I didn't want to 
 * change the interface, I have an outer function that prepares the arguments 
 * for the recursive implementation. In a real library (like the stdlib), I 
 * would just have the caller use the recursive call directly.
 */
int chop2_r(const int key, const int list[], const int lower, const int upper)
{
    if (upper < lower)
    {
        return -1;
    }

    const int mid = (lower + upper) / 2;
    if (key < list[mid])
    {
        return chop2_r(key, list, lower, mid - 1);
    }
    else if (key > list[mid])
    {
        return chop2_r(key, list, mid + 1, upper);
    }
    else
    {
        return mid;
    }
}

int chop2(const int key, const int list[], const int length)
{
    return chop2_r(key, list, 0, length - 1);
}
