/*
 * permutations.c
 * print all permutations of a string
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swap(char* s, int i, int j)
{
    char ch = s[i];
    s[i] = s[j];
    s[j] = ch;
}

void do_permute(char* s, int start, int end)
{
    // base case - we're at the end!
    if (start == end) {
        puts(s);
    } else {
        for (int i = start; i < end; i++) {
            swap(s, i, start);
            do_permute(s, start+1, end);
            swap(s, start, i);
        }
    }
}

void permute(char* s)
{
    const size_t n = strlen(s);
    char* t = malloc(n);
    if (!t) abort();
    strcpy(t, s);
    do_permute(t, 0, n);
    free(t);
}

int main(int argc, char* argv[])
{
    permute("abc");
    return EXIT_SUCCESS;
}
