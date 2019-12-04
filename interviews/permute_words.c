/*
 * permute_words.c
 * display all of the permutations of an array of words
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swap(const char* words[], size_t i, size_t j)
{
    const char* tmp = words[i];
    words[i] = words[j];
    words[j] = tmp;
}

void do_permute(const char* words[], size_t start, size_t end)
{
    // base case - we're at the end!
    if (start == end) {
        for (int i = 0; i < end; i++) {
            printf("%s ", words[i]);
        }
        puts("");
    } else {
        for (int i = start; i < end; i++) {
            swap(words, i, start);
            do_permute(words, start+1, end);
            swap(words, start, i);
        }
    }
}

void permute(const char* words[], const size_t n)
{
    do_permute(words, 0, n);
}

int main(void)
{
    const char* words[] = {"hello", "kitty", "cat", "pizza"};
    permute(words, sizeof(words)/sizeof(char*));
    return EXIT_SUCCESS;
}

