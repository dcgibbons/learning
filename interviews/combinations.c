/*
 * combinations.c
 * print all combinations of k numbers from 1..n
 */

#include <stdio.h>
#include <stdlib.h>

void do_combination(const int data[], int tmp[], size_t start, size_t end, size_t index, size_t k)
{
    // base case - we're at the end!
    if (index == k) {
        for (int j = 0; j < k; j++) {
            printf("%d ", tmp[j]);
        }
        puts("");
    } else {
        for (int i = start; i < end && end-i >= k-index; i++) {
            tmp[index] = data[i];
            do_combination(data, tmp, i+1, end, index+1, k);
        }
    }
}

void combinations(const int data[], size_t n, size_t k)
{
    int tmp[n];
    do_combination(data, tmp, 0, n, 0, k);

}

int main(void)
{
    int data[] = {1, 2, 3, 4};
    combinations(data, sizeof(data)/sizeof(int), 2);
}

