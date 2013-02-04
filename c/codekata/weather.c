/*
 * Code Kata Four - Data Munging
 * http://codekata.pragprog.com/2007/01/kata_four_data_.html
 *
 * In weather.dat you’ll find daily weather data for Morristown, NJ 
 * for June 2002. Download this text file, then write a program to 
 * output the day number (column one) with the smallest temperature 
 * spread (the maximum temperature is the second column, the minimum 
 * the third column).
 * 
 * Chad Gibbons
 * dcgibbons@gmail.com
 * August 21, 2012
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#define DATA_FILE       "weather.dat"

int main(void)
{
    bool start_of_data = false;
    int smallest_day = -1;
    int smallest_temp_spread = INT_MAX;

    FILE *fp = fopen(DATA_FILE, "r+");
    char buffer[BUFSIZ];
    while (fgets(buffer, sizeof(buffer), fp) != NULL)
    {
        if (!start_of_data)
        {
            if (strncmp(buffer, "  Dy", 4) == 0) start_of_data = true;
            continue;
        }
        else
        {
            const char *day = strtok(buffer, " ");
            const char *mxt = strtok(NULL, " ");
            const char* mnt = strtok(NULL, " ");

            int n = (int)strtol(day, (char **)NULL, 10);
            if (n == 0 && errno == EINVAL) continue;

            int spread = atoi(mxt) - atoi(mnt);
            if (spread < smallest_temp_spread)
            {
                smallest_day = n;
                smallest_temp_spread = spread;
            }
        }
    }
    fclose(fp);

    fprintf(stdout, "Day #%d had the smallest temperature spread of %d degrees\n",
            smallest_day, smallest_temp_spread);

    return 0;
}
