/*
 * Code Kata Four - Data Munging
 * Part Two - Soccer League Table
 * http://codekata.pragprog.com/2007/01/kata_four_data_.html
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

#define DATA_FILE "football.dat"

int main(void)
{
    bool start_of_data = false;
    const char* smallest_team = NULL;
    int smallest_point_spread = INT_MAX;

    FILE *fp = fopen(DATA_FILE, "r+");
    char buffer[BUFSIZ];
    while (fgets(buffer, sizeof(buffer), fp) != NULL)
    {
        if (!start_of_data)
        {
            if (strncmp(buffer, "       Team", 11) == 0) start_of_data = true;
            continue;
        }
        else
        {
            const char* num = strtok(buffer, ".");
            if (strtol(num, NULL, 10) == 0 && errno == EINVAL) continue;

            const char* team = strtok(NULL, " ");
            const char* p = strtok(NULL, " ");
            const char* w = strtok(NULL, " ");
            const char* l = strtok(NULL, " ");
            const char* d = strtok(NULL, " ");
            const char* f = strtok(NULL, "-");
            const char* a = strtok(NULL, " ");
            const char* pts = strtok(NULL, " ");

            int spread = abs(atoi(f) - atoi(a));
            if (spread < smallest_point_spread)
            {
                smallest_point_spread = spread;
                smallest_team = strdup(team);
            }
        }
    }
    fclose(fp);

    fprintf(stderr, "Team with smallest point spread: %s with %d points\n", smallest_team, smallest_point_spread);

    return 0;
}
