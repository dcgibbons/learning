/*
 * Code Kata Four - Data Munging
 * Part Three - Dry Fusion
 * http://codekata.pragprog.com/2007/01/kata_four_data_.html
 *
 * Chad Gibbons
 * dcgibbons@gmail.com
 * August 23, 2012
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

static void usage(const char* progname)
{
    fprintf(stderr, "Usage: %s [WEATHER|FOOTBALL] [data file]\n", progname);
    exit(EXIT_FAILURE);
}

static void parse_file(const char* file_name)
{
    bool start_of_data = false;
    FILE *fp = fopen(file_name, "r+");

    char buffer[BUFSIZ];
    while (fgets(buffer, sizeof(buffer), fp) != NULL)
    {
        if (!start_of_data)
        {

        }
        else
        {
        }
    }

    fclose(fp);
}

static void parse_weather_data(const char* file_name)
{
}

static void parse_football_data(const char* file_name)
{
}

int main(int argc, const char** argv)
{
    if (argc != 3)
    {
        usage(argv[0]);
    }

    const char* data_type = argv[1];
    const char* data_file = argv[2];

    if (strcmp(data_type, "WEATHER"))
    {
        parse_weather_data(data_file);
    }
    else if (strcmp(data_type, "FOOTBALL"))
    {
        parse_football_data(data_file);
    }
}

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
