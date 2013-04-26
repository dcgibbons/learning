/*
 * pos370-wk4-dcg.c
 * Week Four Program for David C. Gibbons
 * POS/370 - Programming Concepts
 * November 6, 2005
 * Version 2.00
 */


/* standard library include files */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>


#define CURRENCY_NAME                   "Euro"
#define CURRENCY_EXCHANGE_RATE          0.8370


/* forward reference for internal functions */
void displayGreeting();
double getCurrencyAmount(char* currencyName);
void convertCurrency(char* currencyName, 
                     double exchangeRate, 
                     double currencyAmount);


/* main program entry point */
int main() 
{
    double currencyAmount;

    displayGreeting();

    currencyAmount = getCurrencyAmount(CURRENCY_NAME);
    if (currencyAmount > 0.0) 
    {
        convertCurrency(CURRENCY_NAME, CURRENCY_EXCHANGE_RATE, currencyAmount);
    }

    return EXIT_SUCCESS;
}


/* displays a greeting message about this program */
void displayGreeting()
{
    puts("Currency Conversion\n");
}


/* retrieves the amount of currency to convert to dollars; if an invalid 
   value was input by the user then 0.0 is returned */
double getCurrencyAmount(char* currencyName)
{
    double currencyAmount = 0.0;

    printf("Enter amount of %s currency to convert to U.S. $: ", currencyName);
    fflush(stdout); // force output to be displayed before newline is sent

    // retrieve the currency amount from the user, but check for input errors
    // too; scanf has limitations on input so this error check isn't flawless
    scanf("%lf", &currencyAmount);
    return currencyAmount;
}


/* converts the specified currency to dollars using the given exchange rate */
void convertCurrency(char* currencyName,
                     double exchangeRate,
                     double currencyAmount)
{
    double dollars = (1.0 / exchangeRate) * currencyAmount;
    printf("%.4lf %s = %.4lf U.S. $\n",
        currencyAmount, currencyName, dollars);
}

