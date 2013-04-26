/*
 * pos370-wk3-dcg.c
 * Week Three Program for David C. Gibbons
 * POS/370 - Programming Concepts
 * October 23, 2005
 */

#include <stdlib.h>
#include <stdio.h>

#define GREETING_MSG    "Currrency Conversion"

typedef enum { 
    CURRENCY_YEN = 0, 
    CURRENCY_EURO = 1,
    CURRENCY_CAN = 2,
    CURRENCY_UK = 3,
    CURRENCY_AUS = 4,
    CURRENCY_SWISS = 5,
    CURRENCY_MAX = 6,
} currency_t;


/* forward reference */
void displayGreeting();
void displayCurrencies();
const double getCurrencyRate(const currency_t currency);
const char* getCurrencyName(const currency_t currency);


/* main program entry point */
int main() {
    displayGreeting();
    displayCurrencies();
    return EXIT_SUCCESS;
}


/* displays a greeting message to the user */
void displayGreeting() {
    puts(GREETING_MSG);
}


/* displays a currency and its equivalent in US dollars */
void displayCurrencies() {
    currency_t currency = CURRENCY_YEN;
    while (currency < CURRENCY_MAX) {
        const char* currencyName = getCurrencyName(currency);
        const double currencyRate = getCurrencyRate(currency);
        double currencyToDollars = (1.0 / currencyRate);

        printf("1 U.S. $ = %.6f %-12.12s \t1 %s = %.6f $ U.S.\n", 
            currencyToDollars, currencyName, currencyName, currencyRate);

        currency++;
    }
}


/* retrieves the current exchange rate for the specified currency 
   to US dollars; a returned rate of 0.0 indicates an invalid currency */
const double getCurrencyRate(currency_t currency) {
    double currencyRate;
    // these values retrieved from http://finance.yahoo.com/currency on
    // the close of business, Oct. 28th, 2005.
    switch (currency) {
        case CURRENCY_YEN:
            currencyRate = 0.008644;
            break;
        case CURRENCY_EURO:
            currencyRate = 1.2070;
            break;
        case CURRENCY_CAN:
            currencyRate = 0.8495;
            break;
        case CURRENCY_UK:
            currencyRate = 1.7734;
            break;
        case CURRENCY_AUS:
            currencyRate = 0.7492;
            break;
        case CURRENCY_SWISS:
            currencyRate = 0.7811;
            break;
        default:
            currencyRate = 0.0;
            break;
    }
    return currencyRate;
}


/* retrieves the current currency name for the specified currency;
   a return value of NULL indicates an invalid currency */
const char* getCurrencyName(currency_t currency) {
    const char* currencyName;
    switch (currency) {
        case CURRENCY_YEN:
            currencyName = "Yen";
            break;
        case CURRENCY_EURO:
            currencyName = "Euro";
            break;
        case CURRENCY_CAN:
            currencyName = "Can $";
            break;
        case CURRENCY_UK:
            currencyName = "U.K. Pounds";
            break;
        case CURRENCY_AUS:
            currencyName = "AU $";
            break;
        case CURRENCY_SWISS:
            currencyName = "Swiss Franc";
            break;
        default:
            currencyName = NULL;
            break;
    }
    return currencyName;
}
