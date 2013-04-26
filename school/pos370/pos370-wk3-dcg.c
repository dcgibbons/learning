/*
 * pos370-wk3-dcg.c
 * Week Three Program for David C. Gibbons
 * POS/370 - Programming Concepts
 * October 30, 2005
 */


/* standard library include files */
#include <stdlib.h>
#include <stdio.h>


/* forward reference for internal functions */
void displayGreeting();
void displayCurrency(char* name, float exchangeRate);


/* main program entry point */
int main() {
    displayGreeting();

    // the following exchange rates were obtained from
    // http://finance.yahoo.com/currency at the close of business on 
    // October 28, 2005
    displayCurrency("Yen", 0.008644F);
    displayCurrency("Euro", 1.2070F);
    displayCurrency("Can $", 0.8495F);
    displayCurrency("UK Pounds", 1.7734F);
    displayCurrency("Aus $", 0.7492F);
    displayCurrency("Swiss Francs", 0.7811F);

    return EXIT_SUCCESS;
}


/* displays a greeting message to the user */
void displayGreeting() {
    puts("Currency Conversion\n");
}


/* displays a currency and its equivalent in US dollars */
void displayCurrency(char* name, float exchangeRate) {
    // calculate the units of the given currency to 1 U.S. dollar using the
    // provided exchange rate
    float currencyToDollars = (1.0F / exchangeRate);

    printf("1 U.S. $ = %.6f %s\n", currencyToDollars, name);
}

