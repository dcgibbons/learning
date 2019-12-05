/*
 * pos370-wk4-dcg.c
 * Week Four Program for David C. Gibbons
 * POS/370 - Programming Concepts
 * November 6, 2005
 * Version 3.00
 */


/* standard library include files */
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>


#define MAX_CURRENCIES  6


// make sure BUFSIZ is defined on systems that don't define it
#ifndef BUFSIZ
#define BUFSIZ 1024
#endif


// a data structure used to store a currency and its exchange rate
typedef struct {
    char* currencyName;
    double exchangeRate;
} currency_t;


/* forward reference for internal functions */
void displayGreeting();
void populateCurrencies(currency_t currencies[], int max);
int getCurrencySelection(currency_t currencies[], int max);
void displayMenu(currency_t currencies[], int max);
double getCurrencyAmount(char* currencyName);
void convertCurrency(char* currencyName, 
                     double excchangeRate, 
                     double currencyAmount);


/* main program entry point */
int main() 
{
    int selection;
    double currencyAmount;
    currency_t currencies[MAX_CURRENCIES];

    displayGreeting();
    populateCurrencies(currencies, MAX_CURRENCIES);

    // determine which currency the user wishes to convert and loop until
    // they are ready to exit
    while ((selection = getCurrencySelection(currencies, MAX_CURRENCIES)) != -1)
    {
        char* currencyName = currencies[selection].currencyName;
        double exchangeRate = currencies[selection].exchangeRate;

        // determine how much of the currency the user wishes to convert
        if ((currencyAmount = getCurrencyAmount(currencyName)) > 0.0)
        {
            convertCurrency(currencyName, exchangeRate, currencyAmount);
        }
    }

    return EXIT_SUCCESS;
}


/* displays a greeting message about this program */
void displayGreeting()
{
    puts("Currency Conversion\n");
}


/* populates the currencies array with the current exchange rates */
void populateCurrencies(currency_t currencies[], int max)
{
    int n = 0;

    // populate the currencies array - Miracle C doesn't support a static
    // initialization of an array, so this uglyness is necessary
    // these exchange rates were retrieved from 
    // http://finance.yahoo.com/currency as of the close of business 
    // on November 9, 2005
    currencies[n].currencyName = "Yen";
    currencies[n++].exchangeRate = 117.7650;

    currencies[n].currencyName = "Euro";
    currencies[n++].exchangeRate = 0.8370;

    currencies[n].currencyName = "Can $";
    currencies[n++].exchangeRate = 1.1862;

    currencies[n].currencyName = "U.K.";
    currencies[n++].exchangeRate = 0.5738;

    currencies[n].currencyName = "AU $";
    currencies[n++].exchangeRate = 1.3638;

    currencies[n].currencyName = "Swiss Franc";
    currencies[n++].exchangeRate = 1.3105;

    // make sure we didn't goof and exceed the bounds of the array
    assert(n <= max);
}


/* determine which currency the user wishes to convert; -1 will be returned
   if the user did not make a valid selection */
int getCurrencySelection(currency_t currencies[], int max) 
{
    char lineBuffer[BUFSIZ];
    int selection = -1;

    // display the menu of choices
    do
    {
        displayMenu(currencies, max);
    
        // determine the user's selection - retrieve a line buffer first so
        // scanf doesn't have to deal with continued errors in the input stream
        fflush(stdin);
        if (fgets(lineBuffer, sizeof(lineBuffer), stdin) == NULL)
        {
            // end-of-file! go ahead and exit
            selection = -1;
            break;
        }
        else if (sscanf(lineBuffer, "%d", &selection) == 1)
        {
            selection--; // fix selection to be an zero-based array offset
            // verify that the selection is within the valid range of
            // currencies and if not set the selection to -1
            if (selection < 0 || selection > max)
            {
                selection = -1;
            }
            else if (selection == max)
            {
                // time to exit!
                selection = -1;
                break;
            }
        }
        else
        {
            selection = -1;
        }

        if (selection == -1)
        {
            printf("Invalid menu selection. Please try again.\n");
        }
    }
    while (selection == -1);

    return selection;
}


/* displays a menu of available currencies the user may select */
void displayMenu(currency_t currencies[], int max)
{
    char* menuFormat = "%2d. %s\n";
    int i;

    printf("\nCurrency Selection\n");
    for (i = 0; i < max; i++)
    {
        // display the menu option for the currency, but ensure that the
        // menu selections are one-based instead of zero-based
        printf(menuFormat, i + 1, currencies[i].currencyName);
    }
    printf(menuFormat, i + 1, "Exit Program");
    printf("\nEnter selection: ");
    fflush(stdout);
}


/* retrieves the amount of currency to convert to dollars; if an invalid 
   value was input by the user then 0.0 is returned */
double getCurrencyAmount(char* currencyName)
{
    char lineBuffer[BUFSIZ];
    double currencyAmount = 0.0;

    printf("Enter amount of %s currency to convert to U.S. $: ", currencyName);
    fflush(stdout); // force output to be displayed before newline is sent

    // determine the user's selection - retrieve a line buffer first so
    // scanf doesn't have to deal with continued errors in the input stream
    if (fgets(lineBuffer, sizeof(lineBuffer), stdin) != NULL)
    {
        if (sscanf(lineBuffer, "%lf", &currencyAmount) != 1)
        {
            printf("Invalid amount. Please enter a valid decimal value.\n");
        }
    }

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

