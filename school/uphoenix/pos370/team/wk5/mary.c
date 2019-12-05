//Currency conversion due week 5, Mary Reck, POS370
//includes libraries

#include <stdio.h>
#include <stdlib.h>

//defines the values and subtasks to be used

float usDollar;
float newVal;
float foreign;
char sel;
char again;
char y;
char n;
void curr_check(void);
void us_conv_check(void);



main()

{



//prints title and name, gives direction to subtasks

printf("\n\n\t\tCurrency Conversion Program, by Mary Reck\n\n\n");

//calls subtasks to select currency, then enter US Dollars and get value


start:

curr_check();
us_conv_check();


}

void curr_check(void)

{

	//displays currency options to be selected from listing, if another number is selected, it begins currency request again.

	printf("\n1 German Marks\n\n2 French Francs\n\n3 British Pounds\n\n4 Chinese Yen\n\n5 Italian Lira\n\n\nWhat currency would you like to use?  ");

	scanf("%d", &sel);

	if (sel == 1) 

	foreign = .25 , printf("\n\nYou have selected German Marks @ .25 per US Dollar\n" );

		else if (sel == 2)

		foreign = .5, printf("\n\nYou have selected French Francs @ .5 per US Dollar\n" );

			else if (sel == 3)

			foreign = .75,  printf("\n\nYou have selected British Pounds @ .75 per US Dollar\n" );

				else if (sel == 4)

				foreign = 1.25, printf("\n\nYou have selected Chinese Yen @ 1.25 per US Dollar\n" );

					else if (sel == 5)

					foreign = 1.5, printf("\n\nYou have selected Italian Lira @ 1.5 per US Dollar\n" );

						//last else if is for most other numbers it states invalid & goes to beginning,
						// PROBLEM NOTE:  if character or letter selected, this runs into a loop and you must
						// exit by closing the DOS screen and hitting End

						else if (sel > 5)

						printf ("\n\n\t INVALID SELECTION \n\n"), curr_check();
						

}

void us_conv_check(void)

{

	//requests dollar value that user would like to convert
	//PROBLEM NOTE:  if letters or invalid characters are entered
	//computer runs into a loop and you must exit by closing DOS screen and hitting End


	printf("\n\nWhat US Dollar between $.01 and $9,999.00 would you like to convert? ");

	scanf("%f", &usDollar);
	
	if (usDollar < .01 || usDollar > 9999.99 )
	
	printf("\n\tINVALID VALUE ENTERED\n"),us_conv_check();
	
	else

	newVal = usDollar * foreign;

	printf("\n\n\tYou selected a rate of %f (per US Dollar) \n\n\tand entered $%f \n\n\tresulting value is %f", foreign, usDollar, newVal);

	printf("\n\n\nWould you like to try another conversion?  ");

	scanf("%c", &again);
	
	printf ("%c", again);
	
	if (again == 'y') main();

}
