/* Robert Daly Currency Conversion */

/* Expand the "Currency Conversion" program to accept one input 
currency, which is error checked as a valid entry, and then display its 
equivalency in US dollars.
*/

#include <stdio.h>

/*The #include directive allows external header files to be processed by 
the compiler. The stdio header provides functions for performing input and output.*/

int main ()

/*This is a function, in particular the main block.*/

{
	int choice;
	float money;
	float total;

	/*output text*/
	printf("\n\nCURRENCY CONVERSION\n");
	printf(" \n");
	printf("1. Euro\n");
	
	/*ask user to decide what kind of money to convert and how much*/
	printf("\n\n What would you like to convert your money to? (1): ");
	scanf("%d",&choice);

	printf("\n\n How much money do you want to convert? (US Dollars): ");
	scanf("%f",&money);

/* This is where the scanf looks for the number number that was inputed. Then the
 information is displayed.*/

if(choice == 1)
	{
		total = money * 0.8285;
		printf("\n\n You will have %f Euro!\n\n", total);
	}
	
	/*normal program end*/
	return 0;
	getchar();
}

