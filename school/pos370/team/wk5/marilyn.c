/*Currency Exchange Program */
//by Marilyn Johnson
//POS 370
//Week 5 - Expand Currency exchange program*/

#include <stdio.h>

main()


{

int choice;
float dollar;
float total;
float conversion;

/*this will list the four choices for foreign currencies*/

	printf ("Select the currency you would like to convert.\n1 Canadian Dollars\n2 European Euro\n3 Swiss Francs\n4 Japanese Yen\n\nEnter currency option  ");
	scanf ("%d", &choice);
	

/*user must enter how much currency to convert*/

printf ("\nHow much foreign currency do you have? ");
	scanf ("%f", &dollar);
	total = dollar * conversion;



/*these are the conversion rates for foreign currencies*/


	if (choice == 1) 

		total = dollar * 1.1189;
		
	else
	if (choice == 2)

		total = dollar * 8.522;

	else
	if (choice == 3)

		total = dollar * 1.3112;
		
	else
	if (choice == 4)

		total = dollar * 118.03;
		

/*this will list the result of the dollar * conversion rate calculation*/

	

	printf ("\nBased on the 11/12/05 converson rate, the total US Dollars equals $%f", total );
	getchar();
} 



