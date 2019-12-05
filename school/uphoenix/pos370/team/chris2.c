/* Week 3 Individual Project
   Currency Conversion Program
   POS 370
/* Week 3 Individual Project
   Currency Conversion Program
   POS 370
   Christopher W. Kopyar
   10-31-05*/

// Library inclusion
#include <stdio.h>
#include <stdlib.h>


/**************************************
Main Program
**************************************/


Main ()
{

     char c12345 = '\0';

  while ( iselection ! = 5) {

   printf("Welcome to the Five Country American Currency Converter\n");
   printf("Choose your Country of Origin, the amount will then be displayed in it's American Equivilent\n");
   printf("For Hong Kong Currency Choose 1\n");
   printf("For Malaysian Ringgits Choose 2\n");
   printf("For Venezuelan Bolivars Choose 3\n");
   printf("For South Korean Wons Choose 4\n");
   printf("For Sri Lanka Rupees choose 5\n");
   Printf("Which Country of Origin is your Choice? (1 - 5): ");
   scanf("%d, &iselection);

float fUSADollars = 1.00;                        //USA Dollars
float fconvertUSA1 = ((fUSADollars)*(7.7519));    //Hong Kong Dollars
float fconvertUSA2 = ((fUSADollars)*(3.7748));    //Malaysian Ringgits
float fconvertUSA3 = ((fUSADollars)*(2144.6));   //Venezuelan Bolivars
float fconvertUSA4 = ((fUSADollars)*(1043.5));   //South Korean Wons
float fconvertUSA5 = ((fUSADollars)*(101.78));    //Sri Lanka Rupees


  
  /*The Following presents the Graphical Output for the Hardcoded Market Value Compared to the USA Dollar as of 10-31-05*/

   printf("\n\t\t\tCurrent Currency Conversion Rates for the USA Market\n\n");
   printf("USA Dollars = $ %.2f\n", fUSADollars);
   printf("The Following Conversion Rates are Current as of 10-31-05\n");
   printf("Please contact the US Mint or George W. Bush with any Complaints Resulting from Low Conversion Rates\n");
	printf("\1. %.2f USA Dollars Equals %.2fHong Kong Dollars\n",fUSADollars,fconvertUSA1);
	printf("\2. %.2f USA Dollars Equals %.2fMalaysian Ringgits\n",fUSADollars,fconvertUSA2);
	printf("\3. %.2f USA Dollars Equals %.2fVenezuelan Bolivars\n",fUSADollars,fconvertUSA3);
	printf("\4. %.2f USA Dollars Equals %.2fSouth Korean Wons\n",fUSADollars,fconvertUSA4);
	printf("\5. %.2f USA Dollars Equals %.2fSri Lanka Rupees\n",fUSADollars,fconvertUSA5);

}

//End Program