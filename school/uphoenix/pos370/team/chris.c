/* Week 4 Individual Project
   Currency Conversion Program
   POS 370
   Christopher W. Kopyar
   11-08-05*/

// Library inclusion
#include <stdio.h>

/**************************************
Main Program
**************************************/


Main ()
{
float f_USADollars = 1.00;                        //USA Dollars
float f_convertUSA1 = ((f_USADollars)*(7.7519));    //Hong Kong Dollars
float f_convertUSA2 = ((f_USADollars)*(3.7748));    //Malaysian Ringgits
float f_convertUSA3 = ((f_USADollars)*(2144.6));   //Venezuelan Bolivars
float f_convertUSA4 = ((f_USADollars)*(1043.5));   //South Korean Wons
float f_convertUSA5 = ((f_USADollars)*(101.78));    //Sri Lanka Rupees
  
  /*The User is greated with a message stating the Programs Name. At this point the intended currency conversion is regognized and input is requested.*/
         
            printf("Welcome to the American Currency Converter\n");
            printf("Please enter the amount in Malaysian Ringgits you would like displayed in it's American Equivilent\n");
            printf("Enter the amount you would like to Convert to American Money. (In dollars & Cents):    ");
            scanf("f%.2f");
   "If (%.2f>0.00 && 1.00<%.2f)";
                 {
                 printf("The Converted Currency amount is ((%.2f) * (3.7748))\n");
                 }
              "else";
                 {
                 printf("Better Luck next time. Please try again!\n");
                 }
                 
}

//End Program