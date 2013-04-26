/***************************************************************************
 *   Copyright (C) 2005 by Jon Barrett                                     *
 *   jbarrettcr@gmail.com                                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

/*************************************
 *Currency Converter, by Jon Barrett *
 *   POS370 Programming Concepts     *
 *     University of Phoenix         *
 *************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h> 
#include <stdlib.h>

  int main(int argc, char *argv[])


{  // This starts the main block of code.

   int intSelection=0;                // This will be the input expceted from a menu selection

   float fltUS_Dollar=0;              // This function will be used to accept a numerical value                                                from the users input, in US Dollars.



         /***************************************************
          * These are the hard coded currency rates         *
          ***************************************************/ 


   float ConvertToColon=490.050;      // conversion to Costa Rican Colon
   float ConvertToRiyal=3.75130;      // conversion rate of Saudi Riyal
   float ConvertToDjibouti=183.790;   // conversion rate of Djibouti Franc
   float ConvertToYemeni=180.120;     // conversion rate of Yemeni Rial
   float ConvertToLaoKip=10899.00;    // conversion rate of Lao Kip
   float US_Dollar=1;		      // This is a set value of 1 dollar to represent the basic                                                conversion.


         /**************************************************
          * Below: This is my menu selection block. Users  *
          * choose a number between 1-6.                   *
          **************************************************/

 do
 { //begin do while loop

  printf("\nCurrency Conversion\n");
  printf("\n1\tConvert from US dollar to Costa Rican Colon");
  printf("\n2\tConvert from US dollar to Saudi Riyal");
  printf("\n3\tConvert from US dollar to Djibouti Franc");
  printf("\n4\tConvert from US dollar to Yemeni Rial");
  printf("\n5\tConvert from US dollar to Lao Kip");
  printf("\n6\tQuit");
  printf("\nEnter your Selection: ");
  scanf("%d", &intSelection);

  switch (intSelection)

  {  // begin switch
  case 1:

    if (intSelection==1)

     printf("\nEnter US dollar amount: ");
     scanf("%f", &fltUS_Dollar);
     printf("\nYour currency conversion is %.4f Colones.\n\n", fltUS_Dollar*ConvertToColon);

    break;

  case 2:

   if (intSelection==2)

    printf("\nEnter US dollar amount: ");
    scanf("%f", &fltUS_Dollar);
    printf("\nYour currency conversion is %.4f Riyal.\n\n", fltUS_Dollar*ConvertToRiyal);

   break; 

  case 3:

   if (intSelection==3)

    printf("\nEnter US dollar amount: ");
    scanf("%f", &fltUS_Dollar);
    printf("\nYour currency conversion is %.4f Francs.\n\n", fltUS_Dollar*ConvertToDjibouti);

   break;

  case 4:

   if (intSelection==4)

    printf("\nEnter US dollar amount: ");
    scanf("%f", &fltUS_Dollar);
    printf("\nYour currency conversion is %.4f Rial.\n\n", fltUS_Dollar*ConvertToYemeni);

   break;

  case 5:

   if (intSelection==5)

    printf("\nEnter US dollar amount: ");
    scanf("%f", &fltUS_Dollar);
    printf("\nYour currency conversion is %.4f Lao Kip.\n\n", fltUS_Dollar*ConvertToLaoKip);

   break;

  } //end switch

 } while (intSelection!=6);  // this tells the program to loop unless 6 is selected.

  return EXIT_SUCCESS;

} // ends main block

