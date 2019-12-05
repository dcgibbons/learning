/***************************************************************************
 *   Copyright (C) 2005 by Jon Barrett   *
 *   jbarrettcr@gmail.com   *
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
          * to be used for curent or future reference.      *
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


  printf("\n\tCurrency Conversion\n");
  printf("\nConvert from US dollar to Costa Rican Colon\n");
  printf("\nEnter the US dollar amount to convert: ");
  scanf("%f", &fltUS_Dollar);
  printf("\nYour conversion is %.4f\n\n", fltUS_Dollar*ConvertToColon);

  return EXIT_SUCCESS;
}
