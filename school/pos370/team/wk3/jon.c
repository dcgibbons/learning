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


   float ConvertToColon=490.050;      // conversion to Costa Rican Colon
   float ConvertToRiyal=3.75130;      // conversion rate of Saudi Riyal
   float ConvertToDjibouti=183.790;   // conversion rate of Djibouti Franc
   float ConvertToYemeni=180.120;     // conversion rate of Yemeni Rial
   float ConvertToLaoKip=10899.00;    // conversion rate of Lao Kip
   float US_Dollar=1;		      // This is a set value of 1 dollar to represent the basic conversion.

/************************************************************
 * The following block is the main block, that shows the    *
 *complete coversion rates from US Dollar to Foreign        *
 *Currency and vise-a-versa. This program was set up to     *
 *execute on one word, "currency."                          *
 ************************************************************/

  printf("\nOne US Dollar to Costa Rican Colon is: \t%.3f\n", US_Dollar*ConvertToColon);

  printf("\nOne US Dollar to Djibouti Franc is: \t%.3f\n", US_Dollar*ConvertToDjibouti);

  printf("\nOne US Dollar to Saudi Riyal is: \t%.3f\n", US_Dollar*ConvertToRiyal);

  printf("\nOne US Dollar to Yemeni Rial is: \t%.3f\n", US_Dollar*ConvertToYemeni);

  printf("\nOne US Dollar to Lao Kip is: \t\t%.3f\n\n", US_Dollar*ConvertToLaoKip);

/************************************************************
 *I used the "\n" and the "\t" to move the cursor to the    *
 *next line so it would be more ledgible when executed.     *
 *It also seperates the block of information and tabs the   *
 *output, "currency rate," to be more ledgible.             * 
 ************************************************************/

  return EXIT_SUCCESS;
}
