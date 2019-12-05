//
// Ch2_Ex2.cpp
// Chapter 2, Exercise 2
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// September 15, 2007
//
// 2. Write a program that reads an integer value from the keyboard into a
//    variable of type int, and uses one of the bitwise operators (i.e. not
//    the % operator!) to determine the positive remainder when divided by 8.
//    For example, 29 = (3x8)+5 and -14 = (-2x8)+2 have positive remainder 5
//    and 2, respectively.
//

#include <iostream>
#include <limits>

using namespace std;

// forward reference
int readInt();

int main()
{
	// read an integer from the user
	int x = readInt();

	// to determine the remainder of the user supplied integer when divided
	// by 8 without using the modulus operator we must use the bitwise AND
	// operator and mask off the 3 bits below the 4th bit (which represents 8).
	// the bitwise AND operator will only pass a 1 bit to the result if both
	// values contain a 1 bit, otherwise a 0 is passed; this gives us a way to
	// do a modulus operator for any power of 2 divisor
	int remainder = x & 7;

	// display the entered value and result to the user
	cout << "You entered: " << x << endl;
	cout << "That value divided by 8 results in a remainder of: " << remainder << endl;

	// exit the program
	return 0;
}

// reads an integer from the user and handle bad input (taken from Ch2_Ex1)
int readInt()
{
	// declare a variable to hold our number and read it in directly from the
	// user
	int x = 0;
	bool inputValid = false;
	do
	{
		// display a prompt for the user and try to read an integer
		cout << "Please enter a number: " << flush;
		cin >> x;

		// determine if the input is invalid; if so, print an error and
		// remove all the invalid data, and let the user try again
		inputValid = !cin.fail();
		if (!inputValid)
		{
			cout << "Your input was invalid; please try again." << endl;

			// clear the error state of the input stream and then ignore all
			// characters currently in the input buffer up to the next newline
			cin.clear();
			cin.ignore(numeric_limits<streamsize>::max(), '\n');
		}
	} while (!inputValid);	

	return x;
}
