//
// Ch2_Ex1.cpp
// Chapter 2, Exercise 1
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// September 15, 2007
//
// 1. Write an ISO/ANSI C++ program that asks the user to enter a number and
//    then prints it out, using an integer as a local variable.
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

	// display the result
	cout << endl << "You entered: " << x << endl;

	// exit the program 
	return 0;
}

// reads an integer from the user and handle bad inputint readInt()
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
