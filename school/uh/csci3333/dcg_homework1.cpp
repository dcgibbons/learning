/**
 * PROGRAMMER:          David C. Gibbons
 * COURSE:              CSCI 3333.01
 * DATE:                September 17, 2008
 * ASSIGNMENT:          Homework # 1
 * ENVIRONMENT:         Any ANSI C++ capable environment
 * FILES INCLUDED:      dcg_homework1.cpp
 * PURPOSE:             1. Determine if n is a multiple of m.
 *                      2. Concatenate strings and display.
 */


// include standard C++ I/O and string header files
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

// make the std namespace available throughout the remainder of the file
using namespace std;


/*
 * FUNCTION:            is_multiple
 * PURPOSE:             Determines if the integer n is a multiple of m.
 * INPUT:               n, m - both multiples
 * PRECONDITIONS:       N/A
 * OUTPUT:              true if n is a multiple of m; false otherwise
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           Use the C++ modulo operator to determine if one
 *                      integer is a factor of another.
 */
bool is_multiple(const int n, const int m)
{
  // use the built-in modulos operator to determine if n is a multiple of m
  return m % n == 0;
}


/*
 * FUNCTION:            concatenate
 * PURPOSE:             Displays concatenated strings.
 * INPUT:               retrieved from user
 * PRECONDITIONS:       N/A
 * OUTPUT:              the first character of s, the second character of t,
 *                      followed by s and t concatentated together and all
 *                      displayed on the standard output with appropriate
 *                      header information
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           Uses simple std::string operations to display the
 *                      appropriate information.
 */
void concatenate()
{
  cout << endl 
       << "Concatenate Test" << endl 
       << setfill('=') << setw(38) << " "
       << endl;

  cout << "Enter the first string: " << flush;
  string s;
  getline(cin, s);

  cout << "Enter the second string: " << flush;
  string t;
  getline(cin, t);

  cout << "1st character of s: '" << s.at(0) << "'" << endl
       << "2nd character of t: '" << t.at(1) << "'" << endl
       << "Concatened string:  '" << s + t << "'" << endl
       << endl;
}


/*
 * FUNCTION:            main
 * PURPOSE:             runs the program with several test cases
 * INPUT:               N/A
 * PRECONDITIONS:       N/A
 * OUTPUT:              N/A
 * POSTCONDITIONS:      N/A
 * ALGORITHM:           N/A
 */
int main(void)
{
  cout << "David C. Gibbons" << endl
       << "Homework Assignment # 1" << endl
       << "CSCI 3333 - Data Structures" << endl
       << "Fall 2008" << endl
       << "Bindra Shrestha" << endl
       << setfill('=') << setw(38) << " "
       << endl << endl;

  // try the is_multiple function
  cout << "Is Multiple Test" << endl 
       << setfill('=') << setw(78) << " "
       << endl;

  int n = 3, m = 27;
  cout << "Is " << n << " a multiple of " << m << "? "
       << (is_multiple(n, m) ? "true" : "false")
       << endl;

  // try the concatenate function
  concatenate();

  // return a successful status to the operating system
  return 0;
}

