//
// DCG_WK2_Ex1.cpp
// Week 2, Exercise 2
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// September 20, 2007
//
// 2. Write a short program that finds the first 400 numbers that are multiples
//    of 13 and prints them out.
//

#include <iostream>
#include <iomanip>

using namespace std;

int main()
{
  cout << "First 400 Multiples of 13 (n >= 1):" << endl << endl;

  for (int n = 1, multiplesFound = 0; multiplesFound <= 400; n++)
  {
    // check to see if this number is a multiple of 13
    if (n % 13 == 0)
    {
      cout << setw(4) << right << n << "\t";

      multiplesFound++;

      // add a newline after every 10 columns of output
      if (multiplesFound % 10 == 0)
      {
        cout << endl;
      }
    }
  }

  cout << endl;

  return 0;
}
