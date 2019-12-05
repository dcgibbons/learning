//
// DCG_WK3_Ex1.cpp
// Week 3, Exercise 1
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// September 29, 2007
//
// 1. Write a native C++ program that allows an unlimited number of values to 
//    be entered and stored in an array allocated in the free store. The 
//    program should create a new array with five additional elements when
//    necessary and copy values from the old array to the new.

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

const int INIT_SIZE = 5; // initial size of array
const int INCR_SIZE = 5; // incremental size of array

// forward reference
int* reallocArray(const int* const oldArray, int oldSize, int newSize);

int main()
{
  cout << "Enter an unlimited number of integer values, one per line. " << endl
       << "Enter a blank line to exit." << endl << endl;

  int currSize = 0;                     // no elements yet!
  int maxSize = INIT_SIZE;              // initial size of the array
  int* values = new int[maxSize];       // allocate the initial array
  int reallocs = 0;                     // number of times we've reallocated

  string lineBuffer;
  do
  {
    // read a line at a time; exit when a blank line is entered
    getline(cin, lineBuffer);

    if (lineBuffer.length() > 0)
    {
      // attempt to process an integer out of the line
      istringstream iss(lineBuffer);
      int n;
      iss >> n;
      if (iss.fail())
      {
        cout << "Invalid integer value; "
             << "please try again or enter a blank line to exit." << endl;
      }
      else
      {
        // add the value to the array; if the array is full, then reallocate
        // it with an additional increment of elements
        values[currSize++] = n;
        if (currSize == maxSize)
        {
          values = reallocArray(values, maxSize, maxSize + INCR_SIZE);
          maxSize += INCR_SIZE;
          reallocs++;
        }
      }
    }
  }
  while (lineBuffer.length() > 0);

  // print the values, 5 per line, and calculate the total of all values
  long int total = 0;
  for (int i = 0; i < currSize; i++)
  {
    total += values[i];
    cout << setw(8) << values[i] << ' ';
    if ((i + 1) % 5 == 0) 
    {
      cout << endl;
    }
  }
  cout << endl;

  // display the average of all values and the number of times we reallocated
  // the array
  cout << "Average of " << currSize+1 << " values = " << total / (currSize+1) << endl;
  cout << "Value array was reallocated " << reallocs << " times" << endl;

  return 0;
}

/**
 * Reallocates an integer array, taking care that old data is copied to the new
 * array.
 *
 * oldArray - a pointer to the old array
 * oldSize - the size of the old array, in integer elements
 * newSize - the size of the new array, in integer elemnets
 *
 * returns a new array with the old data copied as needed
 */
int* reallocArray(const int* const oldArray, const int oldSize, const int newSize)
{
  int* newArray = new int[newSize];
  if (newArray == NULL)
  {
    cerr << "Insufficient memory" << endl;
    // let's just let it fail the old fashioned way
  }

  // copy over the available values that will fit
  for (int i = 0, n = min(oldSize, newSize); i < n; i++)
  {
    newArray[i] = oldArray[i];
  }

  // free the old array's memory
  delete[] oldArray;

  return newArray;
}

