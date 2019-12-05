//
// DCG_PRG410_WK3.cpp
// Mortgage Calculator - McBride Financial Services, Change Request # 9
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// October 1, 2007
//
// Version | Date       | Description of Change
// --------+------------+-----------------------------------------------------
//  1.0    | 09/20/2007 | Initial version; implementation of change request 8.
// --------+------------+-----------------------------------------------------
//  2.0    | 09/27/2007 | implementation of change request 9.
// --------+------------+-----------------------------------------------------
//
// Change Request #9
//
// Write the program as a procedural C++ program. Calculate and display the 
// mortgage payment amount using the amount of the mortgage, the term of the 
// mortgage, and the interest rate of the mortgage as input by the user. Allow
// the user to loop back and enter new data or quit. Insert comments in the 
// program to document the program.
//

#include <cmath>        // needed for pow() function
#include <sstream>      // needed for stringstream
#include <iostream>     // needed for standard output
#include <iomanip>      // needed to change output formatting
#include <string>       // needed for getline() function

using namespace std;

// forward reference for other functions in the file
template <class T> const bool getValue(T& v);
const bool getTimeToQuit();
const bool getMortgageTerms(float& principal, 
                            float& rate,
                            int& term);
const float calculateMortgagePayment(const int term, 
                                     const float rate, 
                                     const float principal);
void printMortgageDetails(const int term, 
                          const float rate, 
                          const float principal, 
                          const float amount);
void printProgramHeader();

// main program entry point
int main()
{
  printProgramHeader();

  bool timeToQuit = false;
  do
  {
    float principal;
    float rate;
    int term;

    // retrieve the mortgage terms from the user; if the input was valid, then
    // calculate and print the mortgage payment details
    if (getMortgageTerms(principal, rate, term))
    {
      const float monthlyPayment = calculateMortgagePayment(term, rate, principal);
      printMortgageDetails(term, rate, principal, monthlyPayment);
    }

    // ask the user if they wish to quit or continue
    timeToQuit = getTimeToQuit();
  }
  while (!timeToQuit);

  cout << endl
       << "Mortgage Calculator exiting" << endl
       << endl;

  return 0;
}

//
// Retrieves a single value from the user
//
// v - a reference to a variable that will receive the input value
//
// returns true if a value was successfully read
//
template <class T> const bool getValue(T& v)
{
  // read an entire line from the user first before trying to extract a
  // value. this allows any error in the input to be more easily handled
  string lineBuffer;
  getline(cin, lineBuffer);

  bool isValid;
  if (lineBuffer.length() == 0) // don't process an empty line
  {
    isValid = false;
  }
  else
  {
    // create a stringstream around the line that was just read and try and
    // read a value of the provided data type from it
    istringstream iss(lineBuffer);
    iss >> v;

    // determine if the input value was read successfully
    isValid = !iss.fail();
  }

  return isValid;
}

//
// Prompts the user if they wish to continue and calculate another mortgage
//
// returns true if user wishes to quit, false otherwise
//
const bool getTimeToQuit()
{
  cout << endl
       << "Do you want to calculate another mortgage? [Y/n] " << flush;

  // only quit if the user inputs something and it's a "no" answer
  char answer;
  const bool timeToQuit = getValue(answer) && (answer == 'N' || answer == 'n');
  cout << endl;

  return timeToQuit;
}

//
// Prompts the user for mortgage information
//
// principal - variable that will receive principal amount
// rate - variable that will receive interest rate
// term - variable that will receive mortgage term
//
// returns false if end-of-file or input is invalid
//
const bool getMortgageTerms(float& principal, 
                            float& rate, 
                            int& term)
{
  cout << "Mortgage Principal: " << flush;
  if (!getValue(principal) || principal < 1.00f)
  {
    cout << "Invalid Principal Amount" << endl;
      return false;
  }

  cout << "Interest Rate: " << flush;
  if (!getValue(rate) || rate < 0.0f || rate > 100.0f)
  {
    cout << "Invalid Interest Rate" << endl;
    return false;
  }
  else
  {
    // convert the rate to a percentage
    rate /= 100.0f;
  }

  cout << "Mortgage Term (Years): " << flush;
  if (!getValue(term) || term < 1 || term > 50)
  {
    cout << "Invalid Mortgage Term" << endl;
    return false;
  }

  return true;
}

//
// calculates a monthly mortgage payment using a standard amortization 
// deriviation
//
// term - term of mortgage in years
// rate - interest rate of mortgage as a percentage, i.e. 5% = 0.05
// principal - amount of the mortgage
//
// returns periodic (monthly) mortgage payment amount
//
const float calculateMortgagePayment(const int term, 
                                     const float rate, 
                                     const float principal)
{
  const int MONTHS_PER_YEAR = 12; 
  const float monthlyRate = rate / MONTHS_PER_YEAR;
  const int numPayments = term * MONTHS_PER_YEAR;

  // use standard amortization formula
  //           P i
  // A = ----------------
  //     1 - (1 + i) ^ -n
  // where 
  //   A = periodic payment amount
  //   P = amount of principal
  //   i = periodic interest rate
  //   n = total number of payments
  const float payment = principal * monthlyRate 
    / (1 - pow(1 + monthlyRate, -numPayments));

  return payment;
}

//
// prints the details of the mortgage to an output stream
//
// out - the output stream to print mortgage details on
// term - the term of the mortgage
// rate - the interest rate of the mortgage
// principal - the principal amount of the mortgage
// payment - the monthly payment amount
//
void printMortgageDetails(const int term, 
                          const float rate, 
                          const float principal, 
                          const float amount)
{
  cout << endl
       << fixed << setprecision(2) // all numbers as fixed-point with 2 digits
       << setw(24) << left << "Term in Years:"
       << " " << setw(7) << right << term << endl
       << setw(24) << left << "Interest Rate:"
       << " " << setw(10) << right << rate * 100.0 << "%" << endl
       << setw(24) << left << "Mortgage Principal:" 
       << "$" << setw(10) << right << principal << endl
       << setw(24) << left << "Monthly Payment:"
       << "$" << setw(10) << right << amount 
       << endl << endl;
}

//
// Displays a program header
//
void printProgramHeader()
{
  cout << endl
       << "\tMortgage Calculator (Week 3)" << endl
       << "\tDavid C. Gibbons" << endl
       << "\tPRG/410 - C++ Programming I" << endl
       << endl;
}

