//
// DCG_PRG410_WK5.cpp
// Mortgage Calculator - McBride Financial Services, Change Request # 11
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// October 15, 2007
//
// Version | Date       | Description of Change
// --------+------------+-----------------------------------------------------
//  1.0    | 09/20/2007 | Initial version; implementation of change request 8.
// --------+------------+-----------------------------------------------------
//  2.0    | 09/27/2007 | implementation of change request 9.
// --------+------------+-----------------------------------------------------
//  3.0    | 09/29/2007 | implementation of change request 10. changed 
//         |            | currency datatypes to double for more accuracy.
// --------+------------+-----------------------------------------------------
//  4.0    | 10/07/2007 | implementation of change request 11.
// --------+------------+-----------------------------------------------------
//
// Change Request # 11
//
// Write the program as a procedural C++ program. Allow the user to input the 
// amount of a mortgage and then select from a menu of mortgage loans:
//
// - 7 year at 5.35%
// - 15 year at 5.5%
// - 30 year at 5.75%
//
// Use an array for the different loans. Display the mortgage payment amount. 
// Then, list the loan balance and interest paid for each payment over the term 
// of the loan. On longer-term loans, the list will scroll off the screen. Do 
// not allow the list to scroll off the screen, but rather display a partial 
// list and then allow the user to continue the list. Allow the user to loop back
// and enter new data or quit. Insert comments in the program to document the 
// program.
//

#include <cstdlib>      // needed for abs() function
#include <cmath>        // needed for pow() function
#include <sstream>      // needed for stringstream
#include <iostream>     // needed for standard output
#include <iomanip>      // needed to change output formatting
#include <string>       // needed for getline() function

using namespace std;

// a structure type used to define our predefined mortgages that the user can
// choose from
typedef struct
{
  const int term;
  const double rate;
}
mortgageType;

// constants
const int MONTHS_PER_YEAR = 12; 
const int MAX_MORTGAGES = 3;
const mortgageType MORTGAGES[MAX_MORTGAGES] =
{
  {  7, 5.35 / 100.0 }, //  7 years @ 5.35%
  { 15, 5.50 / 100.0 }, // 15 years @ 5.50%
  { 30, 5.75 / 100.0 }  // 30 years @ 5.75%
};


// forward reference for other functions in the file
template <class T> const bool getValue(T& v, const bool numericOnly);
const bool getTimeToQuit();
const void pausePrompt();
const bool getMortgageTerms(double& principal, 
                            double& rate,
                            int& term);
const double calculateMortgagePayment(const int term, 
                                     const double rate, 
                                     const double principal);
void printMortgageDetails(const int term, 
                          const double rate, 
                          const double principal, 
                          const double amount);
void printBreakdownHeader(const int year, const double amount);
void printProgramHeader();

// main program entry point
int main()
{
  printProgramHeader();

  bool timeToQuit = false;
  do
  {
    double principal;
    double rate;
    int term;

    // retrieve the mortgage terms from the user; if the input was valid, then
    // calculate and print the mortgage payment details
    if (getMortgageTerms(principal, rate, term))
    {
      const double monthlyPayment = calculateMortgagePayment(term, rate, principal);
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
template <class T> const bool getValue(T& v, const bool numericOnly)
{
  // read an entire line from the user first before trying to extract a
  // value. this allows any error in the input to be more easily handled
  string lineBuffer;
  getline(cin, lineBuffer);

  bool isValid;
  if (lineBuffer.empty())
  {
    isValid = false;
  }
  else if (numericOnly && 
           lineBuffer.find_first_not_of("0123456789.", 0) != string::npos)
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
  const bool timeToQuit = getValue(answer, false) && 
                          (answer == 'N' || answer == 'n');
  cout << endl;

  return timeToQuit;
}

//
// Prompts the user to press return in order to continue something...
//
const void pausePrompt()
{
  cout << endl
       << "Press return to continue...";
  cout.flush();

  string lineBuffer;
  getline(cin, lineBuffer);
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
const bool getMortgageTerms(double& principal, 
                            double& rate, 
                            int& term)
{
  cout << "Mortgage Principal: " << flush;
  if (!getValue(principal, true) || principal < 1.00f)
  {
    cout << "Invalid Principal Amount" << endl;
    return false;
  }

  // display the list of predefined mortgages as a menu list
  cout << endl << "Available Mortgages:" << endl;;
  for (int mortgage = 0; mortgage < MAX_MORTGAGES; mortgage++)
  {
    cout << "\t" << (mortgage + 1) << ") "
         << MORTGAGES[mortgage].term << " years @ "
         << fixed << setprecision(2) 
         << MORTGAGES[mortgage].rate * 100.0 << "%"
         << endl;
  }
  cout << endl << "Pick a Mortgage:" << flush;

  // get the mortgage choice from the user
  int mortgage;
  if (!getValue(mortgage, true) || mortgage < 1 || mortgage > MAX_MORTGAGES)
  {
    cout << "Invalid Mortgage Choice" << endl;
    return false;
  }

  // retrieve the mortgage terms from the predefined array
  rate = MORTGAGES[mortgage - 1].rate;
  term = MORTGAGES[mortgage - 1].term;

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
const double calculateMortgagePayment(const int term, 
                                     const double rate, 
                                     const double principal)
{
  const double monthlyRate = rate / MONTHS_PER_YEAR;
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
  const double payment = principal * monthlyRate 
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
                          const double rate, 
                          const double principal, 
                          const double amount)
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

  printBreakdownHeader(1, amount);

  const double monthlyRate = rate / MONTHS_PER_YEAR;
  const int numPayments = term * MONTHS_PER_YEAR;
  double currentPrincipal = principal;

  for (int month = 1; month <= numPayments; month++)
  {
    const double interestPayment = monthlyRate * currentPrincipal;
    const double principalPayment = amount - interestPayment;
    currentPrincipal = abs(currentPrincipal -  principalPayment);

    cout << setw(10) << right << month 
         << " "
         << "$" << setw(10) << right << interestPayment
         << " "
         << "$" << setw(10) << right << principalPayment
         << " "
         << "$" << setw(10) << right << currentPrincipal
         << endl;

    if (month % MONTHS_PER_YEAR == 0 && month < numPayments)
    {
      pausePrompt();
      printBreakdownHeader(month / MONTHS_PER_YEAR + 1, amount);
    }
  }

}

//
// Displays a header for the breakdown report
//
void printBreakdownHeader(const int year, const double amount)
{
  cout << endl
       << "Year: " << setw(2) << year
       << " \tMonthly Payment: " << fixed << setprecision(2) << "$" << amount
       << endl
       << setw(10) << right << "Month # "
       << " "
       << setw(10) << right << "Interest "
       << " "
       << setw(10) << right << "Principal "
       << " "
       << setw(10) << right << "Balance"
       << endl;
}

//
// Displays a program header
//
void printProgramHeader()
{
  cout << endl
       << "\tMortgage Calculator (Week 5)" << endl
       << "\tDavid C. Gibbons" << endl
       << "\tPRG/410 - C++ Programming I" << endl
       << endl;
}

