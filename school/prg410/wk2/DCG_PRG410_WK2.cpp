//
// DCG_PRG410_WK2.cpp
// Mortgage Calculator - McBride Financial Services, Change Request # 8
// David C. Gibbons
// PRG/410 - C++ Programming I
// Chip Dickson
// September 24, 2007
//
// Version | Date       | Description of Change
// --------+------------+-----------------------------------------------------
//  1.0    | 09/20/2007 | Initial version; implementation of change request 8.
// --------+------------+-----------------------------------------------------
//
// Change Request #8
// 
// Requestor: Bud Shepherd - Bismarck, ND
//
// Write the program as a procedural C++ program and using a loan amount of 
// $200,000, a term of 30 years, and an interest rate of 5.75%. Insert comments
// in the program to document the program.
//

#include <cmath>        // needed for pow() function
#include <iostream>     // needed for standard output
#include <iomanip>      // needed to change output formatting

using namespace std;

// forward reference for other functions in the file
const float calculateMortgagePayment(const int term, 
                                     const float rate, 
                                     const float principal);
void printMortgageDetails(ostream& out, 
                          const int term, 
                          const float rate, 
                          const float principal, 
                          const float amount);

// main program entry point
int main()
{
  const int term = 30;                  // 30 years
  const float rate = 5.75F / 100.0F;    // 5.75%
  const float principal = 200000.00F;   // $200,000.00

  const float monthlyPayment = calculateMortgagePayment(term, rate, principal);
  printMortgageDetails(cout, term, rate, principal, monthlyPayment);

  // exit the program
  return 0;
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
void printMortgageDetails(ostream& out, 
                          const int term, 
                          const float rate, 
                          const float principal, 
                          const float amount)
{
  out << endl
       << "\tMortgage Calculator (Week 2)" << endl
       << "\tDavid C. Gibbons" << endl
       << "\tPRG/410 - C++ Programming I" << endl;

  out << endl
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

