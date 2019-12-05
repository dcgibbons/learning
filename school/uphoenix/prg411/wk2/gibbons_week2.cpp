/********************************************************************
 Program Name: gibbons_week2.cpp
        
 Student Name: David C. Gibbons

 Date: October 27, 2007

 Learning Team: A

 Course: POS/441

 Program Description: Change Request # 12
   Write the program as an object-oriented C++ program that calculates and 
   displays the mortgage payment amount from user input of the amount of 
   the mortgage, the term of the mortgage, and the interest rate of the 
   mortgage. Allow the user to loop back and enter new data or quit. Insert
   comments in the program to document the program.

**********************************************************************/

#include <cmath>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

#include "gibbons_week2.h"

using namespace std;

int main()
{
  // display a welcome banner
  cout << "\tMortgage Calculator - Week 2" << endl
       << "\tDavid C. Gibbons" << endl
       << "\tPRG/411 - C++ Programming II" << endl
       << endl;

  // calculate mortgages until the user has decided to quit
  MortgageCalculator calculator = MortgageCalculator();
  do
  {
    if (calculator.getMortgageTerms())
    {
      calculator.reportMortgageDetails();
    }
  } while (!calculator.timeToQuit());

  return 0;
}


/********************************************************************
 Function Name: Mortgage::Mortgage

 Function Description: constructor for Mortgage class

**********************************************************************/
Mortgage::Mortgage(const double principalAmount, 
                   const double interestRate, 
                   const int term)
  : MONTHS_IN_YEAR(12),
    m_principalAmount(principalAmount),
    m_interestRate(interestRate / 100.0), // convert to a percentage
    m_numPayments(term * MONTHS_IN_YEAR),
    m_monthlyPayment(calculateMonthlyPayment())
{
}

/********************************************************************
 Function Name: Mortgage::getMonthlyPayment

 Function Description: returns the monthly payment for this mortgage

**********************************************************************/
double Mortgage::getMonthlyPayment()
{
  return m_monthlyPayment;
}

/********************************************************************
 Function Name: Mortgage::calculateMonthlyPayment

 Function Description: calculates a monthly payment for this mortgage

**********************************************************************/
const double Mortgage::calculateMonthlyPayment()
{
  const double monthlyRate = m_interestRate / MONTHS_IN_YEAR;

  // use standard amortization formula
  //           P i
  // A = ----------------
  //     1 - (1 + i) ^ -n
  // where 
  //   A = periodic payment amount
  //   P = amount of principal
  //   i = periodic interest rate
  //   n = total number of payments
  const double payment = m_principalAmount * monthlyRate 
    / (1 - pow(1 + monthlyRate, -m_numPayments));

  return payment;
}

/********************************************************************
 Function Name: MortgageCalculator::getValue

 Function Description: retrieves a value of the user
                       returns true if the value was read successfully
                       for the type provided

**********************************************************************/
template <class T> 
const bool MortgageCalculator::getValue(T& v, const bool numericOnly)
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

/********************************************************************
 Function Name: MortgageCalculator::timeToQuit

 Function Description: asks the user if they wish to calculate another
                       returns true if they wish to quit

**********************************************************************/
const bool MortgageCalculator::timeToQuit()
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

/********************************************************************
 Function Name: MortgageCalculator::pausePrompt

 Function Description: asks the user to press return to continue and 
                       waits

**********************************************************************/
const void MortgageCalculator::pausePrompt()
{
  cout << endl
       << "Press return to continue..."
       << flush;

  string lineBuffer;
  getline(cin, lineBuffer);
}

/********************************************************************
 Function Name: MortgageCalculator::getMortgageTerms

 Function Description: retrieves the terms of the mortgage from the user
                       returns true if input was valid

**********************************************************************/
const bool MortgageCalculator::getMortgageTerms()
{
  cout << "Mortgage Principal: " << flush;
  double principal;
  if (!getValue(principal, true) || principal < 1.00)
  {
    cout << "Invalid Principal Amount" << endl;
    return false;
  }

  cout << "Interest Rate: " << flush;
  double rate;
  if (!getValue(rate, true) || rate < 0.0 || rate > 100.0)
  {
    cout << "Invalid Interest Rate" << endl;
    return false;
  }

  cout << "Mortgage Term (Years): " << flush;
  int term;
  if (!getValue(term, true) || term < 1 || term > 50)
  {
    cout << "Invalid Mortgage Term" << endl;
    return false;
  }

  // update the class state with the new input values
  m_principalAmount = principal;
  m_interestRate = rate;
  m_term = term;

  return true;
}

/********************************************************************
 Function Name: MortgageCalculator::reportMortgageDetails

 Function Description: displays the details of the mortgage the user
                       has input

**********************************************************************/
void MortgageCalculator::reportMortgageDetails()
{
  // amortize a mortgage and get the monthly payment
  Mortgage mortgage = Mortgage(m_principalAmount, m_interestRate, m_term);
  const double monthlyPayment = mortgage.getMonthlyPayment();

  // display a nice report tot he user
  cout << endl
     << fixed << setprecision(2) // all numbers as fixed-point with 2 digits
     << setw(24) << left << "Term in Years:"
     << " " << setw(7) << right << m_term << endl
     << setw(24) << left << "Interest Rate:"
     << " " << setw(10) << right << m_interestRate << "%" << endl
     << setw(24) << left << "Mortgage Principal:"
     << "$" << setw(10) << right << m_principalAmount << endl
     << setw(24) << left << "Monthly Payment:"
     << "$" << setw(10) << right << monthlyPayment
     << endl << endl;
}


