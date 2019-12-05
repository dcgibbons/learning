/********************************************************************
 Program Name: gibbons_week4.cpp

 Student Name: David C. Gibbons

 Date: November 9, 2007

 Learning Team: A

 Course: POS/441

 Program Description: Change Request # 14
   Write the program as an object-oriented C++ program that allows the user 
   to select which way they want to calculate a mortgage: by input of the 
   amount of the mortgage, the term of the mortgage, and the interest rate 
   of the mortgage payment or by input of the amount of a mortgage and then 
   select from a menu of mortgage loans:

     - 7 year at 5.35%
     - 15 year at 5.5%
     - 30 year at 5.75%.

   In either case, display the mortgage payment amount. Then, list the loan 
   balance and interest paid for each payment over the term of the loan. On 
   longer term loans, the list will scroll off the screen. Do not allow the 
   list to scroll off the screen, but rather display a partial list and then 
   allow the user to continue the list. Allow the user to loop back and enter 
   a new amount and make a new selection, or quit. Insert comments in the 
   program to document the program.

 Program Description: Change Request # 13
   Write the program as an object-oriented C++ program that allows the user 
   to input the amount of a mortgage and then select from a menu of mortgage 
   loans:

     - 7 year at 5.35%
     - 15 year at 5.5%
     - 30 year at 5.75%.

   Use an array for the different loans. Display the mortgage payment amount. 
   Then, list the loan balance and interest paid for each payment over the 
   term of the loan. On longer term loans, the list will scroll off the screen. 
   Do not allow the list to scroll off the screen, but rather display a partial 
   list and then allow the user to continue the list. Allow the user to loop 
   back and enter a new amount and make a new selection, or quit. Insert 
   comments in the program to document the program.

 Program Description: Change Request # 12
   Write the program as an object-oriented C++ program that calculates and 
   displays the mortgage payment amount from user input of the amount of 
   the mortgage, the term of the mortgage, and the interest rate of the 
   mortgage. Allow the user to loop back and enter new data or quit. Insert
   comments in the program to document the program.

 **********************************************************************/

#include <cmath>
#include <exception>
#include <stdexcept>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

#include "gibbons_week4.h"

using namespace std;

int main()
{
  // display a welcome banner
  cout << "\tMortgage Calculator - Week 4" << endl
       << "\tDavid C. Gibbons" << endl
       << "\tPRG/411 - C++ Programming II" << endl
       << endl;

  // calculate mortgages until the user has decided to quit
  MortgageCalculator calculator;
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
 Member Name: Mortgage::MONTHS_IN_YEAR

 Member Description: constant for the number of months in a year

 **********************************************************************/
const int Mortgage::MONTHS_IN_YEAR = 12;

/********************************************************************
 Function Name: Mortgage::Mortgage

 Function Description: constructor for Mortgage class

 **********************************************************************/
Mortgage::Mortgage(const double interestRate, const int term, const double principalAmount)
  : m_interestRate(interestRate / 100.0), // convert to a percentage
    m_numPayments(term * MONTHS_IN_YEAR),
    m_principalAmount(principalAmount),
    m_monthlyPayment(calculateMonthlyPayment())
{
  const double monthlyRate = m_interestRate / MONTHS_IN_YEAR;
  double currentPrincipal = m_principalAmount;

  m_interestPayments = new double[m_numPayments];
  m_principalPayments = new double[m_numPayments];
  m_remainingBalances = new double[m_numPayments];

  // calculate the payment breakdown and remaining balance for each payment
  for (int month = 0; month < m_numPayments; month++)
  {
    const double interestPayment = monthlyRate * currentPrincipal;
    const double principalPayment = m_monthlyPayment - interestPayment;
    currentPrincipal = abs(currentPrincipal - principalPayment);

    // save the calculated values for later
    m_interestPayments[month] = interestPayment;
    m_principalPayments[month] = principalPayment;
    m_remainingBalances[month] = currentPrincipal;
  }
}

/********************************************************************
 Function Name: Mortgage::~Mortgage

 Function Description: destructor for Mortgage class

 **********************************************************************/
Mortgage::~Mortgage()
{
  delete[] m_interestPayments;
  delete[] m_principalPayments;
  delete[] m_remainingBalances;
}

/********************************************************************
 Function Name: Mortgage::getPrincipalAmount

 Function Description: returns the principal amount of this mortgage

 **********************************************************************/
double Mortgage::getPrincipalAmount() const
{
  return m_principalAmount;
}

/********************************************************************
 Function Name: Mortgage::getInterestRate

 Function Description: returns the interest rate of this mortgage

 **********************************************************************/
double Mortgage::getInterestRate() const
{
  return m_interestRate * 100.0;
}

/********************************************************************
 Function Name: Mortgage::getTerm

 Function Description: returns the term of this mortgage

 **********************************************************************/
int Mortgage::getTerm() const
{
  return m_numPayments / MONTHS_IN_YEAR;
}

/********************************************************************
 Function Name: Mortgage::getNumberOfPayments

 Function Description: returns total number of payments of this mortgage

 **********************************************************************/
int Mortgage::getNumberOfPayments() const
{
  return m_numPayments;
}

/********************************************************************
 Function Name: Mortgage::getInterestPayment

 Function Description: returns the interest payment for the specific 
                       month of the mortgage

 **********************************************************************/
double Mortgage::getInterestPayment(const int month) const 
  throw(invalid_argument) 
{
  if (month < 1 || month > m_numPayments)
  {
    throw invalid_argument("month out of range");
  }
  else
  {
    return m_interestPayments[month - 1];
  }
}

/********************************************************************
 Function Name: Mortgage::getPrincipalPayment

 Function Description: returns the principal payment for the specific
                       month of the mortgage

 **********************************************************************/
double Mortgage::getPrincipalPayment(const int month) const 
  throw(invalid_argument) 
{
  if (month < 1 || month > m_numPayments)
  {
    throw invalid_argument("month out of range");
  }
  else
  {
    return m_principalPayments[month - 1];
  }
}

/********************************************************************
 Function Name: Mortgage::getRemainingBalance

 Function Description: returns the remaining balance for the specific 

 **********************************************************************/
double Mortgage::getRemainingBalance(const int month) const 
  throw(invalid_argument) 
{
  if (month < 1 || month > m_numPayments)
  {
    throw invalid_argument("month out of range");
  }
  else
  {
    return m_remainingBalances[month - 1];
  }
}

/********************************************************************
 Function Name: Mortgage::getMonthlyPayment

 Function Description: returns the monthly payment for this mortgage

 **********************************************************************/
double Mortgage::getMonthlyPayment() const
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
 Member Name: MortgageCalculator::MAX_MORTGAGES

 Member Description: constant for the maximum number of predefined
                     mortgages available

 **********************************************************************/
const int MortgageCalculator::MAX_MORTGAGES = 3;

/********************************************************************
 Member Name: MortgageCalculator::MORTGAGE_CHOICES

 Member Description: constant for predefined mortgages available that
                     the user may choose from

 **********************************************************************/
const Mortgage MortgageCalculator::MORTGAGE_CHOICES[MAX_MORTGAGES] =
{
    Mortgage(5.35, 7),
    Mortgage(5.50, 15),
    Mortgage(5.75, 30)
};

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
  const bool timeToQuit = getValue(answer, false) 
    && (answer == 'N' || answer == 'n');

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

  // display the list of predefined mortgages as a menu list
  cout << endl << "Available Mortgages:" << endl;
  cout << "\t0) Custom Terms" << endl;
  for (int mortgage = 0; mortgage < MAX_MORTGAGES; mortgage++)
  {
    cout << "\t" << (mortgage + 1) << ") "
         << MORTGAGE_CHOICES[mortgage].getTerm() << " years @ "
         << fixed << setprecision(2)
         << MORTGAGE_CHOICES[mortgage].getInterestRate() << "%"
         << endl;
  }
  cout << endl << "Pick a Mortgage: " << flush;

  // get the mortgage choice from the user
  int mortgage;
  if (!getValue(mortgage, true) || mortgage < 0 || mortgage > MAX_MORTGAGES)
  {
    cout << "Invalid Mortgage Choice" << endl;
    return false;
  }

  double rate;
  int term;

  // if the user wants a custom mortgage then get further inputs
  if (mortgage == 0)
  {
    cout << "Interest Rate: " << flush;
    if (!getValue(rate, true) || rate < 0.0 || rate > 100.0)
    {
      cout << "Invalid Interest Rate" << endl;
      return false;
    }

    cout << "Mortgage Term (Years): " << flush;
    if (!getValue(term, true) || term < 1 || term > 50)
    {
      cout << "Invalid Mortgage Term" << endl;
      return false;
    }
  }
  else
  {
    rate = MORTGAGE_CHOICES[mortgage - 1].getInterestRate();
    term = MORTGAGE_CHOICES[mortgage - 1].getTerm();
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
  Mortgage mortgage = Mortgage(m_interestRate, m_term, m_principalAmount);
  const double monthlyPayment = mortgage.getMonthlyPayment();
  const int numberOfPayments = mortgage.getNumberOfPayments();

  // display a nice report to the user
  cout << endl
       << fixed << setprecision(2) // all numbers as fixed-point with 2 digits
       << setw(24) << left << "Term in Years:"
       << " " << setw(7) << right << m_term << endl
       << setw(24) << left << "Interest Rate:"
       << " " << setw(10) << right << m_interestRate << "%" << endl
       << setw(24) << left << "Mortgage Principal:"
       << "$" << setw(10) << right << m_principalAmount << endl
       << setw(24) << left << "Monthly Payment:"
       << "$" << setw(10) << right << monthlyPayment << endl
       << endl;

  // print a payment breakdown by month
  printBreakdownHeader(1, monthlyPayment);
  bool breakoverFound = false;
  bool printBreakover = false;
  for (int month = 1; month <= numberOfPayments; month++)
  {
    const double interestPayment = mortgage.getInterestPayment(month);
    const double principalPayment = mortgage.getPrincipalPayment(month);
    const double remainingBalance = mortgage.getRemainingBalance(month);

    // determine if we can print the breakover point yet
    if (!breakoverFound && interestPayment < principalPayment)
    {
      breakoverFound = true;
      printBreakover = true;
    }
  
    cout << setw(10) << right << month
         << " "
         << "$" << setw(10) << right << interestPayment;

    // print an indicator for the first payment where the interest is
    // less than the principal
    if (printBreakover)
    {
      cout << "*";
      printBreakover = false;
    }
    else
    {
      cout << " ";
    }

    cout << "$" << setw(10) << right << principalPayment
         << " "
         << "$" << setw(10) << right << remainingBalance
         << endl;

    // pause and print a new header every year
    if ((month % Mortgage::MONTHS_IN_YEAR) == 0 && month < numberOfPayments)
    {
      pausePrompt();
      printBreakdownHeader(month / Mortgage::MONTHS_IN_YEAR + 1, monthlyPayment);
    }
  }
}

/********************************************************************
 Function Name: MortgageCalculator::printBreakdownHeader

 Function Description: prints a breakdown header for each year

 **********************************************************************/
void MortgageCalculator::printBreakdownHeader(const int year, const double amount)
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
