/********************************************************************
 Class Name: Mortgage

 Class Description: An amoritized mortgage

 Class Members:
                  Public:
                    Mortgage()
                    getMonthlyPayment()

                  Private:
                    MONTHS_IN_YEAR
                    m_principalAmount
                    m_interestRate
                    m_numPayments
                    m_monthlyPayment

                  Protected:
                    calculateMonthlyPayment

**********************************************************************/
class Mortgage
{
public:
  Mortgage(double principalAmount, double interestRate, int term);
  double getMonthlyPayment();

protected:
  const double calculateMonthlyPayment();

private:
  const int MONTHS_IN_YEAR;
  const double m_principalAmount;
  const double m_interestRate;
  const int m_numPayments;
  const double m_monthlyPayment;
};


/********************************************************************
 Class Name: MortgageCalculator

 Class Description: A Mortgage Calculator

 Class Members:
                  Public:
                    timeToQuit()
                    getMortgageTerms()
                    reportMortgageDetails()

                  Private:
                    m_principalAmount
                    m_interestRate
                    m_term

                  Protected:
                    getValue()
                    pausePrompt()

**********************************************************************/
class MortgageCalculator
{
public:
  const bool timeToQuit();
  const bool getMortgageTerms();
  void reportMortgageDetails();

protected:
  template <class T> const bool getValue(T& v, const bool numericOnly);
  const void pausePrompt();

private:
  double m_principalAmount;
  double m_interestRate;
  int m_term;
};

