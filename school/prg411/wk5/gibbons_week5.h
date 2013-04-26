/********************************************************************
 Class Name: Mortgage

 Class Description: An amoritized mortgage

 Class Members:
                  Public:
                    MONTHS_IN_YEAR
                    Mortgage()
                    ~Mortgage()
                    
                    getPrincipalAmount()
                    getInterestRate()
                    getTerm()
                    getNumberOfPayments()

                    getMonthlyPayment()

                    getInterestPayment()
                    getPrincipalPayment()
                    getRemainingBalance()

                  Private:
                    m_interestRate
                    m_numPayments
                    m_principalAmount
                    m_monthlyPayment
                    m_interestPayments
                    m_principalPayments
                    m_remainingBalances

                  Protected:
                    calculateMonthlyPayment

**********************************************************************/
class Mortgage
{
public:  
  static const int MONTHS_IN_YEAR;

  Mortgage(const double interestRate, 
           const int term, 
           const double principalAmount = 0.0);
  ~Mortgage();
  
  double getPrincipalAmount() const;
  double getInterestRate() const;
  int getTerm() const;
  int getNumberOfPayments() const;

  double getMonthlyPayment() const;

  double getInterestPayment(const int month) 
    const throw(std::invalid_argument);
    
  double getPrincipalPayment(const int month) 
    const throw(std::invalid_argument);

  double getRemainingBalance(const int month) 
    const throw(std::invalid_argument);
  
protected:
  const double calculateMonthlyPayment();

private:
  double m_interestRate;
  int m_numPayments;
  double m_principalAmount;
  double m_monthlyPayment;
  double* m_interestPayments;
  double* m_principalPayments;
  double* m_remainingBalances;
};

/********************************************************************
 Class Name: MortgageCalculator

 Class Description: A Mortgage Calculator

 Class Members:
                  Public:
                    MortgageCalculator();
                    timeToQuit()
                    getMortgageTerms()
                    reportMortgageDetails()

                  Private:
                    MAX_MORTGAGES
                    MORTGAGE_CHOICES
                    m_principalAmount
                    m_interestRate
                    m_term

                  Protected:
                    getValue()
                    pausePrompt()
                    printBreakdownHeader()

**********************************************************************/
class MortgageCalculator
{
public:
  MortgageCalculator();
  ~MortgageCalculator();
  const bool timeToQuit();
  const bool getMortgageTerms();
  void reportMortgageDetails();

protected:
  void loadInterestRates();
  void trim(string& str);
  template <class T> const bool getValue(T& v, const bool numericOnly);
  const void pausePrompt();
  void printBreakdownHeader(const int month, const double paymentAmount);

private:
  static const int MAX_MORTGAGES;
  static const Mortgage* MORTGAGE_CHOICES[];
  
  double m_principalAmount;
  double m_interestRate;
  int m_term;
};
