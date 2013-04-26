/*
 * Mortgage.java
 * Week 2 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-12 | Initial version.
 */

/**
 * This class performs payment calculations for a mortgage loan.
 */
public class Mortgage {
    private static final int MONTHS_PER_YEAR = 12;

    private double principal;
    private double monthlyRate;
    private int numPayments;
    private double monthlyPayment;

    /**
     * Constructs a new Mortgage object using the provided values.
     *
     * @param principal the starting principal of the mortgage
     * @param rate      the annual percentage rate of the mortgage, e.g. 5.75
     * @param term      the length of the mortgage in years
     */
    public Mortgage(final double principal, final double rate, final int term) {
        setPrincipal(principal);
        setRate(rate);
        setTerm(term);
        calculatePayment();
    }

    /**
     * Sets the principal of the mortgage.
     *
     * @param newPrincipal the new mortgage principal
     */
    public void setPrincipal(final double newPrincipal) {
        principal = newPrincipal;
    }

    /**
     * Sets the annual percentage rate of the mortgage.
     *
     * @param yearlyRate the annual percentage rate, e.g. 5.75
     */
    public void setRate(final double yearlyRate) {
        // determine interest rate per month
        monthlyRate = yearlyRate / 100.0 / MONTHS_PER_YEAR;
    }

    /**
     * Sets the term of the mortgage.
     *
     * @param termInYears the length of the mortgage in years
     */
    public void setTerm(final int termInYears) {
        // determine total number of monthly payments for the given term
        numPayments = termInYears * MONTHS_PER_YEAR;
    }

    public double getPayment() {
        return monthlyPayment;
    }

    /**
     * Calculates the monthly payment of the mortgage.
     */
    protected void calculatePayment() {
        // calculate monthly payment
        monthlyPayment = principal *
                (monthlyRate / (1 - Math.pow(1 + monthlyRate, -numPayments)));
    }
}
