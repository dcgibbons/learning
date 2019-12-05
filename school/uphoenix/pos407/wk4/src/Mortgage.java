/*
 * Mortgage.java
 * Week 4 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-12 | Initial version.
 *   2.00  | 2006-01-17 | Added support necessary for week 3 assignment: payment
 *                      | schedule is now calculated; toString method added,
 *                      | copy/clone constructor added; getter methods added.
 */

import java.text.MessageFormat;

/**
 * This class performs payment calculations for a mortgage loan.
 */
public class Mortgage implements Cloneable {
    private static final int MONTHS_PER_YEAR = 12;

    private double principal;
    private double apr;
    private double monthlyRate;
    private int term;
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
    }

    /**
     * Constructs a new Mortgage object using an existing Mortgage as a
     * template.
     * @param oldMortgage the existing Mortgage object who's values will be
     *                    copied into the new instance.
     */
    public Mortgage(final Mortgage oldMortgage) {
        setPrincipal(oldMortgage.getPrincipal());
        setRate(oldMortgage.getRate());
        setTerm(oldMortgage.getTerm());
    }

    /**
     * Produces a friendly string version of the Mortgage Terms.
     * @return the mortgage terms
     */
    public String toString() {
        final String fmt = "{0,number,#.00%} for {1,number,integer} Years";
        final Object[] msgArgs = { new Double(apr), new Integer(term) };
        return MessageFormat.format(fmt, msgArgs);
    }

    /**
     * Retrieves the principal property of the mortgage.
     * @return the current principal value
     */
    public double getPrincipal() {
        return principal;
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
     * Retrieves the annual percentage rate of the mortgage.
     * @return the current annual percentage rate of the mortgage
     */
    public double getRate() {
        return apr * 100.0;
    }

    /**
     * Sets the annual percentage rate of the mortgage.
     *
     * @param yearlyRate the annual percentage rate, e.g. 5.75
     */
    public void setRate(final double yearlyRate) {
        apr = yearlyRate / 100.00;
        // determine interest rate per month
        monthlyRate = apr / MONTHS_PER_YEAR;
    }

    /**
     * Retrieves the current mortgage term.
     * @return the current term of the mortgage
     */
    public int getTerm() {
        return term;
    }

    /**
     * Sets the term of the mortgage.
     *
     * @param termInYears the length of the mortgage in years
     */
    public void setTerm(final int termInYears) {
        this.term = termInYears;
        // determine total number of monthly payments for the given term
        numPayments = termInYears * MONTHS_PER_YEAR;
    }

    /**
     * Retrieves the calculated monthly payment of the mortgage.
     * @return the monthly payment of the mortgage using the current terms
     */
    public double getPayment() {
        calculatePayment();
        return monthlyPayment;
    }

    /**
     * Retrieves an array of mortgage payment information for the entire life
     * of the mortgage.
     * @return an array of payment objects to breakdown the entire detail of the
     *         mortgage payment schedule
     */
    public MortgagePayment[] getPayments() {
        // create an array sized for the entire number of payments for this
        // mortgage
        final MortgagePayment[] payments = new MortgagePayment[numPayments];

        // calculate the monthly payment based on the current values
        calculatePayment();

        // determine the interest payment, principal payment, and remaining
        // balance at each payment interval
        double currentPrincipal = principal;
        for (int month = 0; month < numPayments; month++) {

            // calculate this month's interest payment and then the
            // principal payment and remaining principal balance
            double interestPayment = monthlyRate * currentPrincipal;
            double principalPayment = monthlyPayment - interestPayment;

            // always get the absolute value of the current principal as
            // sometimes the final value of 0 can be -0.0
            currentPrincipal = Math.abs(currentPrincipal - principalPayment);

            final MortgagePayment payment = new MortgagePayment();
            payment.setPayment(new Double(monthlyPayment));
            payment.setInterest(new Double(interestPayment));
            payment.setPrincipal(new Double(principalPayment));
            payment.setRemainingBalance(new Double(currentPrincipal));
            payments[month] = payment;
        }

        return payments;
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
