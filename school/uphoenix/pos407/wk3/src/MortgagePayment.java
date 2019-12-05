/*
 * MortgagePayment.java
 * Week 3 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-17 | Initial version.
 */

/**
 * This class represents a payment to a mortgage. The payment is broken down
 * into its interest and principal components, along with containing an update
 * to what the remaining balance of the mortgage is after the payment has been
 * applied.
 */
public class MortgagePayment {
    private Double payment;
    private Double principal;
    private Double interest;
    private Double remainingBalance;

    public Double getPayment() {
        return payment;
    }

    public void setPayment(Double payment) {
        this.payment = payment;
    }

    public Double getPrincipal() {
        return principal;
    }

    public void setPrincipal(Double principal) {
        this.principal = principal;
    }

    public Double getInterest() {
        return interest;
    }

    public void setInterest(Double interest) {
        this.interest = interest;
    }

    public Double getRemainingBalance() {
        return remainingBalance;
    }

    public void setRemainingBalance(Double remainingBalance) {
        this.remainingBalance = remainingBalance;
    }
}
