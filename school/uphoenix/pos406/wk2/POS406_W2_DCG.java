/*
 * POS406_W2_DCG.java
 * Week 2 Programming Assignment
 * POS/406 - Computer Programming I
 * David C. Gibbons, dcgibbons@email.uophx.edu
 * November 21, 2005
 */

import java.io.PrintStream;
import java.text.DecimalFormat;
import java.text.MessageFormat;

/**
 * Class for the week 3 programming assignment.
 */
public class POS406_W2_DCG {
    private static final String STUDENT_NAME = "Chad Gibbons";
    private static final String ASSIGNMENT_NAME = "Workshop 2";
    private static final double PRINCIPAL = 200000.00;
    private static final double RATE = 5.75;
    private static final int TERM = 30;

    public static void main(String[] args) {
        final PrintStream out = System.out;

        // display the header for this assignment
        out.println("Name            : " + STUDENT_NAME);
        out.println("Assignment      : " + ASSIGNMENT_NAME);
        out.println("------------------------------------");

        // create a new MortgageCalculator instance with the information
        // we want to use in this workshop assignment
        MortgageCalculator mortgageCalc = 
            new MortgageCalculator(PRINCIPAL, RATE, TERM);

        // display the mortgage information to the standard output
        mortgageCalc.displayInfo(System.out);
    }
}

/**
 * Class that provides mortgage calculation information and output.
 */
class MortgageCalculator {
    private final double principal;
    private final double rate;
    private final int numberOfPayments;

    /**
     * Constructs a new instance of the MortgageCalculator class.
     * @param principal the initial principal amount of the mortgage
     * @param rate the interest rate of the mortgage, i.e. 6.5%
     * @param term the length of the mortgage in years
     */
    public MortgageCalculator(double principal, double rate, int term) {
        this.principal = principal;
        this.rate = rate / 100; // convert percentage to decimal form
        this.numberOfPayments = term * 12; // convert term to number of months
    }

    /**
     * Displays the mortgage information to the specified print stream.
     * @param out the stream output should be displayed on
     */
    public void displayInfo(PrintStream out) {
        double monthlyPayment = calculatePayment();
        displayMortgage(out, monthlyPayment);
    }

    /**
     * Calculate the mortgage payment using a derived formula
     * obtained from http://www.hughchou.org/calc/formula.html;
     * variables used match given formula
     */
    private double calculatePayment() {
        final double p = principal;             // initial amount of loan
        final double i = rate;                  // annual interest rate
        final double j = (i / 12);              // monthly interest rate
        final double n = numberOfPayments;      // amortized loan length
        
        // calculate monthly payment and return to caller
        final double m = p * (j / (1 - Math.pow(1 + j, -n)));

        return m;
    }

    /**
     * Displays the details of the mortgage to the specified print
     * stream.
     * @param out the stream output should be displayed on
     */
    private void displayMortgage(PrintStream out, double monthlyPayment) {
        DecimalFormat fmt = new DecimalFormat("$###,##0.00");
        out.println("Principal       : " + fmt.format(principal));

        fmt = new DecimalFormat("#0.00%");
        out.println("Interest Rate   : " + fmt.format(rate));

        fmt = new DecimalFormat("###0");
        out.println("# of payments   : " + fmt.format(numberOfPayments));

        fmt = new DecimalFormat("$###,##0.00");
        out.println("Monthly Payment : " + fmt.format(monthlyPayment));
    }
}

