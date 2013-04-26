/*
 * Author's Name: David C. Gibbons
 * Assignment:    POS406 - Workshop 4A
 * Creation Date: November 29, 2005
 * Due Date:      December 12, 2005
 */

import java.io.PrintStream;
import java.text.DecimalFormat;

/**
 * Class for the workshop 4A programming assignment.
 */
public class POS406_W4A_DCG
{
    private static final int MONTHS_PER_YEAR = 12;

    /**
     * main method - entry point for the entire program.
     */
    public static void main(String[] args)
    {
        // create output and input objects
        final PrintStream out = System.out;

        // display the header for this assignment
        out.println("Name            : David C. Gibbons   ");
        out.println("Assignment      : Workshop 4A        ");
        out.println("-------------------------------------");
        out.println();
        out.println("Mortgage Payment Calculation         ");
        out.println();

        // fetch needed decimal formatters needed for output
        final DecimalFormat numberFmt = new DecimalFormat("#0");
        final DecimalFormat currencyFmt = new DecimalFormat("$0.00");
        final DecimalFormat percentFmt = new DecimalFormat("0.00 %");

        // create the arrays to represent our mortgages that must be
        // calculated - to add additional mortgages only these arrays
        // need change and the remaining code can remain as-is
        final double[] principals = { 100000.00, 200000.00, 300000.00 };
        final double[] rates = { 5.35, 5.50, 5.75 };
        final double[] terms = { 7, 15, 30 };

        // iterate over each mortgage, calculate the monthly payment, and
        // then display the mortgage information to the user
        for (int mortgage = 0; mortgage < principals.length; mortgage++)
        {
            // initialize loan variables from constants
            final double principal = principals[mortgage];
            final double numPayments = terms[mortgage] * MONTHS_PER_YEAR;
            final double rate = rates[mortgage] / 100; // convert to decimal

            // determine interest rate per month
            final double monthlyRate = (rate / MONTHS_PER_YEAR);

            // calculate monthly payment
            final double monthlyPayment = principal *
                (monthlyRate / (1 - Math.pow(1 + monthlyRate, -numPayments)));

            // display overall loan information
            out.println("Principal       : " + currencyFmt.format(principal));
            out.println("Interest Rate   : " + percentFmt.format(rate));
            out.println("# of payments   : " + numberFmt.format(numPayments));
            out.println("Monthly payment : " + currencyFmt.format(monthlyPayment));
            out.println();
        }
    }
}

