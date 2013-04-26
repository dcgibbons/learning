/*
 * Author's Name: David C. Gibbons
 * Assignment:    POS406 - Workshop 3
 * Creation Date: November 29, 2005
 * Due Date:      December 5, 2005
 */

import java.io.*;
import java.text.*;
import java.util.*;

/**
 * Class for the workshop 3 programming assignment.
 */
public class POS406_W3_DCG 
{
    private static final double PRINCIPAL = 200000.00;
    private static final double RATE = 5.75;
    private static final int TERM = 30;
    private static final int MONTHS_PER_YEAR = 12;

    /**
     * main method - entry point for the entire program
     */
    public static void main(String[] args) 
        throws IOException // just let IOExceptions terminate the program
    {
        // initialize loan variables from constants
        final double principal = PRINCIPAL;
        final double rate = RATE / 100; // convert rate into decimal form
        final double numPayments = TERM * MONTHS_PER_YEAR;

        // create output and input objects
        final PrintStream out = System.out;
        final BufferedReader in = 
            new BufferedReader(new InputStreamReader(System.in));

        // display the header for this assignment
        out.println("Name            : David C. Gibbons   ");
        out.println("Assignment      : Workshop 3         ");
        out.println("-------------------------------------");

        // determine interest rate per month
        final double monthlyRate = (rate / MONTHS_PER_YEAR); 

        // calculate monthly payment
        final double monthlyPayment = principal * 
            (monthlyRate / (1 - Math.pow(1 + monthlyRate, -numPayments)));

        // fetch needed decimal formatters needed for output
        final DecimalFormat numberFmt = new DecimalFormat("#0");
        final DecimalFormat currencyFmt = new DecimalFormat("$0.00");
        final DecimalFormat percentFmt = new DecimalFormat("0.00 %");

        // display overall loan information
        out.println("Principal       : " + currencyFmt.format(principal));
        out.println("Interest Rate   : " + percentFmt.format(rate));
        out.println("# of payments   : " + numberFmt.format(numPayments));
        out.println("Monthly payment : " + currencyFmt.format(monthlyPayment));

        // wait for the user to continue and then display the
        // details of all the payments
        out.println();
        out.println("Hit Enter to list Payment Detail");
        in.readLine();

        // calculate the payment detail in yearly blocks
        double currentPrincipal = principal;
        for (int month = 0; month < numPayments; month++) {
            // display header to match the benchmark at the start of the year
            if (month == 0 || (month % MONTHS_PER_YEAR) == 0) {
                // display a pause prompt if this isn't the first year
                if (month > 0)
                {
                    out.println();
                    out.println(" Hit Enter to continue");
                    in.readLine();
                }
                out.println();
                out.println("\tInterest  Principal     Principal");
                out.println("Months\tPayment   Payment       Balance");
                out.println();
            }

            // calculate this month's interest payment and then the
            // principal payment and remaining principal balance
            double interestPayment = rate / MONTHS_PER_YEAR * currentPrincipal;
            double principalPayment = monthlyPayment - interestPayment;

            // always get the absolute value of the current principal as
            // sometimes the final value of 0 can be -0.0
            currentPrincipal = Math.abs(currentPrincipal - principalPayment);

            // format the fields and display to match benchmark
            out.print(numberFmt.format(month + 1) + " \t"); 
            out.print(currencyFmt.format(interestPayment) + "\t  ");
            out.print(currencyFmt.format(principalPayment) + "\t");
            out.println(currencyFmt.format(currentPrincipal));
        }
    }
}

