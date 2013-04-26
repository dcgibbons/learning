/*
 * Author's Name: David C. Gibbons
 * Assignment:    POS406 - Workshop 5
 * Creation Date: December 8, 2005
 * Due Date:      December 19, 2005
 */

import java.io.PrintStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.DecimalFormat;

/**
 * Class for the workshop 5 programming assignment.
 */
public class POS406_W5_DCG
{
    /**
     * main method - entry point for the entire program.
     */
    public static void main(String[] args)
        throws IOException // just let IOExceptions terminate the program
    {
        final PrintStream out = System.out;
        final BufferedReader in =
                new BufferedReader(new InputStreamReader(System.in));

        // display the header for this assignment
        out.println("Name            : David C. Gibbons   ");
        out.println("Assignment      : Workshop 5         ");
        out.println("-------------------------------------");
        out.println();
        out.println("Mortgage Payment Calculation         ");
        out.println();

        // create an array of Mortgage objects to capture the mortgages
        // we want to display information about - to add new mortgages
        // the array only needs to be made larger and new objects added
        Mortgage[] mortgages = new Mortgage[1];
        //mortgages[0] = new Mortgage(100000.00, 5.35, 7);
        //mortgages[1] = new Mortgage(200000.00, 5.50, 15);
        //mortgages[2] = new Mortgage(300000.00, 5.75, 30);
        mortgages[0] = new Mortgage(50000.00, 6.00, 15);

        // go through each mortgage, calculate the monthly payment and then
        // display the mortgage information
        for (int mortgageNo = 0; mortgageNo < mortgages.length; mortgageNo++)
        {
            mortgages[mortgageNo].compute();
            mortgages[mortgageNo].display(out, in);
        }
    }
}

/**
 * Class to represent an individual Mortgage.
 */
class Mortgage
{
    private static final int MONTHS_PER_YEAR = 12;

    private double principal;
    private double rate;
    private int term;
    private double monthlyRate;
    private int numPayments;
    private double monthlyPayment;

    /**
     * Constructs a new Mortgage object using a reasonable set of default
     * values for the principal, rate, and term properties. These can then be
     * overriden by the various setter methods.
     */
    public Mortgage()
    {
        setPrincipal(100000.00);
        setRate(8.5);
        setTerm(30);
    }

    /**
     * Constructs a new Mortgage object using the provided values.
     * @param principal the starting principal of the mortgage
     * @param rate the annual percentage rate of the mortgage, e.g. 5.75
     * @param term the length of the mortgage in years
     */
    public Mortgage(double principal, double rate, int term)
    {
        setPrincipal(principal);
        setRate(rate);
        setTerm(term);
    }

    /**
     * Sets the principal of the mortgage.
     * @param newPrincipal the new mortgage principal
     */
    public void setPrincipal(double newPrincipal)
    {
        principal = newPrincipal;
    }

    /**
     * Sets the annual percentage rate of the mortgage.
     * @param yearlyRate the annual percentage rate, e.g. 5.75
     */
    public void setRate(double yearlyRate)
    {
        rate = yearlyRate / 100.0; // convert to decimal form

        // determine interest rate per month
        monthlyRate = rate / MONTHS_PER_YEAR;
    }

    /**
     * Sets the term of the mortgage.
     * @param termInYears the length of the mortgage in years
     */
    public void setTerm(int termInYears)
    {
        term = termInYears;

        // determine total number of monthly payments for the given term
        numPayments = term * MONTHS_PER_YEAR;
    }

    /**
     * Calculates the monthly payment of the mortgage.
     */
    public void compute()
    {
        // calculate monthly payment
        monthlyPayment = principal *
            (monthlyRate / (1 - Math.pow(1 + monthlyRate, -numPayments)));
    }


    /**
     * Displays detailed information about the mortgage, including a payment
     * history breakdown.
     * @param out the output object
     * @param in the interactive input object
     * @throws IOException if any I/O error occurs
     */
    public void display(final PrintStream out, final BufferedReader in)
        throws IOException
    {
        // fetch needed decimal formatters needed for output
        final DecimalFormat numberFmt = new DecimalFormat("#0");
        final DecimalFormat currencyFmt = new DecimalFormat("$0.00");
        final DecimalFormat percentFmt = new DecimalFormat("0.00 %");

        // display overall loan information
        out.println("Principal       : " + currencyFmt.format(principal));
        out.println("Interest Rate   : " + percentFmt.format(rate));
        out.println("# of payments   : " + numberFmt.format(numPayments));
        out.println("Monthly payment : " + currencyFmt.format(monthlyPayment));
        out.println();

        // wait for the user to continue and then display the
        // details of all the payments
        out.println("Hit Enter to list Payment Detail");
        in.readLine();

        // calculate the payment detail in yearly blocks
        double currentPrincipal = principal;
        double totalInterest = 0.0;
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
            totalInterest += interestPayment;

            // always get the absolute value of the current principal as
            // sometimes the final value of 0 can be -0.0
            currentPrincipal = Math.abs(currentPrincipal - principalPayment);

            // format the fields and display to match benchmark
            out.print(numberFmt.format(month + 1) + " \t");
            out.print(currencyFmt.format(interestPayment) + "\t  ");
            out.print(currencyFmt.format(principalPayment) + "\t");
            out.println(currencyFmt.format(currentPrincipal));
        }

        out.println();
        out.println("Total interest paid: " + currencyFmt.format(totalInterest));

        // add some blank lines to the end of this report
        out.println();
        out.println();
    }
}
