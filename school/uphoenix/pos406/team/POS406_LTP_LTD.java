/*
 * Authors' Names: Jon Barret, Jeff Cuff, David C. Gibbons
 * Assignment:    POS406 - Learning Team D Project
 * Creation Date: November 21, 2005
 * Due Date:      December 19, 2005
 */

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.text.DecimalFormat;
import java.util.Arrays;

/**
 * Main class for the Learning Team D learning-team project.
 */
public class POS406_LTP_LTD {
    private static final int MAX_CUSTOMERS = 3;
    private static final int MAX_ACCOUNTS = 3;

    /**
     * The following constants define the various menu option letters available
     * to the user.
     */
    private static final char MENU_OPTION_INVALID = ' ';
    private static final char MENU_OPTION_EXIT = 'Q';
    private static final char MENU_OPTION_CHANGE_CUSTOMER_ID = 'I';
    private static final char MENU_OPTION_CHANGE_ACCOUNT_ID = 'N';
    private static final char MENU_OPTION_ASCENDING_TRANS_LIST = 'A';
    private static final char MENU_OPTION_DESCENDING_TRANS_LIST = 'D';
    private static final char MENU_OPTION_TRANS_REPORT = 'T';
    private static final char MENU_OPTION_ACCOUNT_STATISTICS = 'S';

    /**
     * Main entry point for this assignment. No arguments are required and
     * any arguments specified are ignored.
     */
    public static void main(final String[] args) throws IOException {
        // reference the system output and wrap the system input
        final PrintStream out = System.out;
        final BufferedReader in =
                new BufferedReader(new InputStreamReader(System.in));

        // create and populate our customer objects with data
        Customer[] customers = createCustomers();

        // start out with the first customer and first account by default
        int customerId = 0;
        int accountId = 0;

        // display the menu in a loop until the user wishes to exit
        char menuSelection = MENU_OPTION_INVALID;
        do {
            displayHeader(out, customers[customerId], accountId);
            displayMenu(out);

            // retrieve the input line from the user and determine their
            // selection only if some characters were actually input
            String line = in.readLine();
            if (line != null && line.length() > 0) {
                menuSelection = line.toUpperCase().charAt(0);
                switch (menuSelection) {
                case MENU_OPTION_CHANGE_CUSTOMER_ID:
                    customerId = changeCustomerId(out,
                                                  in,
                                                  customerId);
                    break;

                case MENU_OPTION_CHANGE_ACCOUNT_ID:
                    accountId = changeAccountId(out,
                                                in,
                                                accountId);
                    break;

                case MENU_OPTION_ASCENDING_TRANS_LIST:
                    customers[customerId].getAccounts()[accountId].DisplayTransactions(out, true);
                    break;

                case MENU_OPTION_DESCENDING_TRANS_LIST:
                    customers[customerId].getAccounts()[accountId].DisplayTransactions(out, false);
                    break;

                case MENU_OPTION_TRANS_REPORT:
                    customers[customerId].getAccounts()[accountId].DisplayTransactionReport(out);
                    break;

                case MENU_OPTION_ACCOUNT_STATISTICS:
                    customers[customerId].getAccounts()[accountId].GetAccountStats(out);
                    break;

                case MENU_OPTION_EXIT:
                    break;

                    // any other choice is invalid
                default:
                    menuSelection = MENU_OPTION_INVALID;
                    break;
                }
            }

            // ask the user to press enter if the exit option was not selected
            if (menuSelection != MENU_OPTION_EXIT) {
                pausePrompt(out, in);
            }
        }
        while (menuSelection != MENU_OPTION_EXIT);
    }

    /**
     * Creates an array of Customer objects; each Customer object will be
     * populated with various Account objects to match the project requirements.
     */
    private static Customer[] createCustomers() {
        final Customer[] customers = new Customer[MAX_CUSTOMERS];
        Account[] accounts;

        // create the first customer with 3 accounts
        customers[0] = new Customer();
        customers[0].setId(1);
        customers[0].setName("Sundar");
        accounts = new Account[MAX_ACCOUNTS];

        accounts[0] = new Account();
        accounts[0].setCarriedOverBalance(100);
        accounts[0].setBonus(1);
        accounts[0].setFine(2);
        accounts[0].setTransactions(new int[]{45, -50, 100, 45, -70});

        accounts[1] = new Account();
        accounts[1].setBonus(1);
        accounts[1].setFine(1);
        accounts[1].setCarriedOverBalance(-50);
        accounts[1].setTransactions(new int[]{45, 50, -100, 45, 70});

        accounts[2] = new Account();
        accounts[2].setBonus(1);
        accounts[2].setFine(0);
        accounts[2].setCarriedOverBalance(50);
        accounts[2].setTransactions(new int[]{-45, -50, 100, 45, 70});

        customers[0].setAccounts(accounts);

        // create the second customer with 3 accounts
        customers[1] = new Customer();
        customers[1].setId(2);
        customers[1].setName("Joe");

        accounts = new Account[MAX_ACCOUNTS];
        accounts[0] = new Account();
        accounts[0].setBonus(1);
        accounts[0].setFine(2);
        accounts[0].setCarriedOverBalance(30);
        accounts[0].setTransactions(new int[]{35, -40, 90, 35, -80});

        accounts[1] = new Account();
        accounts[1].setBonus(1);
        accounts[1].setFine(1);
        accounts[1].setCarriedOverBalance(40);
        accounts[1].setTransactions(new int[]{45, -50, -100, 45, 70});

        accounts[2] = new Account();
        accounts[2].setBonus(1);
        accounts[2].setFine(0);
        accounts[2].setCarriedOverBalance(50);
        accounts[2].setTransactions(new int[]{-40, -55, 105, 45, 75});

        customers[1].setAccounts(accounts);

        // create the third customer with 3 accounts
        customers[2] = new Customer();
        customers[2].setId(3);
        customers[2].setName("Sean");

        accounts = new Account[MAX_ACCOUNTS];
        accounts[0] = new Account();
        accounts[0].setBonus(1);
        accounts[0].setFine(2);
        accounts[0].setCarriedOverBalance(25);
        accounts[0].setTransactions(new int[]{45, -55, 110, 45, -70});

        accounts[1] = new Account();
        accounts[1].setBonus(1);
        accounts[1].setFine(1);
        accounts[1].setCarriedOverBalance(-55);
        accounts[1].setTransactions(new int[]{45, -55, -100, 65, 75});

        accounts[2] = new Account();
        accounts[2].setBonus(1);
        accounts[2].setFine(0);
        accounts[2].setCarriedOverBalance(40);
        accounts[2].setTransactions(new int[]{-40, -60, 80, 35, 60});

        customers[2].setAccounts(accounts);

        return customers;
    }

    /**
     * Displays a simple prompt and wait for the user to press enter to
     * continue.
     */
    private static void pausePrompt(final PrintStream out,
                                    final BufferedReader in)
            throws IOException {
        out.println();
        out.println("Hit Enter to continue");
        in.readLine(); // wait for a line of input
        out.println();
    }

    /**
     * Utility method to display a simple numeric list in a user-friendly
     * way and retrieve a user selection within that range.
     *
     * @param out the output stream to display the menu on
     * @param in  the input reader to receive input from
     * @return the new value the user selected, or -1 if invalid
     */
    private static int displayNumericList(final PrintStream out,
                                          final BufferedReader in)
            throws IOException {
        out.print("(1, 2, or 3) : ");
        out.flush();

        int newValue = -1;
        boolean invalid = true;
        String line = in.readLine();

        // determine if the user actually provided any data and if it is
        // a valid numeric value in the appropriate range
        if (line != null && line.length() > 0) {
            try {
                newValue = Integer.parseInt(line);
                invalid = (newValue < 1 || newValue > 3);
            }
            catch (NumberFormatException ex) {
                invalid = true;
            }
        } else {
            invalid = true;
        }

        if (invalid) {
            newValue = -1;
        }

        return newValue;
    }

    /**
     * Displays header information about the current customer and account.
     *
     * @param out             the output stream to display the menu on
     * @param currentCustomer the current customer to display information about
     * @param accountId       the current account Id to display information about
     */
    private static void displayHeader(final PrintStream out,
                                      final Customer currentCustomer,
                                      final int accountId) {

        final Account currentAccount = currentCustomer.getAccounts()[accountId];
        final DecimalFormat currencyFmt = new DecimalFormat("$#0.00");

        out.println("POS406 Team Assignment for LT D");
        out.println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
        out.println("Customer Name : " + currentCustomer.getName());
        out.println("Customer ID   : " + currentCustomer.getId());

        out.print("Bonus: " + currencyFmt.format(currentAccount.getBonus()));
        out.println(" Fine: " + currencyFmt.format(currentAccount.getFine()));
        out.println("Carried Over Balance: " + currencyFmt.format(
                currentAccount.getCarriedOverBalance()));

        // display the transactions for the current account
        final int[] transactions = currentAccount.getTransactions();
        out.print("Transactions for Account " + (accountId + 1) + " : [");
        for (int i = 0; i < transactions.length; i++) {
            out.print(" " + transactions[i] + " ");
        }
        out.println("]");

        out.println();
    }

    /**
     * Displays a menu of selections.
     *
     * @param out the output stream to display the menu on
     */
    private static void displayMenu(final PrintStream out) {
        out.println("Selection operation using assigned letter");
        out.println("-----------------------------------------");
        out.println("I) Change Customer ID");
        out.println("N) Change Account Number");
        out.println("A) Display Ascending Transactions List");
        out.println("D) Display Descending Transactions List");
        out.println("S) Display Account Statistics");
        out.println("T) Display Transaction Report");
        out.println("Q) Quit");
        out.println();
        out.print(" Selection is : ");
        out.flush();
    }

    /**
     * Changes the current customer Id.
     */
    private static int changeCustomerId(final PrintStream out,
                                        final BufferedReader in,
                                        final int currentCustomerId)
            throws IOException {
        out.println();
        out.print(" Enter New Customer ID ");

        int newCustomerId = displayNumericList(out, in);
        if (newCustomerId == -1) {
            out.println("Invalid Customer ID");
            newCustomerId = currentCustomerId;
        } else {
            out.println();
            out.println("Customer ID changed to " + newCustomerId);
            newCustomerId--; // convert to zero-based array index
        }

        return newCustomerId;
    }

    /**
     * Changes the current account Id.
     */
    private static int changeAccountId(final PrintStream out,
                                       final BufferedReader in,
                                       final int currentAccountId)
            throws IOException {
        out.println();
        out.print(" Enter New Account ");

        int newAccountId = displayNumericList(out, in);
        if (newAccountId == -1) {
            out.println("Invalid Account Number");
            newAccountId = currentAccountId;
        } else {
            out.println();
            out.println("Account Number changed to " + newAccountId);
            newAccountId--; // convert to zero-based array index
        }

        return newAccountId;
    }
}

/**
 * Customer class to represent a single bank customer. A customer can have
 * any number of accounts, including zero.
 */
class Customer {
    private int id;
    private String name;
    private Account[] accounts;

    /**
     * Constructs a new Customer instance with default attribute values.
     */
    public Customer() {
        id = -1;
        name = "";
        accounts = new Account[0];
    }

    /**
     * Retrieves the Id attribute of the current Customer object.
     *
     * @return the current id value
     */
    public int getId() {
        return id;
    }

    /**
     * Sets a new Id attribute for the current Customer object.
     *
     * @param newId the new id attribute value
     */
    public void setId(int newId) {
        id = newId;
    }

    /**
     * Retrieves the Name attribute of the current Customer object.
     *
     * @return the current name value
     */
    public String getName() {
        return name;
    }

    /**
     * Sets a new Name attribute of the current Customer object.
     *
     * @param newName the new Name attribute value
     */
    public void setName(String newName) {
        name = newName;
    }

    /**
     * Retrieves an array of Accounts associated with the current Customer
     * object.
     *
     * @return the current accounts array
     */
    public Account[] getAccounts() {
        return accounts;
    }

    /**
     * Sets a new array of Account objects associated with the current Customer
     * object.
     *
     * @param newAccounts the new Account attributes
     */
    public void setAccounts(Account[] newAccounts) {
        accounts = newAccounts;
    }
}

/**
 * Account class that represents a single bank account with balances,
 * transactions, and other attributes.
 */
class Account {
    private int Bonus;
    private int Fine;
    private int CarryOver;
    private int[] AscArray;
    private int[] DescArray;
    private int[] TransResults;

    // define the methods to set and get fine, bonus, and transactions
    // define the methods to compute and display Transaction report, statistics etc

    /**
     * Sets a new Fine attribute of the current Account object.
     *
     * @param newFine the new Fine attribute value
     */
    public void setFine(int newFine) {
        Fine = newFine;
    }

    /**
     * Sets a new Bonus attribute of the current Account object.
     *
     * @param newBonus the new Bonus attribute value
     */
    public void setBonus(int newBonus) {
        Bonus = newBonus;
    }

    /**
     * Retrieves the Fine attribute of the current Account object.
     *
     * @return the current Fine value
     */
    public int getFine() {
        return Fine;
    }

    /**
     * Retrieves the Bonus attribute of the current Account object.
     *
     * @return the current Bonus value
     */
    public int getBonus() {
        return Bonus;
    }

    /**
     * Sets a new CarryOver attribute of the current Account object.
     *
     * @param CarriedOverBalance the new CarryOver attribute value
     */
    public void setCarriedOverBalance(int CarriedOverBalance) {
        CarryOver = CarriedOverBalance;
    }

    /**
     * Retrieves the CarryOver attribute of the current Account object.
     *
     * @return the current CarryOver value
     */
    public int getCarriedOverBalance() {
        return CarryOver;
    }

    /**
     * Sets a new TransResults attribute of the current Account object.
     *
     * @param Trans the new TransResults attribute value
     */
     public void setTransactions(int [] Trans) {
        TransResults = Trans;
    }

    /**
     * Retrieves the TransResults attribute of the current Account object.
     *
     * @return the current TransResults value
     */
    public int[] getTransactions() {
        return TransResults;
    }

    /**
     * Displays transactions in ascending or descending order
     */
    public void DisplayTransactions(final PrintStream out,
                                    final boolean SortOrder) {
        int[] SortedArray;
        CopyArray(TransResults);

        // sort the array in ascending order.  If descending is needed we will reverse the sort
        Arrays.sort(AscArray);
        SortedArray = AscArray;

        // check sort order and execute the appropriate method
        if (SortOrder) {
            out.println("Sorted data in Ascending Order");

        } else {
            SortDescendingArray(AscArray);
            SortedArray = DescArray;
            out.println("Sorted data in Descending Order");
        }

        out.println("------------------------------\n");

        for (int i = 0; i <= SortedArray.length - 1; i++) {
            out.println("$ " + SortedArray[i]);
        }
    }

    /**
     * Copying the contents of the transresults array into a new array
     */
    private int[] CopyArray(int [] Trans) {
        AscArray = new int[Trans.length];

        for (int i = 0; i <= Trans.length - 1; i++) // Sort array ascending
        {
            AscArray[i] = Trans[i];
        }

        return AscArray;
    }


    /**
     * Sorting the array in descending order by looping through the sorted array in reverse
     */
    private int[] SortDescendingArray(int [] Trans) {
        DescArray = new int[Trans.length];

        for (int i = 0; i <= Trans.length - 1; i++) // Sort array descending
        {
            DescArray[i] = Trans[Trans.length - (i + 1)];
        }

        return DescArray;
    }

    /**
     * Displays the highest,lowest,total and average statistics for this account.
     */
    public void GetAccountStats(final PrintStream out) {
        int Total = 0;
        double Average = 0;
        int[] AccountStatsArray;
        AccountStatsArray = new int[TransResults.length];
        AccountStatsArray = TransResults;

        // sort the array in ascending order
        Arrays.sort(AccountStatsArray);

        // because the array is sorted in ascending order the highest value is the last one
        int HighValue = AccountStatsArray[AccountStatsArray.length - 1];

        // because the array is sorted in ascending order the highest value is the first one
        int LowValue = AccountStatsArray[0];

        // loop through the array and add the values together for the total
        for (int i = 0; i <= AccountStatsArray.length - 1; i++) {
            Total = Total + AccountStatsArray[i];
        }

        // divide the total by the total number of members in the array to get the average
        Average = Total / AccountStatsArray.length;

        // format the average output to display two decimal places
        DecimalFormat FormattedOutput = new DecimalFormat("#####0.00");
        String AverageOutput = FormattedOutput.format(Average);

        // displaying the account statistics
        out.println("\nStatistics");
        out.println("__________");
        out.println("Highest $ " + HighValue);
        out.println("Lowest  $ " + LowValue);
        out.println("Total   $ " + Total);
        out.println("Average $ " + AverageOutput);
    }

    /**
     * Displays a detailed transaction report for this account.
     */
    public void DisplayTransactionReport(final PrintStream out) {
        // create a currency format to match the benchmark; special handling
        // for negative numbers is given as its not the default way
        // DecimalFormat wanted to format the values
        final DecimalFormat numberFmt = new DecimalFormat("$ ##0;$ -#0");

        // create constants for the different type of transactions
        final String typeWithdrawal = "Withdrawal";
        final String typeDeposit    = "Deposit   ";
        final String typeBonus      = "  Bonus   ";
        final String typeFine       = "  Fine    ";

        // display the transaction report header for the report
        out.println("Detail Listing");
        out.println("--------------");
        out.println("Transaction    Amount  Balance");
        out.println("------------------------------");

        out.println("Starting balance\t" + numberFmt.format(CarryOver));

        // calculate and display the transaction listing
        int currentBalance = CarryOver;
        int totalWithdrawal = 0;
        int totalFine = 0;
        int totalDeposit = 0;
        int totalBonus = 0;

        // examine each transaction
        for (int transNo = 0; transNo < TransResults.length; transNo++) {

            // calculate the new intermediate balanace from the current
            // transaction amount
            int transAmount = TransResults[transNo];
            int newBalance = currentBalance + transAmount;

            // determine the type of the transaction and perform any special
            // handling for bonuses and fines based upon that type
            String transType;
            int currentBonus = 0;
            int currentFine = 0;

            if (transAmount < 0) {
                // handle withdrawals and any fine
                transType = typeWithdrawal;
                totalWithdrawal += transAmount;

                // calculate any fine on a withdrawal if the starting balance
                // before this transaction is less than $0
                if (currentBalance < 0) {
                    currentFine = Fine;
                }
            } else {
                // handle deposits and any bonus
                transType = typeDeposit;
                totalDeposit += transAmount;

                // calculate any bonus on a deposit if the starting balance
                // before this transaction is greater than $ 100
                if (currentBalance > 100) {
                    currentBonus = Bonus;
                }
            }

            // display the details of this transaction
            out.print(transType + "\t" + numberFmt.format(transAmount));
            out.println("\t" + numberFmt.format(newBalance));

            // display the bonus, and update balance, if any
            if (currentBonus != 0) {
                newBalance += currentBonus;
                totalBonus += currentBonus;

                out.print(typeBonus + "\t" + numberFmt.format(currentBonus));
                out.println("\t" + numberFmt.format(newBalance));
            }

            // display the fine, and update balance, if any
            if (currentFine != 0) {
                newBalance -= currentFine;
                totalFine += currentFine;

                out.print(typeFine + "\t" + numberFmt.format(currentFine));
                out.println("\t" + numberFmt.format(newBalance));
            }

            // update the current balance to the final newBalance amount
            // after bonus and fine have been calculated; update the totals
            // for summary processing
            currentBalance = newBalance;
        }

        // display a summary of all the transactions
        out.println();
        out.println("Summary");
        out.println("-------");
        out.println("Total Deposit    " + numberFmt.format(totalDeposit));
        out.println("Total Bonus      " + numberFmt.format(totalBonus));
        out.println("Total Withdrawal " + numberFmt.format(totalWithdrawal));
        out.println("Total Fine       " + numberFmt.format(totalFine));
        out.println();
        out.println("Last Balance : " + numberFmt.format(currentBalance));
    }
}
