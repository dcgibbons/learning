/*
 * Wk5DavidGibbons.java
 * Week 5 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-12 | Initial version.
 *   2.00  | 2006-01-14 | Updated for week 3 assignment; stand-alone Swing
 *                      | application instead of an Applet; calculation of
 *                      | mortgage payment detail breakdown and display of the
 *                      | data in a tabular form.
 *   2.10  | 2006-01-21 | Added validation of principal amount so any current
 *                      | mortgage is cleared and the user informed of the
 *                      | invalid input.
 *   3.00  | 2006-01-21 | Updated for week 4 assignment; rate and term fields
 *                      | returned and mortgage terms combobox simply updates
 *                      | these fields, which in turn causes a recalcuation to
 *                      | occur. User may also enter their own terms directly.
 *   3.10  | 2006-01-28 | Added Calculate button; switched to JTextField instead
 *                      | of JFormattedTextField and took control over
 *                      | validation and formatting to give user more feedback.
 *                      | Fixed bug where table viewport preferred size was
 *                      | being calculated before the table had been sized to
 *                      | fit the initial data.
 *   4.00  | 2006-01-28 | Updated for week 5 assignment: a chart of the payment
 *                      | detail is provided instead of a table breakdown.
 *                      | Mortgage terms are also read-in from an external data
 *                      | file rather than hard-coded within the program code.
 *   4.10  | 2006-02-02 | Modified default size of chart so that demo data
 *                      | would be visible on the screen without resizing.
 *   4.20  | 2006-02-05 | Added the payments table back to the program and
 *                      | used a JTabbedPane to show the table and chart in the
 *                      | same location of the screen.
 */

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.LinkedList;


/**
 * The week 5 assignment implemented by this class is a stand-alone application
 * that allows the user to calculate a mortgage payment.
 * <p/>
 * In addition, the payment breakdown over the length of the mortgage is
 * displayed in a table indicating the interest and principal breakdown amounts.
 * <p/>
 */

public class Wk5DavidGibbons {
    /**
     * The following constants are for titles of frames and windows.
     */
    private static final String TITLE_MAINFRAME = "Mortgage Calculator";

    /**
     * The following label constants are used to label each of the data entry
     * fields on the actual user-interface.
     */
    private static final String LABEL_CLOSE = "Close";
    private static final String LABEL_CALCULATE = "Calculate";
    private static final String LABEL_PRINCIPAL = "Principal Amount:";
    private static final String LABEL_MORTGAGE_CHOICES = "Mortgage Terms:";
    private static final String LABEL_TERM = "Term in Years:";
    private static final String LABEL_RATE = "Interest Rate:";
    private static final String LABEL_USER_ENTERED_CHOICE = "<html><i>User Entered</i></html>";

    /**
     * The following constants define the data point names and formats for each
     * data set provided to the Chart component.
     */
    final String[] CHART_DATA_NAMES = { "Interest", "Principal" };
    final NumberFormat[] CHART_FORMATS = {
            new DecimalFormat("#,##0"),
            new DecimalFormat("$ #,##0.00")
    };

    /**
     * Mortgage term data can be found an external file using the following
     * filename.
     */
    private static final String FILENAME_MORTGAGE_DATA = "data.txt";

    /**
     * The following input masks are numeric format masks used when creating the
     * data entry fields themselves. Each mask will be used to display the
     * current numeric value to the user after a new value has been input.
     */
    private static final DecimalFormat FMT_PRINCIPAL = new DecimalFormat("#,##0.00");
    private static final DecimalFormat FMT_TERM = new DecimalFormat("##0");
    private static final DecimalFormat FMT_RATE = new DecimalFormat("##.000");

    /**
     * The following message constants are used to display header, help, and
     * results of the mortgage payment calculation. Since JLabel can display
     * simple HTML tags embedded within the label text, we get some nice
     * formatting without having to do excessive work at the UI component level.
     */
    private static final String MSG_HEADER = "<html><h1>Mortgage Calculator</h1></html>";
    private static final String MSG_CALCULATE = "<html><b>Press <i>Calculate</i> button or <i>Enter</i> key<br> for new mortgage payment detail</b></html>";
    private static final String MSG_PAYMENT = "<html><b>Payment Amount:</b> {0,number,$ #,##0.00}<br></html>";
    private static final String MSG_INVALID_INPUT = "<html><b>Invalid Input Field - Must Be Numeric</b><br></html>";
    private static final String MSG_PRINCIPAL_NEGATIVE = "<html><b>Principal Amount Must Be > 0.00</html>";
    private static final String MSG_RATE_NEGATIVE = "<html><b>Rate Must Be > 0.000</b><br></html>";
    private static final String MSG_TERM_NEGATIVE = "<html><b>Term Must Be > 0</b><br></html>";

    /**
     * The following are error messages used when there are problems loading
     * external data files.
     */
    private static final String ERR_DATA_FILE_IOERR = "Unable to read data file";
    private static final String ERR_DATA_FILE_CORRUPT = "Data File Corrupt";
    private static final String ERR_DATA_FILE_ERROR = "Error Loading Mortgage Data";

    /**
     * The JTable of the payment history needs a default number of visible
     * rows so that its preferredSize can be accurately calculated.
     */
    private static final int VISIBLE_PAYMENT_TABLE_ROWS = 12;

    /**
     * A constant to indicate the minimum number of columns that the data entry
     * fields should provide to the user. This helps to ensure that our
     * components calculate an appropriate preferredSize.
     */
    private static final int FIELD_COLUMNS = 10;

    /**
     * The following constants are default values for the data entry field and
     * are used to populate those fields when the app is initialized. These
     * defaults make it easy for the user to get a result and to see the format
     * of the expceted input data.
     */
    private static final double DEFAULT_PRINCIPAL = 325000.00;
    private static final int DEFAULT_TERM = 30;
    private static final double DEFAULT_RATE = 6.25;

    /**
     * The first element of the default mortgages list is used to indicate
     * that the user is inputting their own mortgage terms. This constant is
     * used so that the array position isn't hard-coded elsewhere in the
     * program.
     */
    private static final int CHOICE_USER_ENTERED = 0;

    /**
     * The Java Look & Feel Guidelines discuss the visual alignment between
     * components. Components are generally separated by multiples of a single
     * Look & Feel padding unit, which is 6 pixels. If a component typically
     * contains a 3D-border or shadow, then the padding is a multiple of 6
     * pixels minus 1. Further details are discussed at
     * http://java.sun.com/products/jlf/ed2/book/HIG.Visual2.html
     */
    private static final int LFPAD = 6;

    /**
     * Since we are in a stand-alone application, we need a JFrame to be the
     * root container for all of our other UI elements.
     */
    private JFrame mainFrame;

    /**
     * Variables to define the label and field for the Principal amount.
     */
    private JLabel labelPrincipal;
    private JTextField fieldPrincipal;

    /**
     * Variables to define the label and combo-box menu for the available
     * Mortgage terms from which the user can pick.
     */
    private JLabel labelMortgageChoices;
    private JComboBox fieldMortgageChoices;

    /**
     * Variables to define the label and field for the Interest Rate.
     */
    private JLabel labelRate;
    private JTextField fieldRate;

    /**
     * Variables to define the label and field for the Mortgage Term.
     */
    private JLabel labelTerm;
    private JTextField fieldTerm;

    /**
     * Variables to define the header and message labels.
     */
    private JLabel labelHeader;
    private JLabel labelMessage;

    /**
     * Variables to define the chart used to display the mortgage payment
     * detail.
     */
    private JTableHelper paymentsTable;
    private MortgagePaymentsTableModel paymentsModel;
    private Chart paymentsChart;

    /**
     * Variables to define the command buttons presented to the user.
     */
    private JButton calcButton;
    private JButton closeButton;

    /**
     * This flag is set whenever the values for the mortgage terms is being
     * changed automatically and we do not want our document listener to react
     * to those changes. This allows the JComboBox listener to automatically
     * update those fields without the fields then changing the combo box.
     */
    private boolean ignoreDocumentChange;

    /**
     * main entry point for this application - starts our Mortgage Calculator
     * user interface
     *
     * @param args not used
     */
    public static void main(String[] args) {
        new Wk5DavidGibbons();
        // Swing event loop takes over in another thread; this thread will
        // terminate but the application will not exit until the Swing event
        // thread exits or the application is forced to exit.
    }

    /**
     * A private onstructor for this class so that an instance can only be
     * created via the main method.
     */
    private Wk5DavidGibbons() {
        createComponents();
        layoutComponents();
        sizeMortgageTable();
        mainFrame.pack();

        // once our components have been laid out then our minimum and preferred
        // sizes should be calculated, so keep an eye out on the mainframe and
        // don't let the user resize it smaller than the minimum size
        mainFrame.addComponentListener(new ResizeWatcher());

        // center the main frame on the user's display before making it visible
        centerWindow(mainFrame);

        // display our main frame - this will start the Swing event loop and
        // essentially take over main-line execution of the program
        mainFrame.setVisible(true);

        // make sure that our principal field requests focus once the frame has
        // been displayed
        fieldPrincipal.requestFocusInWindow();

        // load the mortgages from the data file - do this after the UI has
        // been displayed so that there is more of the application for the
        // user to see so any error messages won't be as confusing
        loadMortgages();
    }

    /**
     * Creates all of the UI components used by this application.
     */
    private void createComponents() {
        mainFrame = new JFrame(TITLE_MAINFRAME);
        mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        // the data entry fields need their focus watched...
        final FocusWatcher focusWatcher = new FocusWatcher();

        // create the label and field for the mortgage principal amount
        fieldPrincipal = new JTextField();
        fieldPrincipal.addFocusListener(focusWatcher);
        fieldPrincipal.setColumns(FIELD_COLUMNS);

        labelPrincipal = new JLabel(LABEL_PRINCIPAL);
        labelPrincipal.setLabelFor(fieldPrincipal);

        // create the label and field for the mortgage terms combo-box/list
        fieldMortgageChoices = new JComboBox();
        fieldMortgageChoices.addActionListener(new FieldActionListener());

        labelMortgageChoices = new JLabel(LABEL_MORTGAGE_CHOICES);
        labelMortgageChoices.setLabelFor(fieldMortgageChoices);

        // create the label and field for the mortgage term period
        fieldTerm = new JTextField();
        fieldTerm.addFocusListener(focusWatcher);
        fieldTerm.setColumns(FIELD_COLUMNS);

        labelTerm = new JLabel(LABEL_TERM);
        labelTerm.setLabelFor(fieldTerm);

        // create the label and field for the mortgage annual interest rate
        fieldRate = new JTextField();
        fieldRate.addFocusListener(focusWatcher);
        fieldRate.setColumns(FIELD_COLUMNS);

        labelRate = new JLabel(LABEL_RATE);
        labelRate.setLabelFor(fieldRate);

        // create the label that will provide the header text for the panel
        labelHeader = new JLabel(MSG_HEADER);

        // create the label that will provide any mesages to the user,
        // including the newly calculated mortgage payment amounts
        labelMessage = new JLabel("");

        // create the table model and table that will display the payment
        // breakdown once a mortgage has been calculated
        paymentsModel = new MortgagePaymentsTableModel();
        paymentsTable = new JTableHelper(paymentsModel);
        paymentsTable.setColumnSelectionAllowed(false);
        paymentsTable.setRowSelectionAllowed(false);
        paymentsTable.setDefaultRenderer(Double.class, new CurrencyRenderer());

        // create a chart used to display the mortgage payment detail; a small
        // pixel size is used so our primary window isn't sized too large
        paymentsChart = new Chart(400, 200);
        paymentsChart.setBorder(BorderFactory.createLineBorder(Color.BLACK));

        // create the button that will allow the user to calculate a mortgage
        // payment schedule from the current input
        final Action calcAction = new CalcAction();
        calcButton = new JButton(calcAction);
        mainFrame.getRootPane().setDefaultButton(calcButton);

        // create the buttom that will allow the user to close the main frame;
        // the user will have several options, depending on their OS, for
        // shutting down the application besides this button
        final Action closeAction = new CloseAction();
        closeButton = new JButton(closeAction);

        // set default values for all of the fields to make the user feel cozy
        fieldPrincipal.setText(FMT_PRINCIPAL.format(DEFAULT_PRINCIPAL));
        fieldRate.setText(FMT_RATE.format(DEFAULT_RATE));
        fieldTerm.setText(FMT_TERM.format(DEFAULT_TERM));

        // watch the documents of our input fields so that we can reset our
        // current mortgage calculate whenever the user changes anything
        final DocumentListener docWatcher = new DocumentWatcher();
        fieldPrincipal.getDocument().addDocumentListener(docWatcher);
        fieldRate.getDocument().addDocumentListener(docWatcher);
        fieldTerm.getDocument().addDocumentListener(docWatcher);
    }

    /**
     * Lays all of the UI components used by the app into the mainframe's
     * default container panel.
     */
    private void layoutComponents() {
        final JPanel topPanel = layoutTopPanel();
        final JPanel centerPanel = layoutCenterPanel();
        final JPanel buttonPanel = layoutButtonPanel();

        // when adding the content to the root panel we must follow the look &
        // feel guidelines which state 2 padding units between the edges of the
        // root panel and the components. If the bottom and right components
        // are 3D or shadowed components then it should be 2 padding units - 1
        // for consistency.

        // the top panel gives us our border for the top and left sides
        final Border topBorder = BorderFactory.createEmptyBorder(
                LFPAD * 2,      // top
                LFPAD * 2,      // left
                0,              // bottom
                LFPAD * 2 - 1);  // right
        topPanel.setBorder(topBorder);

        // the center border needs to have a visible separation from the top
        // panel components
        final Border centerBorder = BorderFactory.createEmptyBorder(
                LFPAD * 2,      // top
                LFPAD * 2,      // left
                0,              // bottom
                LFPAD * 2 - 1);  // right
        centerPanel.setBorder(centerBorder);

        // the bottom panel gives us our border for the bottom and right sides
        // and also must be separated from the top panel by 3 padding units - 1
        // to give any buttons appropriate spacing
        final Border bottomBorder = BorderFactory.createEmptyBorder(
                LFPAD * 3 - 1,  // top
                LFPAD * 2,      // left
                LFPAD * 2 - 1,  // bottom
                LFPAD * 2 - 1); // right
        buttonPanel.setBorder(bottomBorder);

        // finally, change our root panel to have a BorderLayout and force the
        // top panel to use the North section and the bottom panel to use the
        // South section. this will allow the root panel to be resized freely
        // and the other panels should react correctly in both grow and shrink
        // resizing scenarios.
        final JPanel contentPanel = (JPanel) mainFrame.getContentPane();
        contentPanel.setLayout(new BorderLayout());
        contentPanel.add(topPanel, BorderLayout.NORTH);
        contentPanel.add(centerPanel, BorderLayout.CENTER);
        contentPanel.add(buttonPanel, BorderLayout.SOUTH);
    }

    /**
     * Utility method to layout the top panel containing all of the messages
     * and data entry fields.
     *
     * @return the new top panel
     */
    private JPanel layoutTopPanel() {
        // create a panel that will be located at the top of our content area
        // and will contain all of our message labels and data entry fields.
        // the complicated GridBagLayout is used, so utility methods are used
        // to help isolate the grungy details.
        final JPanel topPanel = new JPanel(new GridBagLayout());

        // the header label must be seperated from the rest of the components
        addLabel(topPanel, labelHeader, LFPAD * 2, LFPAD * 2);

        addField(topPanel, labelPrincipal, fieldPrincipal);
        addField(topPanel, labelMortgageChoices, fieldMortgageChoices);
        addField(topPanel, labelTerm, fieldTerm);
        addField(topPanel, labelRate, fieldRate);

        // the message label does not require any bottom padding as it is the
        // last component in the panel
        addLabel(topPanel, labelMessage, 0, LFPAD * 2);

        return topPanel;
    }

    /**
     * Utility method to layout the center panel that holds the payment chart.
     */
    private JPanel layoutCenterPanel() {
        // place the paymentsTable into a scrollPane so it can be easily viewed
        final JScrollPane paymentsTablePane =
                new JScrollPane(paymentsTable,
                                JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        // place the paymentsTable and paymentsChart into a split pane so they
        // can both be easily viewed at once
        final JSplitPane splitPanel =
                new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                               paymentsTablePane,
                               paymentsChart);

        final JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.add(splitPanel, BorderLayout.CENTER);

        return centerPanel;
    }

    /**
     * Utility method to create and layout the panel used to hold command
     * buttons.
     *
     * @return the new button panel
     */
    private JPanel layoutButtonPanel() {
        // by Look & Feel convention, command buttons are aligned horizontially
        // with a single padding unit between them (minus 1 because buttons are
        // 3D).
        final GridLayout gridLayout = new GridLayout(1, 0, LFPAD * 2 - 1, 0);
        final JPanel buttonGridPanel = new JPanel(gridLayout);
        buttonGridPanel.add(calcButton);
        buttonGridPanel.add(closeButton);

        // to force all of the bottons to be their preferred size and be
        // aligned to the right of the button panel, a BorderLayout is used and
        // the button grid is placed in the East section of the layout grid.
        final JPanel buttonPanel = new JPanel(new BorderLayout());
        buttonPanel.add(buttonGridPanel, BorderLayout.EAST);

        return buttonPanel;
    }


    /**
     * Utility method to add a centered label to our panel using a
     * GridBagLayout.
     *
     * @param panel  the panel to which the component will be added
     * @param label  the actual label object that will be added
     * @param bottom any inset padding on the bottom of the component
     * @param right  any inset padding on the right of the component
     */
    private void addLabel(final JPanel panel,
                          final JLabel label,
                          final int bottom,
                          final int right) {
        label.setHorizontalAlignment(SwingConstants.CENTER);

        // this label should be centered, take up the entire row, and be resized
        // horizontally whenever the panel changes size
        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = new Insets(0, 0, bottom, right);
        gbc.weightx = 1.0;

        panel.add(label, gbc);
    }


    /**
     * Utility method to add a field consisting of a JLabel and an actual field
     * object. The label will be given minimum spacing while the remainder goes
     * to the field objects themselves.
     *
     * @param panel the panel to which the component will be added
     * @param label the actual label object that will be added
     * @param field the actual field component object that will be added
     */
    private void addField(final JPanel panel, final JLabel label,
                          final JComponent field) {
        label.setHorizontalAlignment(SwingConstants.RIGHT);

        final GridBagConstraints gbc = new GridBagConstraints();

        // the label should be in a fixed size column and not be resized as
        // the panel changes size
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, LFPAD * 2, LFPAD * 2);
        gbc.weightx = 0.0;
        panel.add(label, gbc);

        // the field should receive the remaining space on the row and receive
        // all of the extra space when the panel is resized (or shrunk)
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = new Insets(0, 0, LFPAD * 2 - 1, LFPAD * 2 - 1);
        gbc.weightx = 1.0;
        panel.add(field, gbc);
    }

    /**
     * Utility method that will load mortgage data from a file.
     */
    private void loadMortgages() {
        // create a list of mortgages so we can dynamically add as many as our
        // data file allows. the first entry is always our default
        // "user entered" value so the combo box works consistently...
        final LinkedList mortgages = new LinkedList();
        mortgages.add(LABEL_USER_ENTERED_CHOICE);

        try {
            final BufferedReader reader =
                    new BufferedReader(new FileReader(FILENAME_MORTGAGE_DATA));

            String line;
            while ((line = reader.readLine()) != null) {
                // split the line of data into tokens separated by commas; if
                // there are two tokens found, we assume we have a valid line
                // of data that can be parsed into rate and term values
                final String[] values = line.split(",");
                if (values.length == 2) {
                    final double rate = Double.parseDouble(values[0].trim());
                    final int term = Integer.parseInt(values[1].trim());

                    // add a new set of mortgage terms to the list; the term
                    // must be divided by 12 to convert to the number of years
                    mortgages.add(new Mortgage(rate, term / 12));
                }
            }
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(mainFrame,
                                          ERR_DATA_FILE_IOERR,
                                          ERR_DATA_FILE_ERROR,
                                          JOptionPane.ERROR_MESSAGE);
            ex.printStackTrace(System.err);
        } catch (NumberFormatException ex ) {
            JOptionPane.showMessageDialog(mainFrame,
                                          ERR_DATA_FILE_CORRUPT,
                                          ERR_DATA_FILE_ERROR,
                                          JOptionPane.ERROR_MESSAGE);
            ex.printStackTrace(System.err);
        }

        fieldMortgageChoices.setModel(new DefaultComboBoxModel(mortgages.toArray()));
    }

    /**
     * setup a dummy mortgage payment that gives us a large set of values
     * so that the table can resize its columns based on what is likely a
     * "worst-case" scenario. this should leave plenty of room for real
     * mortgage payments without making the window overly large and yet
     * still reacting to local system visual sizes.
     */
    private void sizeMortgageTable() {
        resetMortgage();
        final MortgagePayment p = new MortgagePayment(999999.99,
                                                      999999.99,
                                                      999999.99,
                                                      99999999.99);
        paymentsModel.setPayments(new MortgagePayment[] { p });
        paymentsTable.resizeTableColumns();
        paymentsTable.limitTableViewport(VISIBLE_PAYMENT_TABLE_ROWS,
                                         true,
                                         false);
        resetMortgage();
    }

    /**
     * Utility method that will reset the mortgage to an empty value and inform
     * the user they need to recalculate.
     */
    private void resetMortgage() {
        labelMessage.setText(MSG_CALCULATE);
        paymentsChart.clearData();
        paymentsModel.setPayments(new MortgagePayment[0]);
        mainFrame.repaint();
    }

    /**
     * Utility method that will calculate a new mortgage payment and payment
     * breakdown based on the user's current selection.
     */
    private void calculateMortgage() {
        boolean inputValid = true;

        // fetch the new values from the data entry fields and calculate a
        // new mortgage
        double principal = 0.0;
        try {
            final String principalText = fieldPrincipal.getText();
            principal = FMT_PRINCIPAL.parse(principalText).doubleValue();
            if (principal <= 0.0) {
                invalidInput(fieldPrincipal, MSG_PRINCIPAL_NEGATIVE);
                inputValid = false;
            }
        } catch (ParseException ex) {
            invalidInput(fieldPrincipal, MSG_INVALID_INPUT);
            inputValid = false;
        }

        double rate = 0.0;
        if (inputValid) {
            try {
                final String rateText = fieldRate.getText();
                rate = FMT_RATE.parse(rateText).doubleValue();
                if (rate <= 0.0) {
                    invalidInput(fieldRate, MSG_RATE_NEGATIVE);
                    inputValid = false;
                }
            } catch (ParseException ex) {
                invalidInput(fieldRate, MSG_INVALID_INPUT);
                inputValid = false;
            }
        }

        int term = 0;
        if (inputValid) {
            try {
                final String termText = fieldTerm.getText();
                term = FMT_TERM.parse(termText).intValue();
                if (term <= 0) {
                    invalidInput(fieldTerm, MSG_TERM_NEGATIVE);
                    inputValid = false;
                }
            } catch (ParseException ex) {
                invalidInput(fieldTerm, MSG_INVALID_INPUT);
                inputValid = false;
            }
        }

        if (inputValid) {
            // create a new Mortgage object from the selected mortgage terms and
            // then update the principal with the user's requested value
            final Mortgage mortgage = new Mortgage(principal, rate, term);

            // format the mortgage payment into a user-friendly message
            final Object[] values = {new Double(mortgage.getPayment())};
            final String message = MessageFormat.format(MSG_PAYMENT, values);

            // update our message label and payment data and force our UI
            // to repaint itself with the new mortgage results
            labelMessage.setText(message);

            final MortgagePayment[] payments = mortgage.getPayments();
            paymentsModel.setPayments(payments);

            // create a data set so the chart may be updated with the mortgage
            // payment detail
            final double[][] chartData = new double[2][payments.length];
            int crossOverPoint = -1;
            for (int i = 0; i < payments.length; i++) {
                chartData[0][i] = payments[i].getInterest().doubleValue();
                chartData[1][i] = payments[i].getPrincipal().doubleValue();

                // save the first point at which the interest and principal
                // cross over
                if (crossOverPoint == -1 && chartData[0][i] < chartData[1][i]) {
                    crossOverPoint = i;
                }
            }
            paymentsChart.setData(chartData, crossOverPoint,
                                  CHART_DATA_NAMES, CHART_FORMATS);

            mainFrame.repaint();
        }
    }

    /**
     * Utility method that informs the user that their input was invalid.
     */
    private void invalidInput(final JComponent component, final String msg) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                labelMessage.setText(msg);
                paymentsModel.setPayments(new MortgagePayment[0]);
                paymentsChart.clearData();
                component.requestFocusInWindow();
                mainFrame.repaint();
            }
        });
    }

    /**
     * Utility method that will close and cleanup the main frame and then exit
     * the application.
     */
    private void exitApp() {
        mainFrame.setVisible(false);
        mainFrame.dispose();
        System.exit(0);
    }

    /**
     * Utility method that will center a window on the user's screen.
     * @param window the window or frame to center
     */
    private void centerWindow(final Window window) {
        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        final Dimension frameSize = window.getSize();

        // if our frame is actually larger than the screen then we'll want the
        // calculation to still work properly
        if (frameSize.height > screenSize.height) {
            frameSize.height = screenSize.height;
        }
        if (frameSize.width > screenSize.width) {
            frameSize.width = screenSize.width;
        }

        final int xPos = (screenSize.width - frameSize.width) / 2;
        final int yPos = (screenSize.height - frameSize.height) / 2;
        window.setLocation(xPos, yPos);
    }

    /**
     * This inner class watches for focus events from the text components
     * found within this app. If the focus is gained for a text component
     * then the entire field is selected, allowing the user to overwrite the
     * old value by default. If the focus is lost and the field value has
     * changed, then the payment field has its result erased so the user
     * knows they must calculate the new mortgage.
     */
    private class FocusWatcher extends FocusAdapter {
        public void focusGained(final FocusEvent event) {
            final Component component = event.getComponent();
            if (component instanceof JTextComponent) {
                // select all of the component text whenever the component
                // gains focus - we use invokeLater to ensure that the focus is
                // grabbed after all other work is done, which allows
                // components like JFormattedTextField to work properly
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        ((JTextComponent) component).selectAll();
                    }
                });
            }
        }
    }

    /**
     * This inner class will be activated by the Calculate button and will
     * cause a new mortgage payment schedule to be calculated based on the
     * current input.
     */
    private class CalcAction extends AbstractAction {
        public CalcAction() {
            putValue(Action.NAME, LABEL_CALCULATE);
        }

        public void actionPerformed(final ActionEvent event) {
            calculateMortgage();
        }
    }

    /**
     * This inner class will be activated by the Close button and will
     * gracefully cause the application to exit.
     */
    private class CloseAction extends AbstractAction {
        public CloseAction() {
            putValue(Action.NAME, LABEL_CLOSE);
        }

        public void actionPerformed(final ActionEvent event) {
            exitApp();
        }
    }

    /**
     * This inner class will watch the mainframe for any resize events; if the
     * frame is being resized smaller than the initial minimumSize then the
     * resize will be stopped and the size returned back to the minimumSize.
     * This should prevent the user from making the window so small that
     * components begin to overlap one another.
     */
    private class ResizeWatcher extends ComponentAdapter {
        private final Dimension minSize;

        public ResizeWatcher() {
            minSize = mainFrame.getMinimumSize();
        }

        public void componentResized(final ComponentEvent evt) {
            final Dimension newSize = mainFrame.getSize();

            boolean resize = false;

            if (newSize.width < minSize.width) {
                resize = true;
                newSize.width = minSize.width;
            }

            if (newSize.height < minSize.height) {
                resize = true;
                newSize.height = minSize.height;
            }

            if (resize) {
                mainFrame.setSize(newSize);
            }
        }
    }

    /**
     * This inner class will recalculate the mortgage whenever the user performs
     * an action on the mortgage task combobox, i.e. selecting a new mortgage
     * term.
     */
    private class FieldActionListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            final Object item = fieldMortgageChoices.getSelectedItem();
            if (item instanceof Mortgage) {
                final Mortgage mortgageTerms = (Mortgage) item;

                // update the fields, but tell the document listener to
                // not react to those changes since we don't want the
                // combo box to then be changed in turn
                ignoreDocumentChange = true;
                fieldRate.setText(FMT_RATE.format(mortgageTerms.getRate()));
                fieldTerm.setText(FMT_TERM.format(mortgageTerms.getTerm()));
                ignoreDocumentChange = false;
            }
        }
    }

    /**
     * This inner class provides a custom JTable cell renderer for numeric
     * objects that will format the value as a currency amount.
     */
    private static class CurrencyRenderer extends DefaultTableCellRenderer {
        private static final DecimalFormat FMT = new DecimalFormat("$#,##0.00");

        public void setValue(Object value) {
            setText(value == null ? "" : FMT.format(value));
            setHorizontalAlignment(SwingConstants.TRAILING);
        }
    }

    /**
     * This inner class will watch for changes to any of the input field
     * documents and then inform the user that they need to recalculate the
     * mortgage payment to see new results.
     */
    private class DocumentWatcher implements DocumentListener {
        public void insertUpdate(DocumentEvent event) {
            if (!ignoreDocumentChange) {
                fieldMortgageChoices.setSelectedIndex(CHOICE_USER_ENTERED);
            }
            resetMortgage();
        }

        public void removeUpdate(DocumentEvent event) {
            if (!ignoreDocumentChange) {
                fieldMortgageChoices.setSelectedIndex(CHOICE_USER_ENTERED);
            }
            resetMortgage();
        }

        public void changedUpdate(DocumentEvent event) {}
    }
}
