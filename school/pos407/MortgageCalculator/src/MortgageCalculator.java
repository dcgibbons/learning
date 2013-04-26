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
 *   5.00  | 2006-02-03 | Merged Week 4 and Week 5 assignments into an Applet
 *                      | that contains all features. The payment chart and
 *                      | table are presented using a tabbed panel.
 */

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.border.Border;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.InputStream;
import java.text.DecimalFormat;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.LinkedList;

public class MortgageCalculator extends JApplet {
    /**
     * The following label constants are used to label each of the data entry
     * fields on the actual user-interface.
     */
    private static final String LABEL_CALCULATE = "Calculate";
    private static final String LABEL_PRINCIPAL = "Principal Amount:";
    private static final String LABEL_MORTGAGE_CHOICES = "Mortgage Terms:";
    private static final String LABEL_TERM = "Term in Years:";
    private static final String LABEL_RATE = "Interest Rate:";
    private static final String LABEL_USER_ENTERED_CHOICE = "<html><i>User Entered</i></html>";

    /**
     * The JTable of the payment history needs a default number of visible
     * rows so that its preferredSize can be accurately calculated.
     */
    private static final int VISIBLE_PAYMENT_TABLE_ROWS = 12;

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
    private Chart paymentsChart;

    // TODO
    private MortgagePaymentsTableModel paymentsModel;
    private JTableHelper paymentsTable;

    /**
     * Variables to define the command buttons presented to the user.
     */
    private JButton calcButton;

    /**
     * This flag is set whenever the values for the mortgage terms is being
     * changed automatically and we do not want our document listener to react
     * to those changes. This allows the JComboBox listener to automatically
     * update those fields without the fields then changing the combo box.
     */
    private boolean ignoreDocumentChange;

    /**
     * A private onstructor for this class so that an instance can only be
     * created via the main method.
     */
    public void init() {
        createComponents();
        layoutComponents();
        sizeMortgageTable();

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

        // create a chart used to display the mortgage payment detail; a small
        // pixel size is used so our primary window isn't sized too large
        paymentsChart = new Chart(320, 240);
        paymentsChart.setBorder(BorderFactory.createLineBorder(Color.BLACK));

        // TODO
        paymentsModel = new MortgagePaymentsTableModel();
        paymentsTable = new JTableHelper(paymentsModel);
        paymentsTable.setColumnSelectionAllowed(false);
        paymentsTable.setRowSelectionAllowed(true);
        paymentsTable.setDefaultRenderer(Double.class, new CurrencyRenderer());

        // create the button that will allow the user to calculate a mortgage
        // payment schedule from the current input
        final Action calcAction = new CalcAction();
        calcButton = new JButton(calcAction);
        getRootPane().setDefaultButton(calcButton);

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
        final JPanel contentPanel = (JPanel) getContentPane();
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
        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.add("Chart", new JScrollPane(paymentsChart));
        tabbedPane.add("Table", new JScrollPane(paymentsTable));

        final JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.add(tabbedPane);
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
     * Utility method that will load mortgage data from a file.
     */
    private void loadMortgages() {
        // create a list of mortgages so we can dynamically add as many as our
        // data file allows. the first entry is always our default
        // "user entered" value so the combo box works consistently...
        final LinkedList mortgages = new LinkedList();
        mortgages.add(LABEL_USER_ENTERED_CHOICE);

        try {
            final InputStream resource = getClass().getClassLoader().getResourceAsStream(FILENAME_MORTGAGE_DATA);
            if (resource == null) {
                throw new IOException(FILENAME_MORTGAGE_DATA + " not found");
            }
            final BufferedReader reader =
                    new BufferedReader(new InputStreamReader(resource));

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
            JOptionPane.showMessageDialog(getRootPane(),
                                          ERR_DATA_FILE_IOERR,
                                          ERR_DATA_FILE_ERROR,
                                          JOptionPane.ERROR_MESSAGE);
            ex.printStackTrace(System.err);
        } catch (NumberFormatException ex ) {
            JOptionPane.showMessageDialog(getRootPane(),
                                          ERR_DATA_FILE_CORRUPT,
                                          ERR_DATA_FILE_ERROR,
                                          JOptionPane.ERROR_MESSAGE);
            ex.printStackTrace(System.err);
        }

        fieldMortgageChoices.setModel(new DefaultComboBoxModel(mortgages.toArray()));
    }

    /**
     * Utility method that will reset the mortgage to an empty value and inform
     * the user they need to recalculate.
     */
    private void resetMortgage() {
        labelMessage.setText(MSG_CALCULATE);
        paymentsChart.clearData();
        paymentsModel.setPayments(new MortgagePayment[0]);
        repaint();
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
            paymentsModel.setPayments(mortgage.getPayments());

            // create a data set so the chart may be updated with the mortgage
            // payment detail
            final MortgagePayment[] payments = mortgage.getPayments();
            final double[][] chartData = new double[2][payments.length];
            for (int i = 0; i < payments.length; i++) {
                chartData[0][i] = payments[i].getInterest().doubleValue();
                chartData[1][i] = payments[i].getPrincipal().doubleValue();
            }

            final String[] chartDataNames = { "Interest", "Principal" };
            final NumberFormat[] formats = {
                    new DecimalFormat("#,##0"),
                    new DecimalFormat("$ #,##0.00")
            };

            paymentsChart.setData(chartData, chartDataNames, formats);

            repaint();
        }
    }

    /**
     * Utility method that informs the user that their input was invalid.
     */
    private void invalidInput(final JComponent component, final String msg) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                labelMessage.setText(msg);
                paymentsChart.clearData();
                paymentsModel.setPayments(new MortgagePayment[0]);
                component.requestFocusInWindow();
                repaint();
            }
        });
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
}
