/*
 * Wk2DavidGibbons.java
 * Week 2 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-12 | Initial version.
 */

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.text.DecimalFormat;
import java.text.MessageFormat;


/**
 * This class implements the Applet of the week 2 programming assignment. This
 * applet presents a simple three-field interface to the user that allows them
 * to input a principal amount, a loan term, and an interest rate. Once they
 * activate the Calculate button, a new mortgage payment will be computed and
 * displayed on screen. The user can then return to any of the data entry fields
 * and change the values and then recompute a new mortgage payment.
 */
public class Wk2DavidGibbons extends JApplet {

    /**
     * The following label constants are used to label each of the data entry
     * fields on the actual user-interface.
     */
    private static final String LABEL_CALCULATE = "Calculate";
    private static final String LABEL_PRINCIPAL = "Principal Amount:";
    private static final String LABEL_TERM = "Term in Years:";
    private static final String LABEL_RATE = "Interest Rate:";

    /**
     * The following input masks are numeric format masks used when creating the
     * data entry fields themselves. Each mask will be used to display the
     * current numeric value to the user after a new value has been input.
     */
    private static final String MASK_PRINCIPAL = "#,###.##";
    private static final String MASK_TERM = "###";
    private static final String MASK_RATE = "##.###";

    /**
     * The following message constants are used to display header, help, and
     * results of the mortgage payment calculation. Since JLabel can display
     * simple HTML tags embedded within the label text, we get some nice
     * formatting without having to do excessive work at the UI component level.
     */
    private static final String MSG_HEADER = "<html><h1>Mortgage Calculator</h1></html>";
    private static final String MSG_CALCULATE = "<html><i>press Calculate</i></html>";
    private static final String MSG_PAYMENT = "<html><b>Payment Amount:</b> {0,number,$ #,###.##}</html>";

    /**
     * A constant to indicate the minimum number of columns that the data entry
     * fields should provide to the user. This helps to ensure that our
     * components calculate an appropriate preferredSize.
     */
    private static final int FIELD_COLUMNS = 10;

    /**
     * The following constants are default values for the data entry field and
     * are used to populate those fields when the Applet is initialized. These
     * defaults make it easy for the user to get a result and to see the format
     * of the expceted input data.
     */
    private static final double DEFAULT_PRINCIPAL = 325000.00;
    private static final int DEFAULT_TERM = 30;
    private static final double DEFAULT_RATE = 6.25;

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
    private JFormattedTextField fieldPrincipal;

    /**
     * Variables to define the label and field for the Loan Term.
     */
    private JLabel labelTerm;
    private JFormattedTextField fieldTerm;

    /**
     * Variables to define the label and field for the Interest Rate.
     */
    private JLabel labelRate;
    private JFormattedTextField fieldRate;

    /**
     * Variables to define the header and message labels.
     */
    private JLabel labelHeader;
    private JLabel labelMessage;

    /**
     * Variables to define the command buttons presented to the user.
     */
    private JButton calcButton;

    /**
     * Initialize our Applet. Called by the Applet superclass before the
     * applet is made visible.
     */
    public void init() {
        createComponents();
        layoutComponents();

        fieldPrincipal.requestFocusInWindow();
    }

    /**
     * Creates all of the UI components used by this Applet.
     */
    protected void createComponents() {
        // the data entry fields need their focus watched...
        final FocusWatcher focusWatcher = new FocusWatcher();

        // create the label and field for the mortgage principal amount
        fieldPrincipal = new JFormattedTextField(
                new DecimalFormat(MASK_PRINCIPAL));
        fieldPrincipal.addFocusListener(focusWatcher);
        fieldPrincipal.setColumns(FIELD_COLUMNS);

        labelPrincipal = new JLabel(LABEL_PRINCIPAL);
        labelPrincipal.setLabelFor(fieldPrincipal);

        // create the label and field for the mortgage term period
        fieldTerm = new JFormattedTextField(new DecimalFormat(MASK_TERM));
        fieldTerm.addFocusListener(focusWatcher);
        fieldTerm.setColumns(FIELD_COLUMNS);

        labelTerm = new JLabel(LABEL_TERM);
        labelTerm.setLabelFor(fieldTerm);

        // create the label and field for the mortgage annual interest rate
        fieldRate = new JFormattedTextField(new DecimalFormat(MASK_RATE));
        fieldRate.addFocusListener(focusWatcher);
        fieldRate.setColumns(FIELD_COLUMNS);

        labelRate = new JLabel(LABEL_RATE);
        labelRate.setLabelFor(fieldRate);

        // create the label that will provide the header text for the panel
        labelHeader = new JLabel(MSG_HEADER);

        // create the label that will provide any mesages to the user,
        // including the newly calculated mortgage payment amounts
        labelMessage = new JLabel(MSG_CALCULATE);

        // create the Calculate button with an Action object that will populate
        // the button's label and provide it with an action to callback when
        // the button is activated
        calcButton = new JButton(new CalculateAction());

        // make sure the Calculate button is the default button so the user can
        // simply press Enter to activate the action
        getRootPane().setDefaultButton(calcButton);

        // set default values for all of the fields to make the user feel cozy
        fieldPrincipal.setValue(new Double(DEFAULT_PRINCIPAL));
        fieldTerm.setValue(new Long(DEFAULT_TERM));
        fieldRate.setValue(new Double(DEFAULT_RATE));
    }

    /**
     * Lays all of the UI components used by the Applet into the Applet's
     * default container panel.
     */
    protected void layoutComponents() {
        final JPanel topPanel = layoutTopPanel();
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
                0);             // right
        topPanel.setBorder(topBorder);

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
        addField(topPanel, labelTerm, fieldTerm);
        addField(topPanel, labelRate, fieldRate);

        // the message label does not require any bottom padding as it is the
        // last component in the panel
        addLabel(topPanel, labelMessage, 0, LFPAD * 2);

        return topPanel;
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
        // 3D). we don't strictly need the grid in this Applet because we only
        // have one command button, but so we don't forget to add it later...
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
    protected void addLabel(final JPanel panel,
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
    protected void addField(final JPanel panel, final JLabel label,
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
     * This inner class watches for focus events from the text components
     * found within this applet. If the focus is gained for a text component
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

        public void focusLost(final FocusEvent event) {
            // change our message to indicate that the user should recalculate
            // the mortgage whenever they leave the field... technically this
            // should only occur when the field value changes, but this is a
            // Good Enough implementation for now...
            labelMessage.setText(MSG_CALCULATE);
        }
    }

    /**
     * This inner class is the {@link Action} provider for the Calculate
     * button. When the user activates this action a new mortgate will be
     * created based on their current input and a new mortgage payment will be
     * calculated.
     */
    private class CalculateAction extends AbstractAction {
        public CalculateAction() {
            putValue(Action.NAME, LABEL_CALCULATE);
        }

        public void actionPerformed(final ActionEvent event) {
            // fetch the new values from the data entry fields and calculate a
            // new mortgage
            final double principal = ((Number) fieldPrincipal.getValue()).doubleValue();
            final double rate = ((Number) fieldRate.getValue()).doubleValue();
            final int term = ((Number) fieldTerm.getValue()).intValue();
            final Mortgage mortgage = new Mortgage(principal, rate, term);

            // format the mortgage payment into a user-friendly message
            final Object[] values = { new Double(mortgage.getPayment()) };
            final String message = MessageFormat.format(MSG_PAYMENT, values);

            // update our message label and force our Applet to repaint itself
            // with the new mortgage results
            labelMessage.setText(message);
            repaint();
        }
    }
}
