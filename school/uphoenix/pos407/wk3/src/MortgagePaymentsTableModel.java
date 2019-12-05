/*
 * MortgagePaymentTableModel.java
 * Week 3 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-17 | Initial version.
 */

import javax.swing.table.AbstractTableModel;

/**
 * This custom {@link javax.swing.table.TableModel} implementation provides
 * for a simple table of mortgage payment detail using an array of
 * {@link MortgagePayment} objects as a raw data source.
 */
public class MortgagePaymentsTableModel extends AbstractTableModel {
    private static final int COL_PAYMENT = 0;
    private static final int COL_PRINCIPAL = 1;
    private static final int COL_INTEREST = 2;
    private static final int COL_REMAINING_BALANCE = 3;

    private static final String COLUMN_NAMES[] = {
            "Payment #",
            "Principal",
            "Interest",
            "Remaining Balance"
    };

    private MortgagePayment[] payments;

    /**
     * Constructs a new instance of the model and ensures that the current
     * mortgage payment schedule is an empty array.
     */
    MortgagePaymentsTableModel() {
        payments = new MortgagePayment[0];
    }

    /**
     * Sets a new set of data for this table model. Any listeners waiting for
     * the table data to change will be notified.
     * @param payments the mortgage payment detail to model
     */
    public void setPayments(final MortgagePayment[] payments) {
        this.payments = payments;
        fireTableDataChanged();
    }

    /**
     * {@inheritDoc}
     */
    public int getRowCount() {
        return payments.length;
    }

    /**
     * {@inheritDoc}
     */
    public int getColumnCount() {
        return COLUMN_NAMES.length;
    }

    /**
     * {@inheritDoc}
     */
    public String getColumnName(final int column) {
        return COLUMN_NAMES[column];
    }

    /**
     * {@inheritDoc}
     */
    public Class getColumnClass(final int column) {
        Class cls;
        // all of the columns except for the Payment # column are Double objects
        switch (column) {
        case COL_PAYMENT:
            cls = Integer.class;
            break;
        default:
            cls = Double.class;
            break;
        }
        return cls;
    }

    /**
     * {@inheritDoc}
     */
    public Object getValueAt(int row, int column) {
        Object value = null;
        switch (column) {
        case COL_PAYMENT:
            value = new Integer(row + 1); // humans don't like zero-based
            break;
        case COL_PRINCIPAL:
            value = payments[row].getPrincipal();
            break;
        case COL_INTEREST:
            value = payments[row].getInterest();
            break;
        case COL_REMAINING_BALANCE:
            value = payments[row].getRemainingBalance();
            break;
        }
        return value;
    }
}
