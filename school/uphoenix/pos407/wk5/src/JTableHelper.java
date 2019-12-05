/*
 * JTableHelper.java
 * Week 5 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-17 | Initial version.
 *   1.10  | 2006-01-28 | Fixed bug where insets of JScrollBar objects weren't
 *                      | being considered when calculating preferred viewport
 *                      | size.
 */

import javax.swing.*;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import java.awt.*;
import java.awt.event.KeyEvent;

/**
 * This class subclasses the {@link javax.swing.JTable} class and provides useful utility
 * methods for manipulating the table for a better user experience. Methods
 * are provided to ease sizing of the table along with better keyboard event
 * management.
 */
public class JTableHelper extends JTable {
    /**
     * This constant defines the maximum number of rows that the column resize
     * calculation will examine. This limit prevents the resize calculation from
     * taking up too much CPU time with a large table.
     */
    private static final int MAX_RESIZE_ROWS = 25;

    /**
     * Constructs a new instance of the class from the specified table model.
     *
     * @param tm the table model to use when creating the parent {@link javax.swing.JTable}
     */
    public JTableHelper(final TableModel tm) {
        super(tm);
    }

    /**
     * Resizes all of the table's columns based on the maximum width of either
     * the header text or of the data itself. Only a limited number of data
     * rows will be examined to prevent excessive CPU consumption.
     */
    public void resizeTableColumns() {
        final JTable table = this;

        final TableModel tm = table.getModel();
        final int rowCount = Math.min(tm.getRowCount(), MAX_RESIZE_ROWS);
        final int columnCount = tm.getColumnCount();
        final int[] headerWidths = new int[columnCount];
        final int[] maximumWidths = new int[columnCount];

        final TableColumnModel columnModel = table.getColumnModel();

        // determine what the width of each column's header text is
        for (int column = 0; column < columnCount; column++) {
            final TableColumn tableColumn = columnModel.getColumn(column);

            // get the table header renderer so that the preferred size of the
            // header data can be calculated
            TableCellRenderer headerRenderer = tableColumn.getHeaderRenderer();
            if (headerRenderer == null) {
                headerRenderer = table.getTableHeader().getDefaultRenderer();
            }

            // calculate the header width including any insets
            final Component headerComponent = headerRenderer.getTableCellRendererComponent(
                    table, tableColumn.getHeaderValue(), false, false, -1,
                    column);
            final Insets insets = ((JComponent) headerComponent).getInsets();
            headerWidths[column] = headerComponent.getPreferredSize().width + insets.left + insets.right;
        }

        // determine what the width of each row's column data is
        for (int row = 0; row < rowCount; row++) {
            for (int column = 0; column < columnCount; column++) {
                // get the table cell renderer so that the preferred size of
                // the column data can be calculated, including any insets
                final Object value = tm.getValueAt(row, column);
                final TableCellRenderer renderer = table.getCellRenderer(row,
                                                                         column);
                final Component renderedComponent =
                        renderer.getTableCellRendererComponent(table, value,
                                                               false, false,
                                                               row, column);
                final Insets insets = ((JComponent) renderedComponent).getInsets();

                // calculate the component width for this row's column and then
                // save it if it is the new maximum width for the whole column
                final int componentWidth = renderedComponent.getPreferredSize().width + insets.left + insets.right;
                maximumWidths[column] = Math.max(componentWidth,
                                                 maximumWidths[column]);
            }
        }

        // set the preferred width of each column to be either the header width
        // or the width of the data
        for (int column = 0; column < columnCount; column++) {
            final TableColumn tableColumn = columnModel.getColumn(column);
            final int preferredWidth = Math.max(headerWidths[column],
                                                maximumWidths[column]);
            tableColumn.setPreferredWidth(preferredWidth);
        }
    }

    /**
     * Limit the size of the table's scrollable viewport to be the specified
     * number of table rows, rather than some arbitrary pixel size that is used
     * by {@link javax.swing.JTable} by default.
     *
     * @param rows                        the number of data rows to make the
     *                                    viewport
     * @param spaceForVerticalScrollBar   true if space for a vertical scrollbar
     *                                    should be included
     * @param spaceForHorizontalScrollBar true if space for a horizontal
     *                                    scrollbar should be included
     */
    public void limitTableViewport(final int rows,
                                   final boolean spaceForVerticalScrollBar,
                                   final boolean spaceForHorizontalScrollBar) {
        // calculate the viewport height necessary by computing taking into
        // account any margin space between rows
        final int rowHeight = getRowHeight();
        final int rowMargin = getRowMargin();
        final int viewportHeight = (rowHeight + rowMargin) * rows;

        // calculate any extra width needed for the scrollbar
        int extraWidth = 0;
        if (spaceForVerticalScrollBar) {
            final JScrollBar verticalScrollBar = new JScrollBar(JScrollBar.VERTICAL);
            final Dimension sbPreferredSize = verticalScrollBar.getPreferredSize();
            final Insets insets = verticalScrollBar.getInsets();
            extraWidth = sbPreferredSize.width + insets.left + insets.right;
        }

        // calculate any extra height needed for the scrollbar
        int extraHeight = 0;
        if (spaceForHorizontalScrollBar) {
            final JScrollBar horizontalScrollBar = new JScrollBar(JScrollBar.HORIZONTAL);
            final Dimension sbPreferredSize = horizontalScrollBar.getPreferredSize();
            final Insets insets = horizontalScrollBar.getInsets();
            extraHeight = sbPreferredSize.height + insets.top + insets.bottom;
        }

        // determine the current preferred size of our table, and then set the
        // preferred viewport size to be that width by our calculated height,
        // plus any extra width or height necessary for scrollbars.
        final Dimension preferredSize = getPreferredSize();
        final Dimension viewportSize = new Dimension(
                preferredSize.width + extraWidth,
                viewportHeight + extraHeight);
        setPreferredScrollableViewportSize(viewportSize);
    }

    /**
     * When a {@link javax.swing.JTable} has focus it automatically processes certain
     * keyboard events without passing them on to the rest of the component
     * heirarchy. This can cause problems when the table has focus in that the
     * Escape and Enter keys can no longer be processed by the window or dialog.
     * With this modification, we only allow the JTable instance to process
     * those keyboard events if the table is actually editing a column of data.
     * Otherwise, those events should be passed on to the component heirarchy
     * for processing.
     */
    protected boolean processKeyBinding(final KeyStroke ks,
                                        final KeyEvent evt,
                                        final int condition,
                                        final boolean pressed) {
        boolean process = false;
        // don't allow the Escape key to be processed by the parent table
        // (and thus ignored!) unless a cell is being edited
        final char keyChar = evt.getKeyChar();
        if (isEditing() || (keyChar != KeyEvent.VK_ESCAPE && keyChar != KeyEvent.VK_ENTER))
        {
            process = super.processKeyBinding(ks, evt, condition, pressed);
        }
        return process;
    }
}
