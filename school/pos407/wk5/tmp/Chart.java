/*
 * Chart.java
 * Week 5 Programming Assignment - Mortgage Payment Calculator
 * POS/407 - Computer Programming II
 * David C. Gibbons, dcgibbons@email.uophx.edu
 *
 * Version | Date       | Description
 * --------|------------|-----------------------------------------------------
 *   1.00  | 2006-01-28 | Initial version.
 *   1.10  | 2006-02-05 | Added a highlightXValue variable that allows the Chart
 *                      | to be told by the caller at what X-Axis value the
 *                      | data sets should be highlighted.
 */

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.GeneralPath;
import java.text.NumberFormat;

/**
 * This class provides simple charting using multiple data sets. It is assumed
 * that the X-axis consists of values from 1 to N based on the number of
 * elements in the data sets themselves. The values in the data sets are the
 * Y axis values.
 *
 * The chart will use an initial size for computing its minimum and preferred
 * sizes, but will resize gracefully to any size it is set to. Space for the
 * legend is always calculated first, so it is possible that a very large legend
 * will leave no room for the chart itself and will cause the chart to be
 * clipped.
 *
 * In this implementation any number of data sets is theoretically supported,
 * but in practice only 3 maximum will work.
 */
public class Chart extends JComponent {
    private static final int X_AXIS = 0;
    private static final int Y_AXIS = 1;

    /**
     * The following constant defines the default color to use for the legend
     * text and axis lines.
     */
    private static final Color LEGEND_COLOR = Color.BLACK;

    /**
     * The following array of colors are the default color values to use for
     * each data set.
     */
    private static final Color[] DEFAULT_COLORS = {
        Color.BLUE, Color.RED, Color.DARK_GRAY
    };

    /**
     * The following font is the default used for drawing the legend text.
     */
    private static final Font chartFont = new Font("Dialog", Font.BOLD, 10);

    /**
     * A data set consists of the raw data values, the name of each data set,
     * and the formatter to use for each the legend of each axis.
     */
    private double[][] data;
    private String[] dataNames;
    private NumberFormat[] axisFormats;

    /**
     * The following variables define the maximum X and Y values calculated
     * from the user provided data sets. These values are used when determining
     * the appropriate scale factor for the chart.
     */
    private double dataMaxX;
    private double dataMaxY;

    /**
     * A variable that defines the X-Axis point that should be highlighted on
     * the chart.
     */
    private int highlightXValue = -1;

    /**
     * Construts a chart component using the specified initial sizes. The
     * minimum and preferred sizes will be set to this initial size.
     *
     * @param width     the width of the component in pixels
     * @param height    the height of the component in pixels
     */
    public Chart(final int width, final int height) {
        final Dimension initialSize = new Dimension(width, height);
        setMinimumSize(initialSize);
        setPreferredSize(initialSize);

        clearData();
    }

    /**
     * Clears the chart of any data so no chart will be drawn.
     */
    public void clearData() {
        data = new double[0][0];
        dataNames = new String[0];
        axisFormats = new NumberFormat[0];
    }

    /**
     * Provides multiple data sets for the chart component to use in drawing.
     * Multiple data sets can be provided as long as the number does not exceed
     * the available colors to draw with.
     *
     * @param data an array of data sets, with each array containing an array
     *             of values that will be plotted on the chart
     * @param highlightXValue the X-Axis value that should be highlighted on the
     *                        chart
     * @param dataNames an array of data set names that will be used when
     *                  creating the chart legend
     * @param axisFormats the formatting objects to use when displaying the
     *                    various legend values for each axis
     */
    public void setData(final double[][] data,
                        final int highlightXValue,
                        final String[] dataNames,
                        final NumberFormat axisFormats[]) {
        this.data = data;
        this.dataNames = dataNames;
        this.axisFormats = axisFormats;
        this.highlightXValue = highlightXValue;

        if (data.length > DEFAULT_COLORS.length) {
            throw new IllegalArgumentException("too many data sets provided");
        } else if (data.length != dataNames.length) {
            throw new IllegalArgumentException("data set names do not match data");
        } else if (axisFormats.length != 2) {
            throw new IllegalArgumentException("insufficent axis format values");
        }

        // only if the caller provided actual data should be calculate any
        // maximums
        if (data.length > 0) {
            // determine the maximum Y and X values
            double maxY = 0.0;
            long maxX = 0;
            for (int set = 0; set < data.length; set++) {
                for (int value = 0; value < data[set].length; value++) {
                    maxY = Math.max(data[set][value], maxY);
                    maxX = value;
                }
            }

            dataMaxX = maxX + 1.0;      // make the max X value 1-based instead of 0-based
            dataMaxY = Math.ceil(maxY); // raise to the next ceiling value for user-friendlyness
        }
    }

    /**
     * {@inheritDoc}
     */
    public void paintComponent(final Graphics g) {
        // give our parent component a chance to paint itself properly
        super.paintComponent(g);

        // don't bother with any calculations if there is no data to plot
        if (data.length == 0) {
            return;
        }

        final Graphics2D g2d = (Graphics2D) g;

        // turn anti-aliasing on so our lines don't look so jagged
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                             RenderingHints.VALUE_ANTIALIAS_ON);

        // save the original transform, color, and stroke of the graphics
        // context so it can be restored when we are done plotting
        final AffineTransform origTransform = g2d.getTransform();
        final Color origColor = g2d.getColor();
        final Stroke origStroke = g2d.getStroke();

        // calculate the legends for the min and max values of both the X and
        // Y axis; these will be used to calculate the amount of space needed
        // for the axis legend
        final String xLegendMin = axisFormats[X_AXIS].format(1);
        final String xLegendMax = axisFormats[X_AXIS].format(dataMaxX);

        final String yLegendMin = axisFormats[Y_AXIS].format(1.00);
        final String yLegendMax = axisFormats[Y_AXIS].format(dataMaxY);

        // retrieve the font metrics for our chart font so it can be used in
        // calculating the legend sizes
        final FontMetrics fm = g2d.getFontMetrics(chartFont);
        final int spaceWidth = fm.charWidth(' ');
        final int fontHeight = fm.getHeight();

        // calculate the starting X position of the graph based on the maximum
        // width needed for the Y-axis legends on the left side of the graph,
        // plus 5 spaces for padding.
        final int xStart = Math.max(fm.stringWidth(yLegendMin), fm.stringWidth(yLegendMax)) + spaceWidth * 5;

        // calculate the starting Y position of the graph based on how many
        // lines of text will be needed at the bottom of the graph to draw the
        // legend
        final float yStart = fontHeight * 3;

        // determine the total size of the current drawing area
        final Dimension size = getSize();
        final float xEnd = size.width;
        final float yEnd = size.height;

        // create a transform so that the graph portion of the chart can be
        // drawn using a bottom-left origin instead of the default upper-left
        // NOTE: we don't draw the text using this transform because it would
        // cause the text to appear upside down
        final AffineTransform graphTransform = new AffineTransform(origTransform);
        graphTransform.translate(0, size.height);
        graphTransform.scale(1.0, -1.0);

        // set the font and stroke that will be used for the rest of the chart
        g2d.setFont(chartFont);
        g2d.setStroke(new BasicStroke(1.0F));

        // plot the X and Y axis lines using the graph transform
        g2d.setTransform(graphTransform);
        plotAxisLines(g2d, xStart, yStart, xEnd, yEnd);
        g2d.setTransform(origTransform);

        // plot the legends for the chart
        plotLegends(g2d, xStart, xEnd, yStart, yEnd, fm,
                    xLegendMin, xLegendMax, yLegendMin, yLegendMax);

        // determine how many pixel units equal one user space coordinate
        // TODO: could we just use the transform's scale method instead?
        double xStep = dataMaxX / (xEnd - xStart);
        double yStep = dataMaxY / (yEnd - yStart);

        // plot the data sets using the graph transform
        g2d.setTransform(graphTransform);
        for (int set = 0; set < data.length; set++) {
            plotData(data[set],
                     xStep, xStart,
                     yStep, yStart,
                     g2d,
                     DEFAULT_COLORS[set]);
        }

        // restore the original transform
        g2d.setTransform(origTransform);

        // plot the intersections of the data sets
        if (highlightXValue >= 0) {
            plotIntersects(highlightXValue, xStep, xStart, xEnd, yStep, yStart, yEnd, g2d, fm);
        }

        // restore the original pen and color
        g2d.setStroke(origStroke);
        g2d.setColor(origColor);
    }

    /**
     * Plots the X and Y axis lines for the chart.
     * @param g2d       the graphics context to draw into
     * @param xStart    the X starting position of the chart
     * @param yStart    the Y starting position of the chart
     * @param xEnd      the X ending position of the chart
     * @param yEnd      the Y ending position of the chart
     */
    private void plotAxisLines(final Graphics2D g2d,
                               final float xStart,
                               final float yStart,
                               final float xEnd,
                               final float yEnd) {
        final Color origColor = g2d.getColor();
        g2d.setColor(LEGEND_COLOR);

        Line2D hLn = new Line2D.Float(xStart, yStart, xEnd, yStart);
        g2d.draw(hLn);

        Line2D vLn = new Line2D.Float(xStart, yStart, xStart, yEnd);
        g2d.draw(vLn);

        g2d.setColor(origColor);
    }

    /**
     * Plots a single data set as a series of connected line segments.
     *
     * @param data      the data set; x = array index, y = data value
     * @param xStep     the number of X pixels per user space unit
     * @param xStart    the pixel position to start drawing in the X axis
     * @param yStep     the number of Y pixels per user space unit
     * @param yStart    the pixel position to start drawing in the Y axis
     * @param g2d       the graphics context component
     * @param color     the color of the line
     */
    private void plotData(final double[] data,
                          final double xStep,
                          final float xStart,
                          final double yStep,
                          final float yStart,
                          final Graphics2D g2d,
                          final Color color) {
        // create an array of 2D point objects to represent the user space
        // coordinates of each data item
        final Point2D[] points = new Point2D[data.length];
        for (int i = 0; i < data.length; i++) {
            double x = i / xStep + xStart;
            double y = data[i] / yStep + yStart;
            points[i] = new Point2D.Double(x, y);
        }

        // create a GeneralPath from all of our data points that will
        // essentially draw a line between all of the points
        final GeneralPath path = new GeneralPath();
        path.moveTo((float)points[0].getX(), (float)points[0].getY());
        for (int point = 1; point < points.length; point++) {
            path.lineTo((float)points[point].getX(), (float)points[point].getY());
        }

        // finally, draw the actual path using the appropraite color
        final Color origColor = g2d.getColor();
        g2d.setColor(color);
        g2d.draw(path);
        g2d.setColor(origColor);
    }

    /**
     * Plot the legends for each of the data sets.
     *
     * @param g2d           the graphics context to draw into
     * @param xStart        the starting X position of the chart
     * @param xEnd          the ending X position of the chart
     * @param yStart        the starting Y position of the chart
     * @param yEnd          the ending Y position of the chart
     * @param fm            the font metrics for our current font
     * @param xLegendMin    the text for the minimum X legend value
     * @param xLegendMax    the text for the maximum X legend value
     * @param yLegendMin    the text for the minimum Y legend value
     * @param yLegendMax    the text for the maximum Y legend value
     */
    private void plotLegends(final Graphics2D g2d,
                             final float xStart,
                             final float xEnd,
                             final float yStart,
                             final float yEnd,
                             final FontMetrics fm,
                             final String xLegendMin,
                             final String xLegendMax,
                             final String yLegendMin,
                             final String yLegendMax) {
        // calculate some text metrics that will be used in computing offsets
        // of the different legend values
        final int fontHeight = fm.getHeight();
        final int spaceWidth = fm.charWidth(' ');

        final Color origColor = g2d.getColor();
        g2d.setColor(LEGEND_COLOR);

        // draw the minimum X legend value at the bottom-left corner
        g2d.drawString(xLegendMin,
                       xStart,
                       yEnd - fontHeight * 2);

        // draw the maximum X legend value at the bottom-right corner
        g2d.drawString(xLegendMax,
                       xEnd - fm.stringWidth(xLegendMax) - spaceWidth,
                       yEnd - fontHeight * 2);

        // draw the minimum Y legend value at the bottom-left corner
        g2d.drawString(yLegendMin,
                       xStart - fm.stringWidth(yLegendMin) - spaceWidth,
                       yEnd - yStart);

        // draw the maximum Y legend value at the upper-left corner
        g2d.drawString(yLegendMax,
                       xStart - fm.stringWidth(yLegendMax) - spaceWidth,
                       fontHeight);

        // draw each of the legend names and lines on the bottom of the graph
        // going across the X axis.
        // TODO: add support here for wrapping across additional legend lines
        float x = xStart;
        float y = yEnd - fm.getHeight();
        for (int set = 0; set < dataNames.length; set++) {
            // draw the legend name using the approriate color for this data
            // set and move the x position over by the length of the legend
            // text plus one space
            g2d.setColor(DEFAULT_COLORS[set]);
            g2d.drawString(dataNames[set], x, y);
            x += fm.stringWidth(dataNames[set]) + spaceWidth;

            // calculate the line segment for this data set's legend
            final float lineY = y - fm.getDescent(); // up just a little
            final float lineX = x + spaceWidth * 5.0F; // 5 spaces worth
            final Line2D line = new Line2D.Double(x, lineY, lineX, lineY);
            g2d.draw(line);

            // move our next legend position to the end of the line plus 5
            // spaces worth of pixels
            x = lineX + spaceWidth * 5.0F;
        }

        g2d.setColor(origColor);
    }

    /**
     * Plots the intersection point between data sets.
     *
     * @param xStep     the number of X pixels per user space unit
     * @param xStart    the pixel position to start drawing in the X axis
     * @param yStep     the number of Y pixels per user space unit
     * @param yStart    the pixel position to start drawing in the Y axis
     * @param yEnd      the maximum Y value for the chart area
     * @param g2d       the graphics context to draw into
     * @param fm        the font metrics for the current font
     */
    private void plotIntersects(final int xValue,
                                final double xStep,
                                final float xStart,
                                final float xEnd,
                                final double yStep,
                                final float yStart,
                                final float yEnd,
                                final Graphics2D g2d,
                                final FontMetrics fm) {
        // TODO: this algorithm only works for two data sets
        final double xIntersect = xValue / xStep + xStart;
        final double yIntersect = data[1][xValue] / yStep + yStart;

        // calculate the intersection point - note that Y position is offset
        // from the end of the graphics area so that we can plot in the same
        // location that our graph transform is plotting the data, but we
        // aren't using the transform here so that our text will show up
        // correctly
        final Point2D pt = new Point2D.Double(xIntersect, yEnd - yIntersect);

        final Color origColor = g2d.getColor();
        g2d.setColor(LEGEND_COLOR);

        // draw a small circle around the intersection point
        g2d.drawOval((int)pt.getX() - 5, (int)pt.getY() - 5, 10, 10);

        // calculate where the text should be drawn near the intersection point
        final int fontHeight = fm.getHeight();
        final int spaceWidth = fm.charWidth(' ');
        final float xPos = (float) pt.getX() + spaceWidth * 5.0F;
        float yPos = (float) pt.getY() - fontHeight;

        // plot the X and Y values near the intersection point
        g2d.drawString(axisFormats[X_AXIS].format(xValue),
                       xPos, yPos);
        yPos += fm.getHeight();
        for (int set = 0; set < data.length; set++) {
            final String val = dataNames[set] + " "
                    + axisFormats[Y_AXIS].format(data[set][xValue]);
            g2d.drawString(val, xPos, yPos);
            yPos += fontHeight;
        }

        // add a Y axis legend entry for the data intersection point
        // TODO: another example of knowing the data too much; whats a better
        // way?
        String yLegend = axisFormats[Y_AXIS].format(Math.ceil(data[1][xValue]));
        g2d.drawString(yLegend,
                       xStart - fm.stringWidth(yLegend) - spaceWidth,
                       (float) pt.getY());

        g2d.setColor(origColor);
    }
}
