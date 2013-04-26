import java.awt.*;
import javax.swing.*;
import java.awt.geom.*;

public class ChartTest extends JComponent {
  public static void main(String[] args) {
    ChartTest chart = new ChartTest();
    JFrame frame = new JFrame("Chart Test");
    JPanel content = (JPanel) frame.getContentPane();
    content.setLayout(new BorderLayout());
    content.add(new JLabel("This is a chart!"), BorderLayout.NORTH);
    content.add(chart, BorderLayout.CENTER);
    frame.pack();
    frame.setVisible(true);
  }

  public ChartTest() {
    Dimension size = new Dimension(320, 240);
    setMinimumSize(size);
    setPreferredSize(size);
  }

  public void paintComponent(Graphics g) {
    super.paintComponent(g);
    Graphics2D g2d = (Graphics2D)g;

    g2d.setColor(Color.BLUE);
    BasicStroke pen = new BasicStroke(1.0F);
    g2d.setStroke(pen);

    // draw axis lines
    Dimension size = getSize();
    float xStart = 80.0F;
    float xEnd = size.width;
    float yStart = 80.0F;
    float yEnd = size.height;
    float xLegend = 65.0F;
    Line2D hLn = new Line2D.Float(xStart, yStart, xEnd, yStart);
    g2d.draw(hLn);
    Line2D vLn = new Line2D.Float(xStart, yStart, xStart, yEnd);
    g2d.draw(vLn);

    Font chartFont = new Font("Dialog", Font.BOLD, 10);
    g2d.setFont(chartFont);
    g2d.setColor(Color.BLUE);

    g2d.drawString("$0.00", xStart - xLegend, yStart);
  }
}

