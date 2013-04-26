import javax.swing.*;

public class GridFormDemoTemplate {
  public static void main(String[] args) {
    new GridFormDemoTemplate();
  }

  private JFrame mainFrame;

  private GridFormDemoTemplate() {
    createComponents();
    layoutComponents();

    mainFrame.pack();
    mainFrame.setVisible(true);
  }

  private void createComponents() {
    mainFrame = new JFrame("Grid Form Demo");
    mainFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
  }

  private void layoutComponents() {
  }
}
