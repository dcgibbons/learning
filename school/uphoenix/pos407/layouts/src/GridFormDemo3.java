import javax.swing.*;
import java.awt.*;

public class GridFormDemo3 {
  public static void main(String[] args) {
    new GridFormDemo3();
  }

  /**
   * form variables
   */
  private JFrame mainFrame;

  private JTextField lastNameField;
  private JTextField firstNameField;
  private JTextField middleNameField;

  private JLabel lastNameLabel;
  private JLabel firstNameLabel;
  private JLabel middleNameLabel;

  private JButton okButton;
  private JButton cancelButton;

  private GridFormDemo3() {
    createComponents();
    layoutComponents();

    mainFrame.pack();
    mainFrame.setVisible(true);
  }

  private void createComponents() {
    mainFrame = new JFrame("Grid Form Demo");
    mainFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

    lastNameField = new JTextField(32);
    lastNameLabel = new JLabel("Last Name:");

    firstNameField = new JTextField(32);
    firstNameLabel = new JLabel("First Name:");

    middleNameField= new JTextField(32);
    middleNameLabel = new JLabel("Middle Name:");

    okButton = new JButton("OK");
    cancelButton = new JButton("Cancel");
  }

  private void layoutComponents() {
    JPanel labelGridPanel = new JPanel(new GridLayout(0, 1));
    labelGridPanel.add(lastNameLabel);
    labelGridPanel.add(firstNameLabel);
    labelGridPanel.add(middleNameLabel);

    JPanel fieldGridPanel = new JPanel(new GridLayout(0, 1));
    fieldGridPanel.add(lastNameField);
    fieldGridPanel.add(firstNameField);
    fieldGridPanel.add(middleNameField);

    JPanel p1 = new JPanel(new BorderLayout());
    p1.add(labelGridPanel, BorderLayout.WEST);
    p1.add(fieldGridPanel, BorderLayout.CENTER);

    JPanel buttonGridPanel = new JPanel(new GridLayout(1, 0));
    buttonGridPanel.add(okButton);
    buttonGridPanel.add(cancelButton);
    JPanel buttonPanel = new JPanel(new BorderLayout());
    buttonPanel.add(buttonGridPanel, BorderLayout.EAST);

    // layout the fields and the buttons onto the main content panel
    JPanel contentPanel = (JPanel) mainFrame.getContentPane();
    contentPanel.setLayout(new BorderLayout());
    contentPanel.add(p1, BorderLayout.NORTH);
    contentPanel.add(buttonPanel);
  }
}
