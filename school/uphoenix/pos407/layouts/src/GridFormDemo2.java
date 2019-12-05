import java.awt.*;
import javax.swing.*;

public class GridFormDemo2 {
  public static void main(String[] args) {
    GridFormDemo2 demo = new GridFormDemo2();
  }

  private JFrame mainFrame;

  private JTextField lastNameField;
  private JTextField firstNameField;
  private JTextField middleNameField;

  private JLabel lastNameLabel;
  private JLabel firstNameLabel;
  private JLabel middleNameLabel;

  private JButton okButton;
  private JButton cancelButton;

  private GridFormDemo2() {
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
    JPanel fieldPanel = new JPanel(new GridBagLayout());
    addField(fieldPanel, lastNameLabel, lastNameField);
    addField(fieldPanel, firstNameLabel, firstNameField);
    addField(fieldPanel, middleNameLabel, middleNameField);

    JButton[] buttons = { okButton, cancelButton };
    JPanel buttonPanel = layoutButtons(buttons);

    JPanel contentPanel = (JPanel) mainFrame.getContentPane();
    contentPanel.setLayout(new BorderLayout());
    contentPanel.add(fieldPanel, BorderLayout.NORTH);
    contentPanel.add(buttonPanel, BorderLayout.SOUTH);
  }

  private void addField(JPanel panel, JLabel label, JComponent field) {
    GridBagConstraints gbc = new GridBagConstraints();

    gbc.anchor = GridBagConstraints.EAST;
    gbc.fill = GridBagConstraints.NONE;
    gbc.gridwidth = 1;
    gbc.weightx = 0.0;
    panel.add(label, gbc);

    gbc.anchor = GridBagConstraints.WEST;
    gbc.fill = GridBagConstraints.HORIZONTAL;
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.weightx = 1.0;
    panel.add(field, gbc);
  }

  private JPanel layoutButtons(JButton[] buttons) {
    JPanel buttonGridPanel = new JPanel(new GridLayout(1, 0));
    for (int i = 0; i < buttons.length; i++) {
      buttonGridPanel.add(buttons[i]);
    }

    JPanel buttonPanel = new JPanel(new BorderLayout());
    buttonPanel.add(buttonGridPanel, BorderLayout.EAST);
    return buttonPanel;
  }
}

