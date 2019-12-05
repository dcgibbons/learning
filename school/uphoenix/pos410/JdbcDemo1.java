import java.sql.*;

public class JdbcDemo1 {
  public static void main(String[] args) 
      throws ClassNotFoundException, SQLException {
    if (args.length != 4) {
      System.err.println("Usage: java JdbcDemo1 [JDBC Driver Class Name] [JDBC URL] [JDBC Username] [JDBC Password]");
      System.exit(1);
    }

    // assign our database information based on the command-line arguments
    String driverClassName = args[0];
    String jdbcUrl = args[1];
    String jdbcUsername = args[2];
    String jdbcPassword = args[3];

    // load the JDBC Driver from the classpath
    Class.forName(driverClassName);

    // get a connection to a specific database instance
    Connection conn = DriverManager.getConnection(jdbcUrl,
                                                  jdbcUsername, 
                                                  jdbcPassword);

    // allocate a Statement from the Connection and then execute our query
    Statement stmt  = conn.createStatement();
    ResultSet results = stmt.executeQuery("SELECT * FROM employee");

    // iterate over the results of the query and display the results
    while (results.next()) {
      String ssn = results.getString("ssn");
      String lastName = results.getString("last_name");
      String firstName = results.getString("first_name");
      String middleName = results.getString("middle_name");

      System.out.println("SSN:  " + ssn + " \t" +
                         "Name: " + lastName + ", " + 
                                    firstName + " " + middleName);
    }

    // cleanup all resources
    results.close();
    stmt.close();
    conn.close();
  }
}
