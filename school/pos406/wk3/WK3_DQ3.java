import java.io.*;

public class WK3_DQ3 {
    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String line;
        while ((line = in.readLine()) != null) {
            // remove all whitespace and convert to all lowercase
            line = line.trim().toLowerCase(); 

            // skip this line if there was no data entered
            if (line.length() == 0) {
                continue;
            }

            if (line.equals("quit")) {
                break; // exit
            } else if (line.equals("stats")) {
                // display statistics here
            } else if (line.equals("load")) {
                // load firmware command
            } else if (line.equals("save")) {
                // save firmware command
            } else {
                // display invalid command message
            }
        }
    }
}
