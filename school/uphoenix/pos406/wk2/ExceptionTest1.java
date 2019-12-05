import java.io.*;

public class ExceptionTest1 
{
    public static void main(String[] args) throws IOException
    {
        BufferedReader in = new BufferedReader(new FileReader("input.txt"));
        BufferedWriter out = new BufferedWriter(new FileWriter("output.txt"));

        int lineNo = 0;
        String line;
        while ((line = in.readLine()) != null) 
        {
            out.write("line # " + lineNo + " = " + line);
            out.newLine();
        }
    }
}

