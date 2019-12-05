import java.io.*;

public class ExceptionTest2 
{
    public static void main(String[] args) throws IOException
    {
        try
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
        catch (IOException ex)
        {
            System.err.println("Unable to copy input file to output file: " + ex.getMessage());
            ex.printStackTrace(System.err);
        }
    }
}

