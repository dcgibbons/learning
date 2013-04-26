import java.io.*;

public class FileDemo {
  public static void main(String[] args) throws IOException {
    String fileName = "filedemo.txt";

    FileOutputStream fout = new FileOutputStream(fileName);
    BufferedOutputStream bout = new BufferedOutputStream(fout);
    PrintStream pout = new PrintStream(bout);

    pout.println("hello, world!");
    pout.println();
    pout.println("goodbye, cruel world!");

    pout.close();
  }
}
