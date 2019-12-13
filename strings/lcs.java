import static java.lang.System.out;
import java.text.*;

public class lcs {
  public static void main(String[] args) {
    String s1 = "ACCGGTCGAGTGCGCGGAAGCCGGCCGAA";
    String s2 = "GTCGTTCGGAATGCCGTTGCTCTGTAAA";

    lcsLength(s1, s2);
  }

  private static enum Directions { nowhere, W, N, NW } ;

  public static void lcsLength(final String s1, final String s2) {
    final int m = s1.length();
    final int n = s2.length();
    out.println("m="+m+" n="+n);
    int[][] c = new int[m+1][n+1];
    Directions[][] b = new Directions[m+1][n+1];

    for (int i = 1; i <= m; i++) {
      for (int j = 1; j <= n; j++) {
        if (s1.charAt(i-1) == s2.charAt(j-1)) {
          c[i][j] = c[i - 1][j - 1] + 1;
          b[i][j] = Directions.NW;
        } else if (c[i - 1][j] >= c[i][j - 1]) {
          c[i][j] = c[i - 1][j]; 
          b[i][j] = Directions.N;
        } else {
          c[i][j] = c[i][j - 1];
          b[i][j] = Directions.W;
        }
      }
    }

    DecimalFormat df = new DecimalFormat("00 ");
    for (int i = 0; i < c.length; i++) {
      for (int j = 0; j < c[0].length; j++) {
        out.print(df.format(c[i][j]));
      }
      out.println();
    }

    StringBuilder buffer = new StringBuilder(c[m][n]);
    int i = m;
    int j = n;
    while (b[i][j] != null) {
      buffer.append((m > n) ? s1.charAt(i-1) : s2.charAt(j-1));
      switch (b[i][j]) {
        case W:
          j--;
          break;
        case N:
          i--;
          break;
        case NW:
          i--;
          j--;
          break;
      }
    }
    out.println("S1="+s1);
    out.println("S2="+s2);
    out.println("S3="+buffer);

    out.println("S4=GTCGTCGGAAGCCGGCCGAA");
  }
}

