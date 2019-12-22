import java.util.*;
import static java.lang.System.out;

public class Permute {
  public static void main(String... args) {
    String str = (args.length > 0) ? args[0] : "abcd";
    final int strlen = str.length();

    int permutations = factorial(str.length());
    out.println("Number of permutations in input string \"" + str + "\" : " + permutations);

    boolean[] used = new boolean[strlen];
    List<String> permutationStrings = new ArrayList<String>(permutations);
    StringBuilder buffer = new StringBuilder(strlen);

    permute(str, strlen, used, buffer, permutationStrings, 0);

    for (int i = 0; i < permutations; i++) {
      out.println("[" + i + "]=" + permutationStrings.get(i));
    }
  }

  public static int factorial(int n) {
    int f = 1;
    while (n > 1) {
      f *= n--;
    }
    return f;
  }

  public static void permute(final String str, 
                             final int strlen, 
                             final boolean[] used, 
                             final StringBuilder buffer,
                             final List<String> permutationStrings,
                             final int level) {
    assert(level <= strlen);
    assert(used.length == strlen);
    assert(str.length() == strlen);

    // have we completed a permutation?
    if (level == strlen) {
      permutationStrings.add(buffer.toString());
    } else {
      for (int i = 0; i < strlen; i++) {
        if (!used[i]) {
          used[i] = true;
          buffer.append(str.charAt(i));
          permute(str, strlen, used, buffer, permutationStrings,  level + 1);
          buffer.setLength(level);
          used[i] = false;
        }
      }
    }
  }
}

