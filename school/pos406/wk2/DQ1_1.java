import java.io.PrintStream;

public class DQ1_1 {
    public static final String NAME_CHAD = "Chad";

    public static void main(String[] args) {
        final PrintStream out = System.out;
        final String xyz = NAME_CHAD;
        final String abc = NAME_CHAD;

        out.println("xyz == abc?      : " + (xyz == abc));
        out.println("xyz.equals(abc)? : " + xyz.equals(abc));
    }
}

