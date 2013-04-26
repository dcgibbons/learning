import java.text.MessageFormat;
public class FixedPoint {
    public static void main(String[] args) {
        int value = 1530932;
        Object[] msgArgs = { new Integer(value/1000), new Integer(value%1000) };
        System.out.println(MessageFormat.format("{0,number,$###0}.{1,number,0000}", msgArgs));
    }
}

