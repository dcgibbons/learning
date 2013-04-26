/*
 * SpeedTest.java
 * Floating-point benchmarking tool
 * POS/406 - Computer Programming I
 * David C. Gibbons, dcgibbons@email.uophx.edu
 * November 20, 2005
 */

import java.text.MessageFormat;

public class SpeedTest {
    public static void main(String[] args) {
        int n = 10;
        if (args.length > 0) {
            n = Integer.parseInt(args[0]);
        }

        long startTime = System.currentTimeMillis();

        double result = Math.PI;
        for (int i = 0; i < n; i++) {
            result += Math.pow(i, Math.PI);
        }

        long endTime = System.currentTimeMillis();

        Object[] msgArgs = { new Double(result), new Long(endTime-startTime) };
        String msgFmt = "Result is {0} and took {1,number,integer} ms";
        System.out.println(MessageFormat.format(msgFmt, msgArgs));
    }
}

