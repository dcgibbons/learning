/*
 * Algorithm E (Euclid's algorithm). Given two positive integers m and n, 
 * find their greatest common divisor, that is, the largest positive 
 * integer that evenly divides both m and n.
 *
 * The Art of Computer Programming, Volume 1, Fundamental Algorithms
 * Third Edition
 * Donald E. Knuth
 * Pg. 13 
 *
 * David C. Gibbons
 * September 29, 2006
 */

public class AlgorithmE2 {
    private static final String USAGE = "java AlgorithmE2 [m] [n]";

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println(USAGE);
            System.exit(1);
        } else {
            int m = Integer.parseInt(args[0]);
            int n = Integer.parseInt(args[1]);
            int gcd = AlgorithmE2.findGCD(m, n);

            System.out.printf("GCD of m(%d) and n(%d) is %d\n", 
                    m, n, gcd);
        }
    }

    public static int findGCD(int m, int n) {
        int a1 = 1;
        int b = 1;
        int a = 0;
        int b1 = 0;
        int c = m;
        int d = n;
        do {
            // E2. [Divide.]
            int q = c / d;
            int r = c % d;

            // E3. [Is it zero?]
            if (r == 0) {
                break;
            }

            // E4. [Recycle.]
            c = d;
            d = r;
            int t = a1;
            a = t - q * a;
            t = b1;
            b1 = b;
            b = t - q * b;
        } while (true);


        System.out.println("a="+a+" b="+b+" d="+d);
        return d;
    }
}
