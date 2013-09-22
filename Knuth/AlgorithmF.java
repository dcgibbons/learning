/*
 * Algorithm F (Euclid's algorithm). Given two positive integers m and n, 
 * find their greatest common divisor, that is, the largest positive 
 * integer that evenly divides both m and n.
 *
 * The Art of Computer Programming, Volume 1, Fundamental Algorithms
 * Third Edition
 * Donald E. Knuth
 * Pg. 2
 *
 * David C. Gibbons
 * September 28, 2006
 */

public class AlgorithmF {
    private static final String USAGE = "java AlgorithmF [m] [n]";

    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println(USAGE);
            System.exit(1);
        } else {
            int m = Integer.parseInt(args[0]);
            int n = Integer.parseInt(args[1]);
            int gcd = AlgorithmF.findGCD(m, n);

            System.out.printf("GCD of m(%d) and n(%d) is %d\n", 
                    m, n, gcd);
        }
    }

    public static int findGCD(int m, int n) {
        int r;
        while ((r = m % n) != 0) {
            m = n;
            n = r;
        }
        
        return n;
    }
}
