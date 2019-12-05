/*
 * POS406_W1_DCG.java
 * Week 1 Programming Assignment
 * POS/406 - Computer Programming I
 * David C. Gibbons, dcgibbons@email.uophx.edu
 * November 16, 2005
 */

import java.io.PrintStream;
import java.util.Date;

public class POS406_W1_DCG
{
    public static void main(String[] args)
    {
        Date today = new Date();
        String userName = "Chad Gibbons";

        PrintStream out = System.out;
        out.println("\t\t\tWelcome to my day!");
        out.println("\t\t\tDaily planner for " + userName);
        out.println("\t\t\t" + today);
    }
}

