/*
 * Author's Name: David C. Gibbons
 * Assignment:    POS406 - Workshop 4B
 * Creation Date: December 5, 2005
 * Due Date:      December 12, 2005
 */

/**
 * Class for the workshop 4B programming assignment.
 */
public class POS406_W4B_DCG {
    /**
     * main method - entry point for the entire program
     */
    public static void main(String[] args) {
        CAT sally = new CAT();
        CAT harry = new CAT();
        CAT meow = new CAT();

        sally.setAge(3);
        sally.calculate();
        sally.setName("Ms. Sally");

        harry.setAge(8);
        harry.calculate();
        harry.setName("Mr. Harry");

        meow.setAge(5);
        meow.calculate();
        meow.setName("Mr. Meow");

        CAT[] cats = new CAT[3];
        cats[0] = sally;
        cats[1] = harry;
        cats[2] = meow;

        System.out.println("Name       : David C. Gibbons");
        System.out.println("Assignment : Workshop 4B     ");
        System.out.println("-----------------------------");
        System.out.println("Object Oriented Programming Examples");
        System.out.println();
        for (int cat = 0; cat < cats.length; cat++) {
            cats[cat].displayInfo();
        }
    }
}

class CAT {
    private int age;
    private int livesLeft;
    private String name;

    public void setName(String newName) {
        name = newName;
    }

    public void setAge(int age_value_passed_from_object) {
        age = age_value_passed_from_object;
    }

    public void calculate() {
        livesLeft = age + 1;
    }

    public void displayInfo() {
        System.out.println("Detail");
        System.out.println("Name is : " + name);
        System.out.println("Age is : " + age);
        System.out.println("Lives Left : " + livesLeft);
        System.out.println();
    }
}

