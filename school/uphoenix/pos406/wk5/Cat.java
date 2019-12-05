public class Cat {
  private static int catsCreated = 0;

  public static void main(String[] args) {
    Cat[] cats = new Cat[3];
    cats[0] = new Cat();
    cats[1] = new Cat();
    cats[2] = new Cat();
    Cat.displayInfo();
  }

  public Cat() {
    catsCreated++;
  }

  public static void displayInfo() {
    System.out.println("There are " + catsCreated + " cat objects created");
  }
}
