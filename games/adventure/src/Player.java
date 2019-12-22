import java.io.PrintStream;

public class Player extends Creature {
    private Room currentRoom;

    public Player(Room startRoom) {
        currentRoom = startRoom;
        currentRoom.addItem(this);
    }

    public String getDescription() {
        return "yourself";
    }

    public void look(PrintStream out) {
        out.println("You look around the room.");
        out.println(currentRoom.getDescription());
    }

    boolean move(Direction direction, PrintStream out) {
        Room adjacentRoom = currentRoom.getAdjacentRoom(direction);
        boolean moved;
        if (adjacentRoom != null) {
            currentRoom.removeItem(this);
            currentRoom = adjacentRoom;
            currentRoom.addItem(this);
            moved = true;
            out.println("You moved " + direction.toString());
            out.println(currentRoom.getDescription());
        } else {
            moved = false;
            out.println("You cannot move that direction.");
        }

        return moved;
    }
}
