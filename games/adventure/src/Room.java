
public class Room extends Container {
    private Room[] adjacentRooms = new Room[Direction.values().length];

    public void setAdjacentRoom(Direction direction, Room room) {
        adjacentRooms[direction.ordinal()] = room;
    }

    public Room getAdjacentRoom(Direction direction) {
        return adjacentRooms[direction.ordinal()];
    }

    public String getDescription() {
        StringBuilder buffer = new StringBuilder("You are in an ordinary looking room.\n");
        for (int i = 0; i < adjacentRooms.length; i++) {
            if (adjacentRooms[i] != null) {
                buffer.append("You see another room to the ").append(Direction.values()[i]).append("\n");
            }
        }

        for (GameObject o : getItems() ) {
            buffer.append("You see a ").append(o.getDescription()).append(" here.\n");
        }

        return buffer.toString();
    }
}
