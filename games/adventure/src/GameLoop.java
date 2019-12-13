import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintStream;


public class GameLoop implements Runnable {
    private Player thePlayer;

    private GameLoop() {
        Room centerRoom = new Room();
        Room leftRoom = new Room();
        Room rightRoom = new Room();

        centerRoom.setAdjacentRoom(Direction.WEST, leftRoom);
        centerRoom.setAdjacentRoom(Direction.EAST, rightRoom);
        leftRoom.setAdjacentRoom(Direction.EAST, centerRoom);
        rightRoom.setAdjacentRoom(Direction.WEST, centerRoom);

        thePlayer = new Player(centerRoom);
    }

    public void run() {
        PrintStream out = System.out;
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        try {
            do {
                out.print("Your next move? ");
                out.flush();
                String line = reader.readLine();
                if (line == null || line.startsWith("quit")) {
                    break;
                }

                String[] input = line.split(" ");
                if (input.length == 0 || input[0].equals("look")) {
                    thePlayer.look(out);
                } else if (input[0].equals("move")) {
                    thePlayer.move(Direction.valueOf(input[1]), out);
                } else {
                    out.println("Huh?\n");
                }


            } while (true );
        } catch (IOException ex) {
            ex.printStackTrace(System.err);
        }
    }

    public static void main(String[] args) {
        GameLoop gameLoop = new GameLoop();
        gameLoop.run();
    }
}
