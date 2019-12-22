import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

/**
 * Created by IntelliJ IDEA.
 * User: chadwick
 * Date: Oct 19, 2006
 * Time: 12:51:32 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Container implements GameObject
{
    private List<GameObject> containedItems = new ArrayList<GameObject>();

    public List<GameObject> getItems() {
        return Collections.unmodifiableList( containedItems );
    }

    public void removeItem(GameObject item) {
        containedItems.remove(item);
    }

    public void addItem(GameObject item) {
        containedItems.add(item);
    }

    protected void itemAdded(GameObject item) {
        // NO-OP  by default
    }

    protected void itemRemoved(GameObject item) {
        // NO-OP by default
    }


}
