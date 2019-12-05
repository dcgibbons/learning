/*************************************************************************
 *  Compilation:  javac DeluxeBFS.java
 *  Execution:    java DeluxeBFS V E
 *  Dependencies: Digraph.java Queue.java Stack.java
 *
 *  Run breadth first search on a digraph.
 *  Runs in O(E + V) time.
 *
 *  % java DeluxeBFS tinyDG.txt 3
 *  3 to 0 (2):  3->2->0
 *  3 to 1 (3):  3->2->0->1
 *  3 to 2 (1):  3->2
 *  3 to 3 (0):  3
 *  3 to 4 (2):  3->5->4
 *  3 to 5 (1):  3->5
 *  3 to 6 (-):  not connected
 *  3 to 7 (-):  not connected
 *  3 to 8 (-):  not connected
 *  3 to 9 (-):  not connected
 *  3 to 10 (-):  not connected
 *  3 to 11 (-):  not connected
 *  3 to 12 (-):  not connected
 *
 *************************************************************************/

/**
 * 
 * I modified the BFS the following way: put into the queue both source vertices 
 * one after the other (or vertices of both source sets). I applied a helper queue. 
 * The i-th item in the helper queue shows the starting source(set)'s ID  of the  
 * i-th vertex in the "main queue." The "marked" and the "distTo" arrays shall be 
 * duplicated. Regarding the "marked" array you can avoid duplication if you change 
 * its type  to "byte" and store the "marked" info  for the two paths in the first 
 * and second bits (google bit twiddling). 
 *
 * This way you run the search concurrently form the two sources. When you visit a 
 * new vertex from any path, you  check, whether the other path has already visited 
 * that vertex or not. If yes (you found a common ancestor), you compare, whether 
 * the new one is better than the cached previous best ancestor. Here you shall 
 * compare the total distances. ("Total distance" means distance from the first 
 * source to the ancestor + distance from the second source to the ancestor).  
 * 
 * Update cached best if new one is better and continue search.
 *
 * You can immediately stop search when the  "single" distance  to the dequeued 
 * vertex (from its own source) is bigger or equal to the total distance of the 
 * best ancestor found already (starting value is infinity of course).
 */

import java.util.ArrayList;
import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class DeluxeBFS {
    private static final int INFINITY = Integer.MAX_VALUE;
    private boolean[][] marked;  // marked[v] = is there an s->v path?
    private int[][] edgeTo;      // edgeTo[v] = last edge on shortest s->v path
    private int[][] distTo;      // distTo[v] = length of shortest s->v path
    private Set<Integer> modifiedIndicies;
    private Map<Integer, Integer> indexSource = new HashMap<Integer, Integer>();
    private int bestAncestor = -1;
    private int bestAncestorLength = INFINITY;

    // multiple sources
    public DeluxeBFS(Digraph G, Iterable<Integer> sourcesA, Iterable<Integer> sourcesB) 
    {
        modifiedIndicies = new HashSet<Integer>();
        marked = new boolean[G.V()][2];
        distTo = new int[G.V()][2];
        edgeTo = new int[G.V()][2];
        for (int v = 0; v < G.V(); v++) distTo[v][0] = distTo[v][1] = INFINITY;
        bfs(G, sourcesA, sourcesB);
    }

    public DeluxeBFS(Digraph G, 
                     DeluxeBFS previousBFS, 
                     Iterable<Integer> sourcesA,
                     Iterable<Integer> sourcesB) 
    {
        modifiedIndicies = new HashSet<Integer>();
        marked = previousBFS.marked;
        distTo = previousBFS.distTo;
        edgeTo = previousBFS.edgeTo;
        for (Integer i : previousBFS.modifiedIndicies)
        {
            marked[i][0] = marked[i][1] = false;
            distTo[i][0] = distTo[i][1] = INFINITY;
            edgeTo[i][0] = edgeTo[i][1] = 0;
        }
        bfs(G, sourcesA, sourcesB);
    }

    // BFS from multiple sources
    private void bfs(Digraph G, Iterable<Integer> sourcesA, Iterable<Integer> sourcesB) {
        Queue<Integer> q = new Queue<Integer>();
        Iterator<Integer> itA = sourcesA.iterator();
        Iterator<Integer> itB = sourcesB.iterator();

        while (itA.hasNext() && itB.hasNext())
        {
            int s = itA.next();
            marked[s][0] = true;
            distTo[s][0] = 0;
            modifiedIndicies.add(s);
            indexSource.put(s, 0);
            q.enqueue(s);

            s = itB.next();
            marked[s][1] = true;
            distTo[s][1] = 0;
            modifiedIndicies.add(s);
            indexSource.put(s, 1);
            q.enqueue(s);

            if (marked[s][0])
            {
                int d = distTo[s][0] + distTo[s][1];
                if (d < bestAncestorLength)
                {
                    bestAncestorLength = d;
                    bestAncestor = s;
                }
            }
        }

        while (itA.hasNext())
        {
            int s = itA.next();
            marked[s][0] = true;
            distTo[s][0] = 0;
            modifiedIndicies.add(s);
            indexSource.put(s, 0);
            q.enqueue(s);
            if (marked[s][1])
            {
                // System.out.println(w + " is a shared ancestor");
                int d = distTo[s][1] + distTo[s][0];
                if (d < bestAncestorLength)
                {
                    bestAncestorLength = d;
                    bestAncestor = s;
                    System.out.println("bestancestor="+bestAncestor+" length="+bestAncestorLength);
                }
            }
        }

        while (itB.hasNext())
        {
            int s = itB.next();
            marked[s][1] = true;
            distTo[s][1] = 0;
            modifiedIndicies.add(s);
            indexSource.put(s, 1);
            q.enqueue(s);
            if (marked[s][0])
            {
                int d = distTo[s][0] + distTo[s][1];
                if (d < bestAncestorLength)
                {
                    bestAncestorLength = d;
                    bestAncestor = s;
                    System.out.println("bestancestor="+bestAncestor+" length="+bestAncestorLength);
                }
            }
        }

        qLoop: while (!q.isEmpty()) 
        {
            int v = q.dequeue();
            int source = indexSource.get(v);
            int otherSource = (source == 1) ? 0 : 1;

            for (int w : G.adj(v)) {
                System.out.println(w + " is adj to "+v+"["+source+"] marked[0]="+marked[w][0]+" [1]="+marked[w][1]);
                if (!marked[w][source]) {
                    edgeTo[w][source] = v;
                    distTo[w][source] = distTo[v][source] + 1;
                    marked[w][source] = true;
                    modifiedIndicies.add(w);
                    q.enqueue(w);
                    indexSource.put(w, source);
                    if (marked[w][otherSource])
                    {
                        int d = distTo[w][source] + distTo[w][otherSource];
                        if (d < bestAncestorLength)
                        {
                            bestAncestorLength = d;
                            bestAncestor = w;
                            System.out.println("bestancestor="+bestAncestor+" length="+bestAncestorLength);
                            //break qLoop;
                        }
                    }
                }
            }
        }
    }

    public int getBestAncestor() { return bestAncestor; }
    public int getBestAncestorLength() { return bestAncestorLength; }

    // length of shortest path from s (or sources) to v
    public int distTo(int v) {
        return distTo[v][0];
    }

    // is there a directed path from s (or sources) to v?
    public boolean hasPathTo(int v) {
        return marked[v][0];
    }

    // shortest path from s (or sources) to v; null if no such path
    public Iterable<Integer> pathTo(int v) {
        if (!hasPathTo(v)) return null;
        Stack<Integer> path = new Stack<Integer>();
        int x;
        for (x = v; distTo[x][0] != 0; x = edgeTo[x][0])
            path.push(x);
        path.push(x);
        return path;
    }

    public static void main(String[] args) {
        In in = new In(args[0]);
        Digraph G = new Digraph(in);
        // StdOut.println(G);

        int s = Integer.parseInt(args[1]);
        ArrayList<Integer> a = new ArrayList<Integer>();
        a.add(s);
        ArrayList<Integer> b = new ArrayList<Integer>();
        DeluxeBFS bfs = new DeluxeBFS(G, a, b);

        for (int v = 0; v < G.V(); v++) {
            if (bfs.hasPathTo(v)) {
                StdOut.printf("%d to %d (%d):  ", s, v, bfs.distTo(v));
                for (int x : bfs.pathTo(v)) {
                    if (x == s) StdOut.print(x);
                    else        StdOut.print("->" + x);
                }
                StdOut.println();
            }

            else {
                StdOut.printf("%d to %d (-):  not connected\n", s, v);
            }

        }
    }
}
