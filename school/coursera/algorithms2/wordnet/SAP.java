/*
 * File:        SAP.java
 * Assignment:  WordNet
 * Course:      Algorithms II
 * School:      Princeton University via coursera.com
 * Instructors: Robert Sedgewick and Kevin Wayne
 * Student:     David Chadwick Gibbons <dcgibbons@gmail.com>
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class SAP
{
    private Digraph G;
    private DeluxeBFS lastBFS = null;
    private Map<String, Map<Integer, Integer>> cache;

    // constructor takes a digraph (not necessarily a Digraph)
    public SAP(Digraph G)
    {
        this.G = new Digraph(G);
        cache = new HashMap<String, Map<Integer, Integer>>();
    }

    // length of shortest ancestral path between v and w; -1 if no such path
    public int length(int v, int w)
    {
        ArrayList<Integer> a = new ArrayList<Integer>(1);
        a.add(v);
        ArrayList<Integer> b = new ArrayList<Integer>(1);
        b.add(w);
        return length(a, b);
    }

    // a common ancestor of v and w that participates in a shortest ancestral 
    // path; -1 if no such path
    public int ancestor(int v, int w)
    {
        ArrayList<Integer> a = new ArrayList<Integer>(1);
        a.add(v);
        ArrayList<Integer> b = new ArrayList<Integer>(1);
        b.add(w);
        return ancestor(a, b);
    }

    // length of shortest ancestral path between any vertex in v and any vertex
    // in w; -1 if no such path
    public int length(Iterable<Integer> v, Iterable<Integer> w)
    {
        int shortestAncestor = -1;
        int shortestAncestorLength = Integer.MAX_VALUE;

        DeluxeBFS A;
        if (lastBFS != null)
            A = new DeluxeBFS(this.G, lastBFS, v, w);
        else
            A = new DeluxeBFS(this.G, v, w);
        lastBFS = A;
        shortestAncestor = A.getBestAncestor();
        shortestAncestorLength = A.getBestAncestorLength();

        if (shortestAncestorLength != Integer.MAX_VALUE) 
            return shortestAncestorLength;
        else
            return -1;
    }

    // a common ancestor that participates in shortest ancestral path;
    // -1 if no such path
    public int ancestor(Iterable<Integer> v, Iterable<Integer> w)
    {
        int shortestAncestor = -1;
        int shortestAncestorLength = Integer.MAX_VALUE;

        DeluxeBFS A;
        if (lastBFS != null)
            A = new DeluxeBFS(this.G, lastBFS, v, w);
        else
            A = new DeluxeBFS(this.G, v, w);
        lastBFS = A;
        shortestAncestor = A.getBestAncestor();
        shortestAncestorLength = A.getBestAncestorLength();

        return shortestAncestor;
    }

    private static String stringify(Iterable<Integer> it)
    {
        StringBuilder buffer = new StringBuilder();
        for (int x : it) buffer.append(x).append(",");
        return buffer.toString();
    }

    private static int count(Iterable<Integer> it)
    {
        int n = 0;
        for (int x : it) n++;
        return n - 1; // -1 to remove start of path, which is our vertex
    }

    // for unit testing of this class (such as the one below)
    public static void main(String[] args) 
    {
        In in = new In(args[0]);
        Digraph G = new Digraph(in);
        System.out.println("Digraph loaded:\n"+G);
        SAP sap = new SAP(G);
        while (!StdIn.isEmpty()) 
        {
            int v = StdIn.readInt();
            int w = StdIn.readInt();
            int length   = sap.length(v, w);
            int ancestor = sap.ancestor(v, w);
            StdOut.printf("length = %d, ancestor = %d\n", length, ancestor);
        }
    }
}
