import java.util.*;
import java.lang.reflect.Array;

public class SAP
{
	Digraph G;

	// constructor takes a digraph (not necessarily a Digraph)
	public SAP(Digraph G)
	{
		this.G = new Digraph(G);
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

	// a common ancestor of v and w that participates in a shortest ancestral path; -1 if no such path
	public int ancestor(int v, int w)
	{
		ArrayList<Integer> a = new ArrayList<Integer>(1);
		a.add(v);
		ArrayList<Integer> b = new ArrayList<Integer>(1);
		b.add(w);
		return ancestor(a, b);
	}

	// length of shortest ancestral path between any vertex in v and any vertex in w; -1 if no such path
	public int length(Iterable<Integer> v, Iterable<Integer> w)
	{
		int shortestAncestor = -1;
		int shortestAncestorLength = Integer.MAX_VALUE;

		for (int i = 0; i < this.G.V(); i++)
		{
			BreadthFirstDirectedPaths A = new BreadthFirstDirectedPaths(this.G, v);
			BreadthFirstDirectedPaths B = new BreadthFirstDirectedPaths(this.G, w);

			if (A.hasPathTo(i) && B.hasPathTo(i))
			{
				int length = count(A.pathTo(i)) + count(B.pathTo(i));
				if (length < shortestAncestorLength) 
				{
					shortestAncestor = i;
					shortestAncestorLength = length;
				}
			}
		}

		return (shortestAncestorLength != Integer.MAX_VALUE) ? shortestAncestorLength : -1;
	}

	// a common ancestor that participates in shortest ancestral path; -1 if no such path
	public int ancestor(Iterable<Integer> v, Iterable<Integer> w)
	{
		int shortestAncestor = -1;
		int shortestAncestorLength = Integer.MAX_VALUE;

		for (int i = 0; i < this.G.V(); i++)
		{
			BreadthFirstDirectedPaths A = new BreadthFirstDirectedPaths(this.G, v);
			BreadthFirstDirectedPaths B = new BreadthFirstDirectedPaths(this.G, w);

			if (A.hasPathTo(i) && B.hasPathTo(i))
			{
				int length = count(A.pathTo(i)) + count(B.pathTo(i));
				if (length < shortestAncestorLength) 
				{
					shortestAncestor = i;
					shortestAncestorLength = length;
				}
			}
		}

		return shortestAncestor;
	}

	private static int count(Iterable<Integer> it)
	{
		int n = 0;
		for (Integer x : it) n++;
		return n - 1; // -1 to remove start of path, which is our vertex
	}

	public static Integer[] toArray(Iterable<Integer> it)
	{
		ArrayList<Integer> a = new ArrayList<Integer>();
		for (Integer i : it) a.add(i);
		return a.toArray(new Integer[0]);
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