/*
 * File:        Outcast.java
 * Assignment:  WordNet
 * Course:      Algorithms II
 * School:      Princeton University via coursera.com
 * Instructors: Robert Sedgewick and Kevin Wayne
 * Student:     David Chadwick Gibbons <dcgibbons@gmail.com>
 */

public class Outcast
{
    private WordNet wordnet;

    // constructor takes a WordNet object
    public Outcast(WordNet wordnet)
    {
        this.wordnet = wordnet;
    }

    // given an array of WordNet nouns, return an outcast
    public String outcast(String[] nouns)
    {
        String currentOutcast = null;
        int currentOutcastDistance = -1;

        for (String nounA : nouns)
        {
            int dT = 0;

            for (String nounB : nouns)
            {
                if (nounA.equals(nounB)) continue;
                int d = wordnet.distance(nounA, nounB);
                if (d == -1) continue;
                dT += d;
            }

            if (dT > currentOutcastDistance)
            {
                currentOutcast = nounA;
                currentOutcastDistance = dT;
            }
        }

        return currentOutcast;
    }

    // for unit testing of this class (such as the one below)
    public static void main(String[] args)
    {
        WordNet wordnet = new WordNet(args[0], args[1]);
        Outcast outcast = new Outcast(wordnet);
        for (int t = 2; t < args.length; t++) 
        {
            String[] nouns = In.readStrings(args[t]);
            StdOut.println(args[t] + ": " + outcast.outcast(nouns));
        }
    }
}
