/*
 * File:        Outcast.java
 * Assignment:  WordNet
 * Course:      Algorithms II
 * School:      Princeton University via coursera.com
 * Instructors: Robert Sedgewick and Kevin Wayne
 * Student:     David Chadwick Gibbons <dcgibbons@gmail.com>
 */

import java.util.Map;
import java.util.HashMap;

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
        Map<String, Integer> cache = new HashMap<String, Integer>();

        for (String nounA : nouns)
        {
            for (String nounB : nouns)
            {
                if (nounA.equals(nounB)) continue;
                String a = null;
                String b = null;
                int n = nounA.compareTo(nounB);
                if (n == 0) continue;
                else if (n < 0)
                {
                    a = nounA;
                    b = nounB;
                }
                else
                {
                    a = nounB;
                    b = nounA;
                }

                String key = a + "," + b;
                if (cache.containsKey(key)) continue;

                int d = wordnet.distance(a, b);
                cache.put(key, d);
                if (d != -1 && d > currentOutcastDistance)
                {
                    currentOutcast = nounB;
                    currentOutcastDistance = d;
                }
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
