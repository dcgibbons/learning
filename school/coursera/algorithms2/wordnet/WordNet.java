import java.util.*;

public class WordNet
{
	private Map<Integer, String> synsets = new HashMap<Integer, String>();
	private Map<String, Set<Integer>> nouns = new HashMap<String, Set<Integer>>();
	private Digraph dag;
	private SAP sap;

	// constructor takes the name of the two input files
	public WordNet(String synsetsFilename, String hypernymsFilename)
	{
		Integer maxSynsetId = -1;
		In synsetsFile = new In(synsetsFilename);
		while (synsetsFile.hasNextLine())
		{
			String line = synsetsFile.readLine();
			String[] splits = line.split(",");

			Integer synsetId = Integer.parseInt(splits[0]);
			String synset = splits[1];
			String[] synsetNouns = synset.split(" ");
			for (int i = 0; i < synsetNouns.length; i++)
			{
				synsets.put(synsetId, synset);

				if (!nouns.containsKey(synsetNouns[i]))
				{
					nouns.put(synsetNouns[i], new HashSet<Integer>());
				}
				nouns.get(synsetNouns[i]).add(synsetId);
			}

			if (synsetId > maxSynsetId)
			{
				maxSynsetId = synsetId;
			}
		}

		dag = new Digraph(maxSynsetId + 1);
		In hypernymsFile = new In(hypernymsFilename);
		while (hypernymsFile.hasNextLine())
		{
			String line = hypernymsFile.readLine();
			String[] splits = line.split(",");
			Integer synsetId = Integer.parseInt(splits[0]);
			for (int i = 1; i < splits.length; i++)
			{
				Integer hypernymId = Integer.parseInt(splits[i]);
				dag.addEdge(synsetId, hypernymId);
			}
		}

		sap = new SAP(dag);
	}

	// returns all WordNet nouns
	public Iterable<String> nouns()
	{
		return nouns.keySet();
	}

	// is the word a WordNet noun?
	public boolean isNoun(String word)
	{
		return nouns.containsKey(word);
	}

	// distance between nounA and nounB (defined below)
	public int distance(String nounA, String nounB)
	{
		if (!nouns.containsKey(nounA))
		{
			throw new IllegalArgumentException(nounA + " is not in WordNet");
		}
		if (!nouns.containsKey(nounB))
		{
			throw new IllegalArgumentException(nounB + "is not in WordNet");
		}

		Set<Integer> a = nouns.get(nounA);
		Set<Integer> b = nouns.get(nounB);
		return sap.length(a, b);
	}

	// a synset (second field of synsets.txt) that is the common ancestor of nounA and nounB
	// in a shortest ancestral path (defined below)
	public String sap(String nounA, String nounB)
	{
		if (!nouns.containsKey(nounA))
		{
			throw new IllegalArgumentException(nounA + " is not in WordNet");
		}
		Set<Integer> a = nouns.get(nounA);

		if (!nouns.containsKey(nounB))
		{
			throw new IllegalArgumentException(nounB + "is not in WordNet");
		}
		Set<Integer> b = nouns.get(nounB);

		int ancestor = sap.ancestor(a, b);
		return (ancestor == -1) ? null : synsets.get(ancestor);
	}

	// for unit testing of this class
	public static void main(String[] args)
	{
		WordNet wordnet = new WordNet(args[0], args[1]);

		System.out.println("sap(Black_Plague,black_marlin="+wordnet.sap("Black_Plague", "black_marlin"));
		int d = wordnet.distance("Black_Plague", "black_marlin");
		System.out.println("distance(Black_Plague,black_marlin)="+d);

		System.out.println("sap(American_water_spaniel,histology="+wordnet.sap("American_water_spaniel", "histology"));
		d = wordnet.distance("American_water_spaniel", "histology");
		System.out.println("distance(American_water_spaniel,histology)="+d);

		System.out.println("sap(Brown_Swiss,barrel_roll="+wordnet.sap("Brown_Swiss", "barrel_roll"));
		d = wordnet.distance("Brown_Swiss", "barrel_roll");
		System.out.println("distance(Brown_Swiss,barrel_roll)="+d);
	}	
}
