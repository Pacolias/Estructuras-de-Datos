import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.Graph;

public class eGraph<V> {
    private Graph<V> graph;
    private Dictionary<V, Double> dict;
    private final double THRESHOLD = 0.00001;

    public eGraph(Graph<V> g) {
        graph = (Graph<V>) g.clone();
        dict = new HashDictionary<>();
        for (V v : graph.vertices()) {
            dict.insert(v, 0.0);
        }
    }

    public void distribute(V vertex, double value) {
        // lógica de distribución con recursión
    }

    public void eRank(double value) {
        for (V v : graph.vertices()) {
            distribute(v, value);
        }
    }
}
