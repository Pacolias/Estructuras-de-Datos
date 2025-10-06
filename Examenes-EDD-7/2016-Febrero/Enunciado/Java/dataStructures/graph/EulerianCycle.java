/**
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. Informática. UMA.
 */

package dataStructures.graph;
import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.set.*;
import dataStructures.list.*;

public class EulerianCycle<V> {
    private List<V> eCycle;

    @SuppressWarnings("unchecked")
    public EulerianCycle(Graph<V> g) {
        Graph<V> graph = (Graph<V>) g.clone();
        eCycle = eulerianCycle(graph);
    }

    public boolean isEulerian() {
        return eCycle != null;
    }

    public List<V> eulerianCycle() {
        return eCycle;
    }

    // J.1
    private static <V> boolean isEulerian(Graph<V> g) {

    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {

    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {

    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {

    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {

    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {

    }
}
