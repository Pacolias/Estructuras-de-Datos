package dataStructures.graph;

import java.util.*;

public class DiGraph<V> {
    private final Set<V> vertices;
    private final Map<V, Set<V>> adjList;

    public DiGraph(Collection<V> vertices, Map<V, Set<V>> adjList) {
        this.vertices = new HashSet<>(vertices);
        this.adjList = new HashMap<>();
        for (V v : vertices) {
            this.adjList.put(v, new HashSet<>(adjList.getOrDefault(v, new HashSet<>())));
        }
    }

    // Constructor desde lista de aristas
    public static <V> DiGraph<V> fromEdges(Collection<V> vertices, Collection<DiEdge<V>> edges) {
        Map<V, Set<V>> adj = new HashMap<>();
        for (V v : vertices) {
            adj.put(v, new HashSet<>());
        }
        for (DiEdge<V> edge : edges) {
            adj.get(edge.source()).add(edge.target());
        }
        return new DiGraph<>(vertices, adj);
    }

    public Set<V> vertices() {
        return new HashSet<>(vertices);
    }

    public Set<V> successors(V v) {
        return new HashSet<>(adjList.getOrDefault(v, Set.of()));
    }

    public Set<V> predecesors(V v) {
        Set<V> result = new HashSet<>();
        for (V u : vertices) {
            if (adjList.getOrDefault(u, Set.of()).contains(v)) {
                result.add(u);
            }
        }
        return result;
    }

    public int inDegree(V v) {
        return predecesors(v).size();
    }

    public int outDegree(V v) {
        return successors(v).size();
    }

    public Set<DiEdge<V>> edges() {
        Set<DiEdge<V>> result = new HashSet<>();
        for (V v : vertices) {
            for (V w : successors(v)) {
                result.add(new DiEdge<>(v, w));
            }
        }
        return result;
    }

    @Override
    public String toString() {
        return "DiGraph" + adjList.toString();
    }
}

