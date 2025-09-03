// Examen Febrero 2014 - SCCs
// Data Structures. UMA

package dataStructures.graph;

import static dataStructures.graph.DiGraph.*;
import static dataStructures.graph.DiGraphBFT.bft;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class SCCs {

    // (A) Devuelve el grafo inverso
    public static <V> DiGraph<V> reverseDiGraph(DiGraph<V> g) {
         Collection<DiEdge<V>> edges = new HashSet<>();
        
        for(DiEdge<V> edge : g.edges()){
            edges.add(new DiEdge<>(edge.target(), edge.source()));
        }

        return fromEdges(g.vertices(), edges);
    }

    // (B) Subgrafo con los vértices dados
    public static <V> DiGraph<V> restrictDiGraph(DiGraph<V> g, Set<V> vs) {
         Collection<DiEdge<V>> edges = new HashSet<>();
        
        for(DiEdge<V> edge : g.edges()){
            if(vs.contains(edge.source()) && vs.contains(edge.target()))
                edges.add(new DiEdge<>(edge.source(), edge.target()));
        }

        return fromEdges(vs, edges);
    }

    // (C) SCC de un vértice
    public static <V> Set<V> sccOf(DiGraph<V> g, V src) {
        List<V> vs = bft(g, src);
        DiGraph<V> gr = restrictDiGraph(g, new HashSet<>(vs));
        DiGraph<V> g_prima = reverseDiGraph(gr);
        return new HashSet<>(bft(g_prima, src));
    }

    // (D) Todas las componentes fuertemente conexas
    public static <V> Set<Set<V>> stronglyConnectedComponentsDiGraph(DiGraph<V> g) {
        Set<V> verticesComputados = new HashSet<>();
        Set<V> scc;
        Set<Set<V>> sccs = new HashSet<>();

        for(V v : g.vertices()){
            if(!verticesComputados.contains(v)){
                scc = sccOf(g, v);
                sccs.add(scc);
                verticesComputados.addAll(scc);
            }
        }

        return sccs;
    }
}

