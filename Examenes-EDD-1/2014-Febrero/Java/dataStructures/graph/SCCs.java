// Examen Febrero 2014 - SCCs
// Data Structures. UMA

package dataStructures.graph;

import static dataStructures.graph.DiGraphBFT.bft;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class SCCs {

    // (A) Devuelve el grafo inverso
    public static <V> DiGraph<V> reverseDiGraph(DiGraph<V> g) {
        Set<V> vertices = g.vertices();
        Map<V, Set<V>> correspondencias = new HashMap<>();

        for(V v : vertices)
            correspondencias.put(v, g.predecesors(v));

        return new DiGraph<>(vertices, correspondencias);
    }

    // (B) Subgrafo con los vértices dados
    public static <V> DiGraph<V> restrictDiGraph(DiGraph<V> g, Set<V> vs) {
        Map<V, Set<V>> correspondencias = new HashMap<>();
        Set<V> sucesores = null;

        for(V v : vs){
            sucesores = new HashSet<>();

            for(V sucesor : g.successors(v)){
                if(vs.contains(sucesor)){
                    sucesores.add(sucesor);
                }       
            }

            correspondencias.put(v, sucesores); 
        }
        
        return new DiGraph<>(vs, correspondencias);
    }

    // (C) SCC de un vértice
    public static <V> Set<V> sccOf(DiGraph<V> g, V src) {
        List<V> vsList = bft(g, src);
        Set<V> vs = new HashSet<>(vsList);
        DiGraph<V> gr = restrictDiGraph(g, vs);
        DiGraph<V> gPrima = reverseDiGraph(gr);
        return new HashSet<>(bft(gPrima, src));
    }

    // (D) Todas las componentes fuertemente conexas
    public static <V> Set<Set<V>> stronglyConnectedComponentsDiGraph(DiGraph<V> g) {
        Set<Set<V>> res = new HashSet<>();
        Set<V> componente = null;
        Set<V> verticesUsados = new HashSet<>();

        for(V vertice : g.vertices()){
            if(!verticesUsados.contains(vertice)){
                componente = sccOf(g, vertice);
                verticesUsados.addAll(componente);
                res.add(componente);
            }
        }

        return res;
    }
}

