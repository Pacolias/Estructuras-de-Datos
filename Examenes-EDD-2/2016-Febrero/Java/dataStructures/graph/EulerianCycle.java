/**
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. Informática. UMA.
 */

package dataStructures.graph;
import dataStructures.list.*;
import dataStructures.set.*;

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
        boolean esEuleriano = true;
        
        for(V v : g.vertices()){
            if(g.degree(v) % 2 != 0)
                esEuleriano = false;
        }

        return esEuleriano;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteEdge(v, u);
        Set<V> vertices = new HashSet<>();

        for(V w : g.vertices())
            vertices.insert(w);

        for(V w : vertices){
            if(g.degree(w) == 0)
                g.deleteVertex(w);
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        List<V> path = new LinkedList<>();
        path.append(v0);
        V v = v0;
        V u;

        while(!v.equals(v0) || path.size() < 2){
            u = getSuccessor(g.successors(v));
            path.append(u);
            remove(g, v, u);
            v = u;
        }

        return path;
    }

    private static <V> V getSuccessor(Set<V> successors){
        V v = null;
        boolean esPrimero = false;

        for(V u : successors){
            if(!esPrimero){
                v = u;
                esPrimero = true;
            }
        }

        return v;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
        if(xs.isEmpty()){
            for(int i = 0; i < ys.size(); i++)
                xs.append(ys.get(i));
        
        } else{
            List<V> res = new LinkedList<>();

            int i = 0;
            V elem = ys.get(0);

            while(!xs.get(i).equals(elem)){
                res.append(xs.get(i));
                i++;
            }

            for(V v : ys)
                res.append(v);
            
            for(i = i + 1; i < xs.size(); i++)
                res.append(xs.get(i));
            
            for(int j = 0; j < xs.size(); j++)
                xs.remove(j);
            
            for(int j = 0; j < res.size(); j++)
                xs.append(res.get(j));
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        V res = null;
        boolean esComun = false;
        int i = 0;

        while(i < cycle.size() && !esComun){
            if(g.vertices().isElem(cycle.get(i))){
                esComun = true;
                res = cycle.get(i);
            }
            i++;
        }

        return res;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        if(!isEulerian(g))
            return null;
        
        List<V> path = new LinkedList<>();
        List<V> ciclo;
        V v = verticeArbitrario(g);

        while(!g.isEmpty()) { 
            ciclo = extractCycle(g, v);
            connectCycles(path, ciclo);
            v = vertexInCommon(g, ciclo);
        }

        return path;
    }

    private static <V> V verticeArbitrario(Graph<V> g){
        boolean esVertice = false;
        V v = null;

        for(V u : g.vertices()){
            if(!esVertice){
                v = u;
                esVertice = true;
            }
        }

        return v;
    }
}
