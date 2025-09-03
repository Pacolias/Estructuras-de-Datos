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
        List<V> res = new LinkedList<>();
        res.append(v0);
        V v = v0;
        V u;

        while(!(v.equals(v0) && res.size() > 1)){
            u = g.successors(v).iterator().next();
            res.append(u);
            remove(g, v, u);
            v = u;
        }

        return res;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
        if(xs.isEmpty()){
            for(V v : ys)
                xs.append(v);
        
        } else{
            List<V> aux = new LinkedList<>();
            V y = ys.get(0);
            int i = 0;

            while(!xs.get(i).equals(y)){
                aux.append(xs.get(i));
                i++;
            }

            i++;

            for(int j = 0; j < ys.size(); j++)
                aux.append(ys.get(j));

            while(i < xs.size()){
                aux.append(xs.get(i));
                i++;
            }

            xs = new LinkedList<>();

            for(V v : aux)
                xs.append(v);
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        boolean esPrimero = false;
        V res = null;

        for(V v : cycle){
            if(!esPrimero && g.vertices().isElem(v)){
                esPrimero = true;
                res = v;
            }
        }

        return res;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        V v = g.vertices().iterator().next();
        List<V> xs = new LinkedList<>();
        List<V> ys;

        if(!isEulerian(g))
            return null;

        while(!g.isEmpty()){
            ys = extractCycle(g, v);
            connectCycles(xs, ys);
            v = vertexInCommon(g, ys);
        }

        return xs;
    }
}
