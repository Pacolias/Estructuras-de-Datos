/**
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. Informática. UMA.
 */

package dataStructures.graph;
import dataStructures.list.*;
import dataStructures.set.*;
import java.util.Iterator;

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
        if(g.isEmpty())
            return false;

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
        V v = v0, u;

        while(!(v.equals(v0) && res.size() > 1)){
            u = g.successors(v).iterator().next();
            remove(g, v, u);
            res.append(u);
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
            V y = ys.get(0);

            int i = 0;
            while(!y.equals(xs.get(i)))
                i++;

            List<V> aux = new LinkedList<>();

            for(int j = i + 1; j < xs.size(); j++)
                aux.append(xs.get(j));

            for(int k = xs.size() - 1; k >= i; k--)
                xs.remove(k);

            for(V v : ys)
                xs.append(v);

            for(V v : aux)
                xs.append(v);
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        boolean esVerticeComun = false;
        Iterator<V> vertices = cycle.iterator();
        V v = null;
        
        while(!esVerticeComun && vertices.hasNext()){
            v = vertices.next();

            if(g.vertices().isElem(v))
                esVerticeComun = true;
        }

        return v;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        if(!isEulerian(g))
            return null;

        V v = g.vertices().iterator().next(), u;
        List<V> xs = new LinkedList<>(), ys;

        while(!g.isEmpty()){
            ys = extractCycle(g, v);
            connectCycles(xs, ys);
            u = vertexInCommon(g, xs);
            v = u;
        }

        return xs;
    }
}
