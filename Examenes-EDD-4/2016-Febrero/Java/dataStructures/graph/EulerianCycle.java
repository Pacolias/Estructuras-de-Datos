/**
 * Student's name: Ángel Manuel Soria Gil
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. Informática. UMA.
 */

package dataStructures.graph;
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
        boolean isEulerian = true;

        for(V v : g.vertices()){
            if(g.degree(v) % 2 != 0)
                isEulerian = false;
        }

        return false;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteVertex(v);
        g.deleteVertex(u);
        g.deleteEdge(v, u);
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        V v = v0;
        V u;
        List<V> res = new LinkedList<>();
        res.append(v);

        while(!(v.equals(v0) && res.size() > 1)){
            u = g.vertices().iterator().next();
            res.append(u);
            remove(g, v, u);
            v = u;
        }

        return res;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
        if(xs.isEmpty()){
            xs = new LinkedList<>();

            for(V v : ys)
                xs.append(v);

        } else{
            V y = ys.get(0);
            List<V> res = new LinkedList<>();

            int i = 0;
            while(!y.equals(xs.get(i))){
                res.append(xs.get(i));
                i++;
            }

            i++;

            for(V v : ys)
                res.append(v);

            while(i < xs.size()){
                res.append(xs.get(i));
                i++;
            }

            xs = res;
        }

    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
        boolean contenido = false;
        V res = null;

        for(V v : cycle){
            if(!contenido && g.vertices().isElem(v)){
                contenido = true;
                res = v;
            }
        }

        return res;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        if(!isEulerian(g))
            return null;

        List<V> xs = new LinkedList<>();
        List<V> ys;
        V v = g.vertices().iterator().next();

        while(!g.isEmpty()){
            ys = extractCycle(g, v);
            v = vertexInCommon(g, ys);
            connectCycles(xs, ys);
        }

        return xs;
    }
}
