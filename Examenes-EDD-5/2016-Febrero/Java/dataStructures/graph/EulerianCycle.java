/**
 * Student's name: Francisco Javier Molina Cuenca
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
    private static <V> boolean isEulerian(Graph<V> g){
        boolean isEulerian = true;

        for(V v : g.vertices()){
            if(g.degree(v) % 2 != 0)
                isEulerian = false;
        }
        
        return isEulerian;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u){
        Set<V> vertices = new HashSet<>();

        for(V w : g.vertices())
            vertices.insert(w);

        g.deleteEdge(v, u);
        
        for(V w : vertices){
            if(g.degree(w) == 0)
                g.deleteVertex(w);
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0){
        V v = v0;
        V u;

        List<V> res = new LinkedList<>();
        res.append(v0);

        while(!(v.equals(v0) && res.size() > 1)){
            u = g.successors(v).iterator().next();
            res.append(u);
            remove(g, v, u);
            v = u;
        }

        return res;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys){
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
        boolean encontrado = false;
        V u = null;

        for(V v : cycle){
            if(g.vertices().isElem(v) && !encontrado){
                encontrado = true;
                u = v;
            }
        }

        return u;
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
            connectCycles(xs, ys);
            v = vertexInCommon(g, xs);
        }

        return xs;
    }
}
