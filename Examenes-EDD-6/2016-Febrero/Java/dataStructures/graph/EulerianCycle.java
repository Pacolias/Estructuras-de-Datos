/**
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. Informática. UMA.
 */

package dataStructures.graph;
import dataStructures.list.*;
import dataStructures.set.*;

public class EulerianCycle<V> {
    private List<V> eCycle;

    @SuppressWarnings("unchecked")
    public EulerianCycle(Graph<V> g){
        Graph<V> graph = (Graph<V>) g.clone();
        eCycle = eulerianCycle(graph);
    }

    public boolean isEulerian(){
        return eCycle != null;
    }

    public List<V> eulerianCycle(){
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
            
            if(!v.equals(u)){
                res.append(u);
                remove(g, v, u);
                v = u;
            }
        }

        return res;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys){
        if(xs.isEmpty()){
            for(V v : ys)
                xs.append(v);

        } else{
            List<V> aux = new LinkedList<>();
            V y = ys.get(0);

            int i = 0;
            while(!y.equals(xs.get(i)))
                i++;

            for(int j = i + 1; j < xs.size(); j++)
                aux.append(xs.get(j));

            int xsLength = xs.size();
            for(int l = xsLength - 1; i <= l; l--)
                xs.remove(l);

            for(int k = 0; k < ys.size(); k++)
                xs.insert(i + k, ys.get(k));

            for(V v : aux)
                xs.append(v);
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle){
        boolean isCommon = false;
        V common = null;

        for(V v : cycle){
            if(!isCommon && g.vertices().isElem(v)){
                isCommon = true;
                common = v;
            }
        }

        return common;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g){
        if(!isEulerian(g))
            return null;

        List<V> xs = new LinkedList<>();
        List<V> ys;
        
        V v = g.vertices().iterator().next();
        V u;

        while(!g.isEmpty()){
            ys = extractCycle(g, v);
            connectCycles(xs, ys);
            u = vertexInCommon(g, xs);
            v = u;
        }

        return xs;
    }
}
