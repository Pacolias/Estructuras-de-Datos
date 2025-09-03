/**
 * Student's name: Paco Molina Cuenca
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

        for(V w : g.vertices()){
            if(g.degree(w) == 0)
                g.deleteVertex(w);
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        List<V> ciclo = new LinkedList<>();
        ciclo.append(v0);
        V v = v0;
        V u;

        while(!(v.equals(v0) && ciclo.size() > 1)){
            u = sucesor(g, v);
            remove(g, v, u);
            ciclo.append(u);
            v = u;
        }

        return ciclo;
    }

    private static <V> V sucesor(Graph<V> g, V u){
        boolean esPrimerSucesor = false;
        V primerSucesor = null;

        for(V sucesor : g.successors(u)){
            if(!esPrimerSucesor){
                primerSucesor = sucesor;
                esPrimerSucesor = true;
            }
        }

        return primerSucesor;
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
        boolean esVerticeComun = false;
        V verticeComun = null;

        for(V v : cycle){
            if(!esVerticeComun && g.vertices().isElem(v)){
                esVerticeComun = true;
                verticeComun = v;
            }
        }

        return verticeComun;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
        if(!isEulerian(g)){
          return null;
        
        } else{
            List<V> path = new LinkedList<>();
            V v = verticeArbitrario(g);
            List<V> ciclo;

            while(!g.isEmpty()){
                ciclo = extractCycle(g, v);
                v = vertexInCommon(g, ciclo);
                connectCycles(path, ciclo);
            }

            return path;
        }
    }

    private static <V> V verticeArbitrario(Graph<V> g){
        V vertice = null;
        boolean esPrimerVertice = false;

        for(V v : g.vertices()){
            if(!esPrimerVertice){
                esPrimerVertice = true;
                vertice = v;
            }
        }

        return vertice;
    }
}
