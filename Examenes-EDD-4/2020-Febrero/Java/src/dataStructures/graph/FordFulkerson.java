/**
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 */

package dataStructures.graph;

import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

public class FordFulkerson<V> {
    private WeightedDiGraph<V,Integer> g; // Initial graph 
    private List<WDiEdge<V,Integer>> sol; // List of edges representing maximal flow graph
    private V src; 			  // Source
    private V dst; 		  	  // Sink
	
    /**
     * Constructors and methods
     */

    public static <V> int maxFlowPath(List<WDiEdge<V,Integer>> path) {
        int maxFlow = path.get(0).getWeight();

        for(WDiEdge<V,Integer> edge : path){
            if(edge.getWeight() < maxFlow)
                maxFlow = edge.getWeight();
        }

        return maxFlow;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdge(V x, V y, Integer p, List<WDiEdge<V,Integer>> edges) {
        List<WDiEdge<V,Integer>> res = new LinkedList<>();
        boolean esAnyadido = false;

        for(WDiEdge<V,Integer> edge : edges){
            if(x.equals(edge.getSrc()) && y.equals(edge.getDst())){
                esAnyadido = true;

                if(p + edge.getWeight() != 0)
                    res.append(new WDiEdge<>(x, p + edge.getWeight(), y));
                
            } else{
                res.append(edge);
            }
        }

        if(!esAnyadido)
            res.append(new WDiEdge<>(x, p, y));

        return res;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdges(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> edges) {
        for(WDiEdge<V,Integer> edge : path)
            edges = updateEdge(edge.getSrc(), edge.getDst(), p, edges);

        return edges;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlow(V x, V y, Integer p, List<WDiEdge<V,Integer>> sol) {
        List<WDiEdge<V,Integer>> res = new LinkedList<>();
        boolean esAnyadido = false;

        for(WDiEdge<V,Integer> edge : sol){
            if(x.equals(edge.getSrc()) && y.equals(edge.getDst())){
                res.append(new WDiEdge<>(x, edge.getWeight() + p, y));
                esAnyadido = true;

            } else if(y.equals(edge.getSrc()) && x.equals(edge.getDst()) && edge.getWeight() == p){
                esAnyadido = true;
            
            } else if(y.equals(edge.getSrc()) && x.equals(edge.getDst()) && edge.getWeight() < p){
                res.append(new WDiEdge<>(x, p - edge.getWeight(), y));
                esAnyadido = true;

            } else if(y.equals(edge.getSrc()) && x.equals(edge.getDst()) && edge.getWeight() > p){
                res.append(new WDiEdge<>(y, edge.getWeight() - p, x));
                esAnyadido = true;
            
            } else{
                res.append(edge);
            }
        }

        if(!esAnyadido)
            res.append(new WDiEdge<V,Integer>(x, p, y));

        return res;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlows(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> sol) {
        for(WDiEdge<V,Integer> edge : path)
            sol = addFlow(edge.getSrc(), edge.getDst(), p, sol);

        return sol;
    }

    public FordFulkerson(WeightedDiGraph<V,Integer> g, V src, V dst) {
        this.g = g;
        sol = new LinkedList<>();
        this.src = src;
        this.dst = dst;

        WeightedDiGraph<V,Integer> wdg = g;
        WeightedBreadthFirstTraversal<V,Integer> wbft = new WeightedBreadthFirstTraversal<>(wdg, src);
        List<WDiEdge<V,Integer>> path = wbft.pathTo(dst);
        List<WDiEdge<V,Integer>> edges;

        int mf;

        while(path != null && !path.isEmpty()){
            mf = maxFlowPath(path);
            edges = wdg.wDiEdges();
            edges = updateEdges(path, -mf, edges);
            edges = updateEdges(reverse(path), mf, edges);
            wdg = new WeightedDictionaryDiGraph<>(wdg.vertices(), edges);
            sol = addFlows(path, mf, sol);

            wbft = new WeightedBreadthFirstTraversal<>(wdg, src);
            path = wbft.pathTo(dst);
        }
    }

    private List<WDiEdge<V,Integer>> reverse(List<WDiEdge<V,Integer>> path){
        List<WDiEdge<V,Integer>> res = new LinkedList<>();

        for(WDiEdge<V,Integer> edge : path)
            res.append(new WDiEdge<>(edge.getDst(), edge.getWeight(), edge.getSrc()));

        return res;
    }

    public int maxFlow() {
        int maxFlow = 0;

        for(WDiEdge<V,Integer> edge : sol){
            if(edge.getSrc().equals(src))
                maxFlow += edge.getWeight();
        }

        return maxFlow;
    }

    public int maxFlowMinCut(Set<V> set) {
        int maxFlow = 0;

        for(WDiEdge<V,Integer> edge : g.wDiEdges()){
            if(set.isElem(edge.getSrc()) && !set.isElem(edge.getDst()))
                maxFlow += edge.getWeight();
            else if(!set.isElem(edge.getSrc()) && set.isElem(edge.getDst()))
                maxFlow -= edge.getWeight();
        }

        return maxFlow;
    }

    /**
     * Provided auxiliary methods
     */
    public List<WDiEdge<V, Integer>> getSolution() {
        return sol;
    }
	
    /**********************************************************************************
     * A partir de aquí SOLO para estudiantes a tiempo parcial sin evaluación continua.
     * ONLY for part time students.
     * ********************************************************************************/

    public static <V> boolean localEquilibrium(WeightedDiGraph<V,Integer> g, V src, V dst) {
        boolean isLocalEquilibrium = true;

        for(V v : g.vertices()){
            if(g.inDegree(v) != g.outDegree(v))
                isLocalEquilibrium = false;
        }

        return isLocalEquilibrium;
    }
    public static <V,W> Tuple2<List<V>,List<V>> sourcesAndSinks(WeightedDiGraph<V,W> g) {
        Tuple2<List<V>, List<V>> sourcesAndSinks = new Tuple2<>(new LinkedList<>(), new LinkedList<>());

        for(V v : g.vertices()){
            if(g.inDegree(v) == 0)
                sourcesAndSinks._1().append(v);
            else if(g.outDegree(v) == 0)
                sourcesAndSinks._2().append(v);
        }

        return sourcesAndSinks;
    }

    public static <V> void unifySourceAndSink(WeightedDiGraph<V,Integer> g, V newSrc, V newDst) {
        Tuple2<List<V>, List<V>> sourcesAndSinks = sourcesAndSinks(g);

        if(sourcesAndSinks._1().size() > 1){
            g.addVertex(newSrc);

            for(V v : sourcesAndSinks._1())
                g.addDiEdge(newSrc, nuevoPeso(g, v, true), v);
        }

        if(sourcesAndSinks._2().size() > 1){
            g.addVertex(newDst);

            for(V v : sourcesAndSinks._2())
                g.addDiEdge(v, nuevoPeso(g, v, false), newDst);
        }
    }

    private static <V> int nuevoPeso(WeightedDiGraph<V,Integer> g, V v, boolean isSource){
        int peso = 0;

        if(isSource){
            for(WDiEdge<V,Integer> edge : g.wDiEdges()){
                if(edge.getSrc().equals(v))
                    peso += edge.getWeight();
            }
        
        } else{//isSink
            for(WDiEdge<V,Integer> edge : g.wDiEdges()){
                if(edge.getDst().equals(v))
                    peso += edge.getWeight();
            }
        }

        return peso;
    }
}
