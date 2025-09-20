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
        boolean encontrado = false;
        
        for(WDiEdge<V,Integer> edge : edges){
            if(x.equals(edge.getSrc()) && y.equals(edge.getDst())){
                encontrado = true;

                if(edge.getWeight() + p != 0)
                    res.append(new WDiEdge<>(edge.getSrc(), edge.getWeight() + p, edge.getDst()));
            
            } else{
                res.append(edge);
            } 
        }

        if(!encontrado)
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
        boolean encontrado = false;
        
        for(WDiEdge<V,Integer> edge : sol){
            if(x.equals(edge.getSrc()) && y.equals(edge.getDst())){
                encontrado = true;
                res.append(new WDiEdge<>(x, edge.getWeight() + p, y));
            
            } else if(y.equals(edge.getSrc()) && x.equals(edge.getDst()) && p == edge.getWeight()){
                encontrado = true;

            } else if(y.equals(edge.getSrc()) && x.equals(edge.getDst()) && p > edge.getWeight()){
                encontrado = true;
                res.append(new WDiEdge<>(x, p - edge.getWeight(), y));

            } else if(y.equals(edge.getSrc()) && x.equals(edge.getDst()) && p < edge.getWeight()){
                encontrado = true;
                res.append(new WDiEdge<>(y, edge.getWeight() - p, x));

            } else{
                res.append(edge);
            } 
        }

        if(!encontrado)
            res.append(new WDiEdge<>(x, p, y));

        return res;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlows(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> sol) {
        for(WDiEdge<V,Integer> edge : path)
            sol = addFlow(edge.getSrc(), edge.getDst(), p, sol);

        return sol;
    }

    public FordFulkerson(WeightedDiGraph<V,Integer> g, V src, V dst) {
        this.g = g;
        this.src = src;
        this.dst = dst;
        this.sol = new LinkedList<>();

        WeightedDiGraph<V,Integer> wdg = g;
        WeightedBreadthFirstTraversal<V,Integer> wbft = new WeightedBreadthFirstTraversal<>(g, src);
        List<WDiEdge<V,Integer>> path = wbft.pathTo(dst);
        
        int mf;
        List<WDiEdge<V,Integer>> edges;

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

    private static <V> List<WDiEdge<V,Integer>> reverse(List<WDiEdge<V,Integer>> path){
        List<WDiEdge<V,Integer>> res = new LinkedList<>();

        for(WDiEdge<V,Integer> edge : path)
            res.append(new WDiEdge<>(edge.getDst(), edge.getWeight(), edge.getSrc()));

        return res;
    }

    public int maxFlow() {
        int max = 0;

        for(WDiEdge<V,Integer> edge : sol){
            if(src.equals(edge.getSrc()))
                max += edge.getWeight();
        }

        return max;
    }

    public int maxFlowMinCut(Set<V> set) {
        int max = 0;

        for(WDiEdge<V,Integer> edge : g.wDiEdges()){
            if(set.isElem(edge.getSrc()) && !set.isElem(edge.getDst()))
                max += edge.getWeight();
            else if(!set.isElem(edge.getSrc()) && set.isElem(edge.getDst()))
                max -= edge.getWeight();
        }    
        
        return max;
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
        // TO DO
        return false;
    }
    public static <V,W> Tuple2<List<V>,List<V>> sourcesAndSinks(WeightedDiGraph<V,W> g) {
        // TO DO
        return null;
    }

    public static <V> void unifySourceAndSink(WeightedDiGraph<V,Integer> g, V newSrc, V newDst) {
        // TO DO
    }
}
