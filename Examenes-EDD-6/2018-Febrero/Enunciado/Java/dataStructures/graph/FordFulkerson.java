/**
 * Student's name: Ángel Manuel Soria Gil
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
        int res = path.get(0).getWeight();
        for (WDiEdge<V, Integer> edge : path){
            if (res <= edge.getWeight()){
                res = edge.getWeight();
            }
        }
        return res;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdge(V x, V y, Integer p, List<WDiEdge<V,Integer>> edges) {
        boolean thereisone = false;
        for (int i = 0; i< edges.size(); i++){
            WDiEdge<V, Integer> edge = edges.get(i);
            if (edge.getSrc().equals(x) && edge.getDst().equals(y)){
                thereisone = true;
                Integer newW = edge.getWeight()+p;
                if (newW == 0){
                    edges.remove(i);
                } else {
                    edge = new WDiEdge<>(edge.getSrc(), newW, edge.getDst());
                    edges.insert(i, edge);
                }
            }
        }
        if (!thereisone){
            edges.append(new WDiEdge<>(x, p, y));
        }
        return edges;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdges(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> edges) {
        for (WDiEdge<V,Integer> arc : path){
            V x = arc.getSrc();
            V y = arc.getDst();
            edges = updateEdge(x, y, p, edges);
        }
        return edges;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlow(V x, V y, Integer p, List<WDiEdge<V,Integer>> sol) {
        boolean thereisone = false;
        for (int i = 0; i< sol.size(); i++){
            WDiEdge<V, Integer> edge = sol.get(i);
            if (edge.getSrc().equals(x) && edge.getDst().equals(y)){
                thereisone = true;
                edge = new WDiEdge<>(x, (edge.getWeight())+p, y);
                sol.insert(i, edge);
            } else if (edge.getSrc().equals(y) && edge.getDst().equals(x) && edge.getWeight().equals(p)){
                thereisone = true;
                sol.remove(i);
            } else if (edge.getSrc().equals(y) && edge.getDst().equals(x) && edge.getWeight()<p){
                thereisone = true;
                edge = new WDiEdge<>(x, p - edge.getWeight(), y);
                sol.insert(i, edge);
            } else if (edge.getSrc().equals(y) && edge.getDst().equals(x) && edge.getWeight()>p){
                thereisone = true;
                edge = new WDiEdge<>(x, edge.getWeight()-p, y);
                sol.insert(i, edge);
            }
        }
        if (!thereisone){
            sol.append(new WDiEdge<>(x,p,y));
        }
        return sol;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlows(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> sol) {
        for (WDiEdge<V,Integer> arc : path){
            V x = arc.getSrc();
            V y = arc.getDst();
            sol = addFlow(x, y, p, sol);
        }
        return sol;
    }

    public FordFulkerson(WeightedDiGraph<V,Integer> g, V src, V dst) {
        this.g = g;
        this.sol = new LinkedList<>();
        this.src = src;
        this.dst = dst;
        WeightedBreadthFirstTraversal<V,Integer> path = new WeightedBreadthFirstTraversal<>(g, src);
        List<WDiEdge<V,Integer>> pathto = path.pathTo(dst);
            while (!pathto.isEmpty()){
                Integer mf = maxFlowPath(pathto);
                List<WDiEdge<V,Integer>> edges = g.wDiEdges();
                edges = updateEdges(pathto, -mf, edges);
                edges = updateEdges(inverso(pathto), mf, edges);
                g = new WeightedDictionaryDiGraph<>(g.vertices(), edges);
                this.sol = addFlows(pathto, mf, sol);
            }
    }

    private List<WDiEdge<V,Integer>> inverso(List<WDiEdge<V, Integer>> pathto) {
        List<WDiEdge<V,Integer>> res = new LinkedList<>();
        for (WDiEdge<V,Integer> edge : pathto){
            edge = new WDiEdge<>(edge.getDst(), edge.getWeight(), edge.getSrc());
            res.prepend(edge);
        }
        return res;
    }


    public int maxFlow() {
        int res = 0;
        for (WDiEdge<V,Integer> edge : sol){
            if (edge.getSrc() == this.src){
                res = res + edge.getWeight();
            }
        }
        return res;
    }

    public int maxFlowMinCut(Set<V> set) {
        int res = 0;
        Set<V> total = g.vertices();
        Set<V> otherSet = new HashSet<>();
        for (V vertex : total){
            if (!set.isElem(vertex)){
                otherSet.insert(vertex);
            }
        }
        if (set.isElem(this.src)){
            for (V vertex : set){
                for (Tuple2<V,Integer> edge : g.successors(vertex)){
                    if (otherSet.isElem(edge._1())){
                        res = res + edge._2();
                    }
                }
            }
            for (V vertex : otherSet){
                for (Tuple2<V, Integer> edge : g.successors(vertex)){
                    if (set.isElem(edge._1())){
                        res = res - edge._2();
                    }
                }
            }
        } else {
            for (V vertex : otherSet){
                for (Tuple2<V, Integer> edge : g.successors(vertex)){
                    if (set.isElem(edge._1())){
                        res = res + edge._2();
                    }
                }
            }
            for (V vertex : set){
                for (Tuple2<V,Integer> edge : g.successors(vertex)){
                    if (otherSet.isElem(edge._1())){
                        res = res - edge._2();
                    }
                }
            }
        }
        return res;
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
