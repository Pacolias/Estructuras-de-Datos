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
        int pesoMinimo = path.get(0).getWeight();

        for(WDiEdge<V, Integer> arco : path){
            if(arco.getWeight() < pesoMinimo)
                pesoMinimo = arco.getWeight();
        }

        return pesoMinimo;
    }

    public static <V> List<WDiEdge<V,Integer>> updateEdge(V x, V y, Integer p, List<WDiEdge<V,Integer>> edges) {
        List<WDiEdge<V,Integer> resultado = new LinkedList<>();
        int nuevoPeso;

        for(WDiEdge<V,Integer> arco : edges){
            if(arco.getSrc().equals(x) && arco.getDst().equals(y)){
                nuevoPeso = arco.getWeight() + p;

                if(nuevoPeso != 0)
                    resultado.append(new WDiEdge<>(x, nuevoPeso, y));
            
            } else{
                resultado.append(arco);
            }
        }

        return resultado;

    }

    public static <V> List<WDiEdge<V,Integer>> updateEdges(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> edges) {
        List<WDiEdge<V,Integer>> resultado = edges;

        for(WDiEdge<V, Integer> arco : path)
            resultado = updateEdge(arco.getSrc(), arco.getDst(), p, resultado);
        
        return resultado;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlow(V x, V y, Integer p, List<WDiEdge<V,Integer>> sol) {
        List<WDiEdge<V,Integer> resultado = new LinkedList<>();

        for(WDiEdge<V,Integer> arco : sol){
            if(arco.getSrc().equals(x) && arco.getDst().equals(y))
                resultado.append(new WDiEdge<>(x, arco.getWeight() + p, y));
            else if(arco.getSrc().equals(y) && arco.getDst().equals(x) && arco.getWeight().equals(p))
                // Nada
            else if(arco.getSrc().equals(y) && arco.getDst().equals(x) && arco.getWeight() < p)
                resultado.append(new WDiEdge<>(x, p - arco.getWeight(), y));
            else if(arco.getSrc().equals(y) && arco.getDst().equals(x) && arco.getWeight() > p)
                resultado.append(new WDiEdge<>(y, arco.getWeight() - p, x));
            else 
                resultado.append(new WDiEdge<>(x, p, y));
        }

        return resultado;
    }

    public static <V> List<WDiEdge<V,Integer>> addFlows(List<WDiEdge<V,Integer>> path, Integer p, List<WDiEdge<V,Integer>> sol) {
        List<WDiEdge<V,Integer>> resultado = sol;

        for(WDiEdge<V, Integer> arco : path)
            resultado = addFlow(arco.getSrc(), arco.getDst(), p, resultado);
        
        return resultado;
    }

    public FordFulkerson(WeightedDiGraph<V,Integer> g, V src, V dst) {
        this.sol = new LinkedList<>();
        this.src = src;
        this.dst = dst;
        WeightedDiGraph<V,Integer> wdg = g;

        int mf;
        List<WDiEdge<V,Integer>> edges;
        WeightedBreadthFirstTraversal<WDiEdge<V,Integer>> pathBeginningInSrc = new WeightedBreadthFirstTraversal<>(g, src);
        List<WDiEdge<V,Integer>> pathFromSrcToDst = pathBeginningInSrc.pathTo(dst);

        while(pathFromSrcToDst != null && !pathFromSrcToDst.isEmpty()){
            mf = maxFlowPath(pathFromSrcToDst);
            edges = wdg.wDiEdges();
            edges = updateEdges(pathFromSrcToDst, -mf, edges);
            edges = updateEdges(reversePath(pathFromSrcToDst), mf, edges);
            wdg = new WeightedDictionaryDiGraph(wdg.vertices(), edges);
            this.sol = addFlows(pathFromSrcToDst, mf, sol);

            pathBeginningInSrc = new WeightedBreadthFirstTraversal<>(wdg, src);
            pathFromSrcToDst = pathBeginningInSrc.pathTo(dst);
        }

        this.g = wdg;
    }

    private List<WDiEdge<V,Integer>> reversePath(List<WDiEdge<V,Integer>> path){
        List<WDiEdge<V,Integer>> res = new LinkedList<>();

        for(WDiEdge<V,Integer> arco : path)
            res.append(new WDiEdge<>(arco.getDst(), arco.getWeight(), arco.getSrc()));

        return res;
    }

    public int maxFlow() {
        int flujoMax = 0;

        for(WDiEdge<V,Integer> arco : sol){
            if(arco.getSrc().equals(src))
                flujoMax += arco.getWeight();
        }

        return flujoMax;
    }

    public int maxFlowMinCut(Set<V> set) {
        int flujoMax = 0;

        for(WDiEdge<V,Integer> arco : g.wDiEdges()){
            if(set.isElem(arco.getSrc()) && !set.isElem(arco.getDst()))
                flujoMax += arco.getWeight();
            else if(!set.isElem(arco.getSrc()) && set.isElem(arco.getDst()))
                flujoMax -= arco.getWeight();
        }

        return flujoMax;
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
        boolean hayEquilibrioTotal = true;

        for(V vertice : g.vertices()){
            if(!vertice.equals(src) && !vertice.equals(dst) && g.inDegree(vertice) != g.outDegree(vertice))
                hayEquilibrioTotal = false;
        }

        return hayEquilibrioTotal;
    }

    public static <V,W> Tuple2<List<V>,List<V>> sourcesAndSinks(WeightedDiGraph<V,W> g) {
        Tuple2<List<V>,List<V>> fuentesYSumideros = new Tuple2<>(new LinkedList<>(), new LinkedList<>());

        for(V vertice : g.vertices()){
            if(g.inDegree(vertice) == 0)
                fuentesYSumideros._1().append(vertice);
            else if(g.outDegree(vertice) == 0)
                fuentesYSumideros._2().append(vertice);
        }

        return fuentesYSumideros;
    }

    public static <V> void unifySourceAndSink(WeightedDiGraph<V,Integer> g, V newSrc, V newDst) {
        Tuple2<List<V>,List<V>> fuentesYSumideros = sourcesAndSinks(g);

        if(fuentesYSumideros._1().size() > 1){
            V nuevaFuente = newSrc;
            g.addVertex(nuevaFuente);

            for(V vertice : fuentesYSumideros._1())
                g.addDiEdge(nuevaFuente, pesoFuente(vertice, g), vertice);
        }

        if(fuentesYSumideros._2().size() > 1){
            V nuevoSumidero = newDst;
            g.addVertex(nuevoSumidero);

            for(V vertice : fuentesYSumideros._2())
                g.addDiEdge(vertice, pesoSumidero(vertice, g), nuevoSumidero);
        }
    }

    private static <V> int pesoFuente(V vertice, WeightedDiGraph<V,Integer> g){
        int sumaPesos = 0;

        for(WDiEdge<V,Integer> arco : g.wDiEdges()){
            if(arco.getSrc().equals(vertice))
                sumaPesos += arco.getWeight();
        }

        return sumaPesos;
    }

    private static <V> int pesoSumidero(V vertice, WeightedDiGraph<V,Integer> g){
        int sumaPesos = 0;

        for(WDiEdge<V,Integer> arco : g.wDiEdges()){
            if(arco.getDst().equals(vertice))
                sumaPesos += arco.getWeight();
        }

        return sumaPesos;
    }
}
