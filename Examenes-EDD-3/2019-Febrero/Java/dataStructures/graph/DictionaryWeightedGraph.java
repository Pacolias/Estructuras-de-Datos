/**----------------------------------------------
 * -- Estructuras de Datos.  2018/19
 * -- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
 * -- Escuela Técnica Superior de Ingeniería en Informática. UMA
 * --
 * -- Examen 4 de febrero de 2019
 * --
 * -- ALUMNO/NAME: Paco Molina Cuenca
 * -- GRADO/STUDIES: Doble Grado Matemáticas e Ingeniería Informática
 * -- NÚM. MÁQUINA/MACHINE NUMBER:
 * --
 * ----------------------------------------------
 */

package dataStructures.graph;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;
import java.util.Iterator;

public class DictionaryWeightedGraph<V, W extends Comparable<? super W>> implements WeightedGraph<V, W> {

    static class WE<V1, W1 extends Comparable<? super W1>> implements WeightedEdge<V1, W1> {

		V1 src, dst;
        W1 wght;

        WE(V1 s, V1 d, W1 w) {
            src = s;
            dst = d;
            wght = w;
        }

        public V1 source() {
            return src;
        }

        public V1 destination() {
            return dst;
        }

        public W1 weight() {
            return wght;
        }

        public String toString() {
            return "WE(" + src + "," + dst + "," + wght + ")";
        }

		  public int hashCode() {
			  return this.src.hashCode()+this.dst.hashCode()+this.wght.hashCode();
		  }

		  public boolean equals(Object obj) {
              Boolean itsWeightedEdge = obj instanceof DictionaryWeightedGraph.WE<?,?>;
              WE<V1, W1> object = itsWeightedEdge ? (WE<V1, W1>) obj : null;
              Boolean res = object.destination().equals(this.destination())
                      && object.source().equals(this.source())
                      && object.weight().equals(this.weight());
              return res;
		  }

		  public int compareTo(WeightedEdge<V1, W1> o) {
			  return weight().compareTo(o.weight());
		  }
    }

    /**
     * Each vertex is associated to a dictionary containing associations
     * from each successor to its weight
     */
    protected Dictionary<V, Dictionary<V, W>> graph;

    public DictionaryWeightedGraph() {
        graph = new HashDictionary<>();
    }


    public void addVertex(V v) {
        graph.insert(v, new HashDictionary<>());
    }

    public void addEdge(V src, V dst, W w) {
        if(!(graph.isDefinedAt(src) && graph.isDefinedAt(dst)))
            throw new GraphException("the graph is not defined on some of the given vertex");

        graph.valueOf(src).insert(dst, w);
        graph.valueOf(dst).insert(src, w);
    }

    public Set<Tuple2<V, W>> successors(V v) {
        if(!(graph.isDefinedAt(v)))
            throw new GraphException("the graph is not defined on the given vertex");

        Set<Tuple2<V, W>> res = new HashSet<>();

        for(Tuple2<V, W> par : graph.valueOf(v).keysValues())
            res.insert(par);

        return res;
    }


    public Set<WeightedEdge<V, W>> edges() {
        Set<WeightedEdge<V, W>> edges = new HashSet<>();

        for(V src : graph.keys()){
            for(Tuple2<V,W> par : graph.valueOf(src).keysValues())
                edges.insert(new WE<>(src, par._1(), par._2()));
        }

        return edges;
    }

    /** DON'T EDIT ANYTHING BELOW THIS COMMENT **/


    public Set<V> vertices() {
        Set<V> vs = new HashSet<>();
        for (V v : graph.keys())
            vs.insert(v);
        return vs;
    }


    public boolean isEmpty() {
        return graph.isEmpty();
    }

    public int numVertices() {
        return graph.size();
    }


    public int numEdges() {
        int num = 0;
        for (Dictionary<V, W> d : graph.values())
            num += d.size();
        return num / 2;
    }


    public String toString() {
        String className = getClass().getSimpleName();
        String s = className + "(vertices=(";

        Iterator<V> it1 = vertices().iterator();
        while (it1.hasNext())
            s += it1.next() + (it1.hasNext() ? ", " : "");
        s += ")";

        s += ", edges=(";
        Iterator<WeightedEdge<V, W>> it2 = edges().iterator();
        while (it2.hasNext())
            s += it2.next() + (it2.hasNext() ? ", " : "");
        s += "))";

        return s;
    }
}
