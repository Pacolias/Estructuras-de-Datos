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

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.WeightedGraph;
import dataStructures.graph.WeightedGraph.WeightedEdge;
import dataStructures.priorityQueue.LinkedPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.set.HashSet;
import dataStructures.set.Set;

public class Kruskal {
	public static <V,W> Set<WeightedEdge<V,W>> kruskal(WeightedGraph<V,W> g){
		Dictionary<V,V> dict = new HashDictionary<>();
		
		for(V v : g.vertices())
			dict.insert(v, v);

		PriorityQueue<WeightedEdge<V,W>> pq = new LinkedPriorityQueue<>();

		for(WeightedEdge<V,W> edge : g.edges())
			pq.enqueue(edge);

		Set<WeightedEdge<V,W>> t = new HashSet<>();

		WeightedEdge<V,W> edge;

		V repOrg, repDst;

		while(!pq.isEmpty()){
			edge = pq.first();
			pq.dequeue();

			repOrg = representante(edge.source(), dict);
			repDst = representante(edge.destination(), dict);

			if(!repOrg.equals(repDst)){
				dict.insert(repDst, edge.source());
				t.insert(edge);
			}
		}

		return t;
	}

	private static <V> V representante(V v, Dictionary<V,V> dict){
		return dict.valueOf(v).equals(v) ? v : representante(dict.valueOf(v), dict);
	}

	// Sólo para evaluación continua / only for part time students
	public static <V,W> Set<Set<WeightedEdge<V,W>>> kruskals(WeightedGraph<V,W> g) {

		// COMPLETAR
		
		return null;
	}
}
