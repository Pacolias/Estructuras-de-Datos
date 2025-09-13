/**
 * @author Blas Ruiz, Data Structures, Grado en Informtica. UMA.
 * Alumno: Paco Molina Cuenca
 * Control 2 del 13-Febrero-2012
 * Estudio de grafos bipartitos por coloreado con búsqueda en profundidad
 */

package graph;

import dictionary.Dictionary;
import dictionary.HashDictionary;
import stack.Stack;
import stack.StackList;


public class BiPartite<V> {
	
	public static enum Color {Red, Blue;
	}

	private static Color nextColor(Color c) {
		return (c == Color.Blue) ?Color.Red:Color.Blue; 
	}
	
	private Stack<Pair<V,Color>> stack; // stack with pair of vertex and color
	private Dictionary<V,Color> dict; // dictionary: Vertices -> Color
	private boolean isBiColored;

	public BiPartite(Graph<V> graph) {
		dict = new HashDictionary<V, Color>();
		stack = new StackList<Pair<V, Color>>();
		isBiColored = true;
		if (graph.numVertices() == 0){
			return;
		}

		V src = graph.vertices().iterator().next(); // initial vertex
		stack.push(new Pair<V,Color>(src,Color.Red));
		
		while (!stack.isEmpty()) {
		  // TODO
		} 
	}

	private boolean isDefinedAt(V v, Dictionary<V, Color> dict) {
		boolean res = false;
		for (V vertex : dict.keys()){
			if (vertex.equals(v)){
				res = true;
			}
		}
		return res;
	}


	public Dictionary<V,Color> biColored() {
		return dict;
	}
	
	public boolean isBicolored() {
		return isBiColored;
	}
	
}
