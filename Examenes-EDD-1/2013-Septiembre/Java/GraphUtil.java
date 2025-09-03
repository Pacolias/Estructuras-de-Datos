/** ------------------------------------------------------------------------------
  * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
  * Diámetro de un grafo conexo
  * (completa y sustituye los siguientes datos)
  * Titulación: Doble Grado en Matemáticas e Ingeniería Informática
  * Alumno: Paco Molina Cuenca
  * -------------------------------------------------------------------------------
  */

import dataStructures.graph.BreadthFirstTraversal;
import dataStructures.graph.Graph;
import dataStructures.list.ArrayList;

import java.util.Iterator;


public class GraphUtil {

	/**
	 * LENGTH: Calcula el número de elementos que contiene un iterable
	 * 
	 * @param it  El iterador
	 * @return   Número de elementos en el iterador
	 */
	public static <T> int length(Iterable<T> it) {
		int res = 0;
		for (T elem : it){
			res++;
		}
		return res;
	}

	/**
	 * ECCENTRICITY: Calcula la excentricidad de un vértice en un grafo El algoritmo toma la
	 * longitud del camino máximo en un recorrido en profundidad del grafo
	 * comenzando en el vértice dado.
	 * 
	 * @param graph    Grafo
	 * @param v        Vértice del grafo
	 * @return         Excentricidad del vértice
	 */
	public static <T> int eccentricity(Graph<T> graph, T v) {
		int excentricidad = 0;
		BreadthFirstTraversal<T> bft = new BreadthFirstTraversal<>(graph, v);

		for(Iterable<T> path : bft.paths()){
			if(length(path) > excentricidad)
				excentricidad = length(path);
		}

		return excentricidad - 1;
	}

	/**
	 * DIAMETER: Se define como la máxima excentricidad de los vértices del grafo.
	 * 
	 * @param graph
	 * @return
	 */

	public static <T> int diameter(Graph<T> graph) {
		List<Integer> excentricidades = new ListArray<>();

		for(T v : graph.vertices())
			excentricidades.append(eccentricity(graph, v));

		int diametro = 0;


		for(Integer excentricidad : excentricidades){
			if(excentricidad > diametro)
				diametro = excentricidad;
		}
		
		return diametro;
	}
	
	/** 
	 * Estima y justifica la complejidad del método diameter
	 */
}
