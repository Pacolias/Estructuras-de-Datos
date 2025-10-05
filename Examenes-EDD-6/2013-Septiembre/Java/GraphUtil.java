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

	}

	/**
	 * DIAMETER: Se define como la máxima excentricidad de los vértices del grafo.
	 * 
	 * @param graph
	 * @return
	 */

	public static <T> int diameter(Graph<T> graph) {

	}
	
	/** 
	 * Estima y justifica la complejidad del método diameter
	 */
}
