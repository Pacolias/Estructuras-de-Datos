// Main.java
// Prueba de SCCs para examen Febrero 2014

package dataStructures.graph;

import static dataStructures.graph.DiGraph.fromEdges;
import java.util.HashSet;
import java.util.Set;

public class test {
  public static void main(String[] args) {
    // Añadir vértices
    Set<Character> vertices = new HashSet<>();
    vertices.add('A');
    vertices.add('B');
    vertices.add('C');
    vertices.add('D');
    vertices.add('E');
    vertices.add('F');
    vertices.add('G');
    vertices.add('H');

    // Añadir aristas dirigidas
    Set<DiEdge<Character>> edges = new HashSet<>();
    edges.add(new DiEdge<>('A', 'B'));
    edges.add(new DiEdge<>('B', 'F'));
    edges.add(new DiEdge<>('B', 'E'));
    edges.add(new DiEdge<>('C', 'D'));
    edges.add(new DiEdge<>('C', 'G'));
    edges.add(new DiEdge<>('D', 'H'));
    edges.add(new DiEdge<>('D', 'C'));
    edges.add(new DiEdge<>('E', 'A'));
    edges.add(new DiEdge<>('E', 'F'));
    edges.add(new DiEdge<>('F', 'G'));
    edges.add(new DiEdge<>('G', 'F'));
    edges.add(new DiEdge<>('H', 'G'));
    edges.add(new DiEdge<>('H', 'D'));

    DiGraph<Character> g = fromEdges(vertices, edges);

    System.out.println("Grafo:");
    System.out.println(g);

    System.out.println("\nGrafo inverso:");
    System.out.println(SCCs.reverseDiGraph(g));

    Set<Character> vs = new HashSet<>();
    vs.add('A'); vs.add('B'); vs.add('E'); vs.add('F'); vs.add('G');

    System.out.println("\nSubgrafo restringido:");
    System.out.println(SCCs.restrictDiGraph(g, vs));

    System.out.println("\nSCC de A:");
    System.out.println(SCCs.sccOf(g, 'A'));

    System.out.println("\nSCC de C:");
    System.out.println(SCCs.sccOf(g, 'C'));

    System.out.println("\nTodas las SCCs:");
    System.out.println(SCCs.stronglyConnectedComponentsDiGraph(g));
  }
}
