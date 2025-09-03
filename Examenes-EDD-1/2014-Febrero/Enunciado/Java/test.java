// Main.java
// Prueba de SCCs para examen Febrero 2014

package dataStructures.graph;

import java.util.Set;
import java.util.HashSet;

public class Main {
  public static void main(String[] args) {
    DiGraph<Character> g = new DictionaryGraph<>();

    // Añadir vértices
    for(char c : new char[]{'A','B','C','D','E','F','G'})
      g.addVertex(c);

    // Añadir aristas dirigidas
    g.addEdge('A', 'B');
    g.addEdge('B', 'E');
    g.addEdge('E', 'A');
    g.addEdge('B', 'F');
    g.addEdge('F', 'G');
    g.addEdge('G', 'F');
    g.addEdge('C', 'D');
    g.addEdge('D', 'C');

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

