package dataStructures.graph;

import java.util.*;

public class Main {
    public static void main(String[] args) {

        // Lista de vértices
        List<Character> vertices = Arrays.asList('A','B','C','D','E','F','G','H');

        // Lista de aristas dirigidas (A -> B, etc.)
        List<DiEdge<Character>> edges = List.of(
            new DiEdge<>('A', 'B'),
            new DiEdge<>('B', 'E'),
            new DiEdge<>('E', 'A'),
            new DiEdge<>('B', 'F'),
            new DiEdge<>('F', 'G'),
            new DiEdge<>('G', 'F'),
            new DiEdge<>('C', 'D'),
            new DiEdge<>('D', 'C'),
            new DiEdge<>('H', 'D'),
            new DiEdge<>('H', 'G'),
            new DiEdge<>('C', 'G'),
            new DiEdge<>('D', 'H')
        );

        // Construcción del grafo original
        DiGraph<Character> g = DiGraph.fromEdges(vertices, edges);

        System.out.println("Grafo:");
        System.out.println(g);

        System.out.println("\nGrafo inverso:");
        System.out.println(SCCs.reverseDiGraph(g));

        Set<Character> vs = Set.of('A', 'B', 'E', 'F', 'G');
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
