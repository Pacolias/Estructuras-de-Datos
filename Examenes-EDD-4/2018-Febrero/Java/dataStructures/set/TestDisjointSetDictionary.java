package dataStructures.set;

import dataStructures.list.List;

public class TestDisjointSetDictionary {

    public static void main(String[] args) {
        DisjointSetDictionary<String> ds = new DisjointSetDictionary<>();

        System.out.println("¿Está vacío?: " + ds.isEmpty());
        System.out.println("Número de elementos: " + ds.numElements());

        System.out.println("\nAñadiendo elementos a, b, c...");
        ds.add("a");
        ds.add("b");
        ds.add("c");

        System.out.println("¿Está vacío?: " + ds.isEmpty());
        System.out.println("Número de elementos: " + ds.numElements());
        System.out.println("¿Contiene 'a'?: " + ds.isElem("a"));
        System.out.println("¿Contiene 'z'?: " + ds.isElem("z"));

        System.out.println("\nProbando areConnected:");
        System.out.println("¿a conectado con b?: " + ds.areConnected("a", "b"));
        System.out.println("¿a conectado con a?: " + ds.areConnected("a", "a"));

        System.out.println("\nUnimos a y b...");
        ds.union("a", "b");
        System.out.println("¿a conectado con b?: " + ds.areConnected("a", "b"));

        System.out.println("\nClase de equivalencia de a: " + ds.kind("a"));
        System.out.println("Clase de equivalencia de c: " + ds.kind("c"));

        System.out.println("\nTodas las clases de equivalencia:");
        List<List<String>> clases = ds.kinds();
        for (List<String> clase : clases) {
            System.out.println(clase);
        }

        System.out.println("\nProbando excepciones:");
        try {
            ds.union("a", "z");
        } catch (IllegalArgumentException e) {
            System.out.println("Excepción esperada al unir con 'z': " + e.getMessage());
        }

        System.out.println("\nRepresentación toString:");
        System.out.println(ds);
    }
}
