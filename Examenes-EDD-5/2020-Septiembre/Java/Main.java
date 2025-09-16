import dataStructures.list.*;

public class Main {
    public static void main(String[] args) {
        System.out.println("=== Prueba de Bin Packing con AVL en Java ===");

        int capacity = 10;
        int[] weights = {4, 8, 1, 4, 2, 1, 7, 2, 3, 5};

        AVL tree = new AVL();
        for (int w : weights) {
            tree.addFirst(capacity, w);
        }

        System.out.println("Se han añadido " + weights.length + " objetos.");
        List<Bin> bins = tree.toList();
        System.out.println("Número de cubos usados: " + bins.size());
        System.out.println("Contenido de los cubos:");
        for (Bin bin : bins) {
            System.out.println(bin);
        }

        System.out.println("\n=== Prueba de rotaciones AVL ===");
        AVL testTree = new AVL();
        testTree.addFirst(capacity, 3);
        testTree.addFirst(capacity, 2);
        testTree.addFirst(capacity, 4);

        System.out.println("Árbol tras inserciones:");
        for (Bin bin : testTree.toList()) {
            System.out.println(bin);
        }
    }
}
