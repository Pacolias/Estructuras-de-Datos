package dataStructures.dictionary;

import dataStructures.tuple.Tuple2;
import dataStructures.list.List;
import dataStructures.list.ArrayList;

public class TestHashBiDictionaryAll {

    public static void main(String[] args) {
        System.out.println("=== Creando HashBiDictionary ===");
        HashBiDictionary<Integer, String> bd = new HashBiDictionary<>();

        // Insertar pares
        bd.insert(1, "a");
        bd.insert(2, "b");
        bd.insert(3, "c");
        bd.insert(4, "d");

        System.out.println("\nDiccionario inicial:");
        for (Tuple2<Integer, String> kv : bd.keysValues()) {
            System.out.println(kv._1() + " -> " + kv._2());
        }

        // Consultas
        System.out.println("\nvalueOf(2): " + bd.valueOf(2));
        System.out.println("keyOf('c'): " + bd.keyOf("c"));
        System.out.println("isDefinedKeyAt(3): " + bd.isDefinedKeyAt(3));
        System.out.println("isDefinedValueAt('x'): " + bd.isDefinedValueAt("x"));

        // Eliminaciones
        bd.deleteByKey(1);
        bd.deleteByValue("d");

        System.out.println("\nDiccionario después de eliminar 1 y 'd':");
        for (Tuple2<Integer, String> kv : bd.keysValues()) {
            System.out.println(kv._1() + " -> " + kv._2());
        }

        // keys y values
        System.out.println("\nClaves:");
        for (Integer k : bd.keys()) System.out.println(k);
        System.out.println("Valores:");
        for (String v : bd.values()) System.out.println(v);

        // Tamaño y vacío
        System.out.println("\nTamaño: " + bd.size());
        System.out.println("¿Está vacío? " + bd.isEmpty());

        // toBiDictionary (crea un nuevo BiDictionary a partir de un Dictionary)
        Dictionary<Integer, String> dict = new HashDictionary<>();
        dict.insert(10, "x");
        dict.insert(20, "y");
        BiDictionary<Integer, String> bd2 = HashBiDictionary.toBiDictionary(dict);
        System.out.println("\nDiccionario creado con toBiDictionary:");
        for (Tuple2<Integer, String> kv : bd2.keysValues()) {
            System.out.println(kv._1() + " -> " + kv._2());
        }

        // compose
        BiDictionary<String, Character> bd3 = new HashBiDictionary<>();
        bd3.insert("a", 'A');
        bd3.insert("b", 'B');
        bd3.insert("c", 'C');
        BiDictionary<Integer, Character> composed = bd.compose(bd3);
        System.out.println("\nDiccionario resultante de compose:");
        for (Tuple2<Integer, Character> kv : composed.keysValues()) {
            System.out.println(kv._1() + " -> " + kv._2());
        }

        // isPermutation (necesita BiDictionary<K,K>)
        HashBiDictionary<Integer, Integer> bdPerm = new HashBiDictionary<>();
        bdPerm.insert(1, 2);
        bdPerm.insert(2, 3);
        bdPerm.insert(3, 1);
        System.out.println("\nisPermutation: " + HashBiDictionary.isPermutation(bdPerm));

        // orbitOf y cyclesOf
        System.out.println("\norbitOf(1): " + HashBiDictionary.orbitOf(1, bdPerm));
        System.out.println("cyclesOf: " + HashBiDictionary.cyclesOf(bdPerm));
    }
}
