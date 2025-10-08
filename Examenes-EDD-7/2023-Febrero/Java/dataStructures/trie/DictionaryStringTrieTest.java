package dataStructures.trie;

import dataStructures.list.*;

public class DictionaryStringTrieTest {

    public static void main(String[] args) {
        System.out.println("== Test básico de DictionaryStringTrie ==");

        // Crear trie vacío
        DictionaryStringTrie<Integer> trie = new DictionaryStringTrie<>();
        System.out.println("Trie recién creado:");
        System.out.println(trie);
        System.out.println("¿Está vacío?: " + trie.isEmpty()); // true

        // Insertar palabras
        trie.insert("to", 1);
        trie.insert("tea", 2);
        trie.insert("ten", 3);

        System.out.println("\nTrie después de insertar 'to', 'tea', 'ten':");
        System.out.println(trie);
        System.out.println("¿Está vacío?: " + trie.isEmpty()); // false
        System.out.println("Tamaño: " + trie.size()); // 3

        // Buscar palabras
        System.out.println("\n== Búsquedas ==");
        System.out.println("Buscar 'to': " + trie.search("to"));   // 1
        System.out.println("Buscar 'tea': " + trie.search("tea")); // 2
        System.out.println("Buscar 'ten': " + trie.search("ten")); // 3
        System.out.println("Buscar 'te': " + trie.search("te"));   // null
        System.out.println("Buscar 'toast': " + trie.search("toast")); // null

        // Probar sampleTrie()
        DictionaryStringTrie<Integer> sample = DictionaryStringTrie.sampleTrie();
        System.out.println("\n== sampleTrie() ==");
        System.out.println(sample);
        System.out.println("Tamaño de sampleTrie: " + sample.size()); // 6 esperadas

        System.out.println("\nBúsqueda en sampleTrie:");
        System.out.println("Buscar 'bat': " + sample.search("bat")); // 0
        System.out.println("Buscar 'be': " + sample.search("be"));   // 1
        System.out.println("Buscar 'bed': " + sample.search("bed")); // 2
        System.out.println("Buscar 'toe': " + sample.search("toe")); // 5
        System.out.println("Buscar 'ca': " + sample.search("ca"));   // null
        System.out.println("Buscar 'cat': " + sample.search("cat")); // 3

        // (Opcional) Comprobar equals
        DictionaryStringTrie<Integer> another = DictionaryStringTrie.sampleTrie();
        System.out.println("\n¿sampleTrie es igual a otro sampleTrie?: " + sample.equals(another)); // true

                // Test de strings()
        System.out.println("\n== Test de strings() ==");
        List<String> strings = trie.strings();
        System.out.println("Palabras en el trie: " + strings); // ["tea", "ten", "to"]

        // Test de fromList()
        System.out.println("\n== Test de fromList() ==");
        List<String> wordList = new LinkedList<>();
        wordList.append("apple");
        wordList.append("ape");
        wordList.append("bat");

        DictionaryStringTrie<Integer> fromListTrie = DictionaryStringTrie.fromList(wordList);
        System.out.println("Tamaño del trie creado desde la lista: " + fromListTrie.size()); // 3
        System.out.println("Palabras en el trie: " + fromListTrie.strings()); // ["ape", "apple", "bat"]
    }
}

