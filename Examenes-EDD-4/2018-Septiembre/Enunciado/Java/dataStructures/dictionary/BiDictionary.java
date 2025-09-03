package dataStructures.dictionary;

import dataStructures.list.List;
import dataStructures.tuple.Tuple2;

public interface BiDictionary<K, V> {

    // Comprueba si el diccionario está vacío
    boolean isEmpty();

    // Devuelve el número de pares clave-valor
    int size();

    // Inserta un nuevo par clave-valor
    void insert(K k, V v);

    // Obtiene el valor asociado a una clave
    V valueOf(K k);

    // Obtiene la clave asociada a un valor
    K keyOf(V v);

    // Comprueba si existe una clave
    boolean isDefinedKeyAt(K k);

    // Comprueba si existe un valor
    boolean isDefinedValueAt(V v);

    // Elimina un par por clave
    void deleteByKey(K k);

    // Elimina un par por valor
    void deleteByValue(V v);

    // Devuelve todas las claves
    Iterable<K> keys();

    // Devuelve todos los valores
    Iterable<V> values();

    // Devuelve todos los pares clave-valor
    Iterable<Tuple2<K, V>> keysValues();

    // Convierte un Dictionary<K,V> en un BiDictionary<K,V>
    static <K, V extends Comparable<? super V>> BiDictionary<K, V> toBiDictionary(Dictionary<K, V> dict) {
        throw new UnsupportedOperationException();
    }

    // Composición de bi-diccionarios
    <W> BiDictionary<K, W> compose(BiDictionary<V, W> bdic);

    // Comprueba si un BiDictionary<K,K> es una permutación
    static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
        throw new UnsupportedOperationException();
    }

    // Órbita de un elemento (solo para permutaciones)
    static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
        throw new UnsupportedOperationException();
    }

    // Todos los ciclos de la permutación
    static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
        throw new UnsupportedOperationException();
    }
}
