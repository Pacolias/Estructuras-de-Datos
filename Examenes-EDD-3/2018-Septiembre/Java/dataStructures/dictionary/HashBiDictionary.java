package dataStructures.dictionary;
import dataStructures.list.List;

import dataStructures.list.ArrayList;
import dataStructures.list.LinkedList;
import dataStructures.set.AVLSet;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de septiembre de 2018.
 *
 * Apellidos, Nombre:
 * Titulacion, Grupo:
 */
public class HashBiDictionary<K,V> implements BiDictionary<K,V>{
	private Dictionary<K,V> bKeys;
	private Dictionary<V,K> bValues;
	
	public HashBiDictionary() {
		bKeys = new HashDictionary<>();
		bValues = new HashDictionary<>();
	}
	
	public boolean isEmpty() {
		return bKeys.isEmpty();
	}
	
	public int size() {
		return bKeys.size();
	}
	
	public void insert(K k, V v) {
		bKeys.delete(k);
		bKeys.insert(k, v);
		bValues.delete(v);
		bValues.insert(v, k);
	}
	
	public V valueOf(K k) {
		return bKeys.valueOf(k);
	}
	
	public K keyOf(V v) {
		return bValues.valueOf(v);
	}
	
	public boolean isDefinedKeyAt(K k) {
		return bKeys.isDefinedAt(k);
	}
	
	public boolean isDefinedValueAt(V v) {
		return bValues.isDefinedAt(v);
	}
	
	public void deleteByKey(K k) {
		bValues.delete(bKeys.valueOf(k));
		bKeys.delete(k);
	}
	
	public void deleteByValue(V v) {
		bKeys.delete(bValues.valueOf(v));
		bValues.delete(v);
	}
	
	public Iterable<K> keys() {
		return bKeys.keys();
	}
	
	public Iterable<V> values() {
		return bValues.keys();
	}
	
	public Iterable<Tuple2<K, V>> keysValues() {
		return bKeys.keysValues();
	}
	
		
	public static <K,V extends Comparable<? super V>> BiDictionary<K, V> toBiDictionary(Dictionary<K,V> dict) {
		if(!isInjective(dict))
			throw new IllegalArgumentException("toBiDictionary on non injective dictionary");
		
		BiDictionary<K,V> biDict = new HashBiDictionary<>();

		for(Tuple2<K,V> par : dict.keysValues())
			biDict.insert(par._1(), par._2());

		return biDict;
	}

	private static <K,V extends Comparable<? super V>> boolean isInjective(Dictionary<K,V> dict){
		List<K> keys = new LinkedList<>();
		List<V> values = new LinkedList<>();

		for(K k : dict.keys())
			keys.append(k);

		for(V v : dict.values())
			values.append(v);
		
		return keys.size() == values.size();
	}
	
	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
		BiDictionary<K,W> res = new HashBiDictionary<>();

		for(K a : bKeys.keys()){
			if(bdic.isDefinedKeyAt(bKeys.valueOf(a)))
				res.insert(a, bdic.valueOf(bKeys.valueOf(a)));
		}

		return res;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
		List<K> keys = new LinkedList<>();
		List<K> values = new LinkedList<>();

		for(K k : bd.keys())
			keys.append(k);

		for(K k : bd.values())
			values.append(k);

		return keys.size() == values.size();
	}
	
	// Solo alumnos con evaluación por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		if(!isPermutation(bd))
			throw new IllegalArgumentException("orbitOf on non permutation BiDictionary");

		List<K> res = new LinkedList<>();
		K elem = k;

		while(!(elem.equals(k) && res.size() > 0)){
			res.append(elem);
			elem = bd.valueOf(elem);
		}

		return res;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		if(!isPermutation(bd))
			throw new IllegalArgumentException("orbitOf on non permutation BiDictionary");		
		
		List<List<K>> ciclos = new LinkedList<>();
		List<K> ciclo;
		Set<K> claves = new HashSet<>();

		for(K k : bd.keys()){
			if(!claves.isElem(k)){
				ciclo = orbitOf(k, bd);

				for(K clave : ciclo)
					claves.insert(clave);
				
				ciclos.append(ciclo);
			}
		}

		return ciclos;
	}

    // =====================================
	
	
	@Override
	public String toString() {
		return "HashBiDictionary [bKeys=" + bKeys + ", bValues=" + bValues + "]";
	}
	
	
}
