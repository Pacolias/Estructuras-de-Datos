package dataStructures.dictionary;
import dataStructures.list.List;

import dataStructures.list.ArrayList;
import dataStructures.set.AVLSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.util.Dictionary;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de septiembre de 2018.
 *
 * Apellidos, Nombre: Molina Cuenca, Francisco Javier
 * Titulacion, Grupo: Doble Grado en Matematicas + Ingenieria Informatica
 */
public class HashBiDictionary<K,V> implements BiDictionary<K,V>{
	private Dictionary<K,V> bKeys;
	private Dictionary<V,K> bValues;
	
	public HashBiDictionary() {
		bKeys = new HashDictionary<>();
		bValues = new HashDictionary<>();
	}
	
	public boolean isEmpty() {
		return bKeys.isEmpty() && bValues.isEmpty();
	}
	
	public int size() {
		return bKeys.size();
	}
	
	public void insert(K k, V v) {
		if(bKeys.isDefinedAt(k)){
			K oldKey = k;
			V oldValue = v;
			bKeys.delete(k);
			bValues.delete(v);
		}

		bKeys.insert(k, v);
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
		if(bKeys.isDefinedAt(k)){
			V valor = bKeys.valueOf(k);
			bValues.delete(valor);
			bKeys.delete(k);
		}
	}
	
	public void deleteByValue(V v) {
		if(bValues.isDefinedAt(v)){
			K clave = bValues.valueOf(v);
			bKeys.delete(clave);
			bValues.delete(v);
		}
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
			throw new IllegalArgumentException("El diccionario pasado como parametro no es inyectivo");
		
		BiDictionary biDict = new HashBiDictionary<>();
		biDict.bKeys = dict;
		biDict.bValues = reverse(dict);

		return biDict;
	}

	private static <K,V extends Comparable<? super V>> boolean isInjective(Dictionary<K,V> dict){
		Set<K> conjuntoClaves = new AVLSet();
		Set<V> conjuntoValues = new AVLSet();

		for(K clave : dict.keys())
			conjuntoClaves.insert(clave);

		for(V valor : dict.values())
			conjuntoValues.insert(valor);

		return conjuntoClaves.size() == conjuntoValues.size();
	}

	private static <K,V extends Comparable<? super V>> Dictionary<V, K> reverse(Dictionary<K,V> dict){
		Dictionary<V, K> reverseDict = new HashDictionary();

		for(Tuple2<K,V> t : dict.keysValues())
				reverseDict.insert(t._2(), t._1());
		
		return reverseDict;
	}
	
	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
		BiDictionary<K, W> composedDict = new HashBiDictionary<>();

		for(Tuple2<K,V> t : bKeys.keysValues()){
			if(bdic.isDefinedKeyAt(t._2()))
				composedDict.insert(t._1(), bdic.valueOf(t._2()));
		}

		return composedDict;
	}
		
	public static <K,V extends Comparable<? super V>> boolean isPermutation(BiDictionary<K,K> bd) {
		Set<K> conjuntoClaves = new AVLSet();
		Set<K> conjuntoValues = new AVLSet();

		for(K clave : dict.bKeys.keys())
			conjuntoClaves.insert(clave);

		for(V valor : dict.bValues.values())
			conjuntoValues.insert(valor);

		return equals(conjuntoClaves, conjuntoValues);
	}

	private static <K extends Comparable<? super K>> boolean equals(Set<K> claves, Set<K> valores) {
		boolean sonIguales = true;

		Iterator<K> conjunto1 = claves.iterator();
		while(conjunto1.hasNext())
			sonIguales = valores.isElem(conjunto1.next());
		
		Iterator<K> conjunto2 = valores.iterator();
		while(conjunto2.hasNext())
			sonIguales = claves.isElem(conjunto2.next());
		
		return sonIguales;
	}
	
	// Solo alumnos con evaluación por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		if(!isPermutation(bd))
			throw new IllegalArgumentException("El diccionario pasado como parametro no es una permutacion");

		List<K> orbita = new ArrayList<>();
		orbita.add(k);
		
		boolean esK = false;
		K clave = k;

		while(!esK){
			clave = bd.valueOf(clave);

			if(!clave.equals(k))
				orbita.add(clave);			
			else
				esK = true;
		}

		return orbita;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		if(!isPermutation(bd))
			throw new IllegalArgumentException("El diccionario pasado como parametro no es una permutacion");

		List<List<K>> ciclos = new ArrayList<>();

		for(K elemento : bd.bKeys.keys()){
			if(!esCicloCalculado(elemento, ciclos))
				ciclos.add(orbitOf(elemento, bd));
		}

		return ciclos;
	}

	private static <K extends Comparable<? super K>> boolean esCicloCalculado(K elemento, List<List<K>> ciclos){
		boolean esElementoCicloCalculado = false;
		Iterator<List<K>> itCiclos = ciclos.iterator();
		List<K> ciclo = null;
		Iterator<K> itElementosCiclo = null;

		while(!esElementoCicloCalculado && itCiclos.hasNext()){
			ciclo = itCiclos.next();
			itElementosCiclo = ciclo.iterator();

			while(!esElementoCicloCalculado && itElementosCiclo.hasNext()){
				if(elemento.equals(itElementosCiclo.next()))
					esElementoCicloCalculado = true;
			}
		}

		return esElementoCicloCalculado;
	}

    // =====================================
	
	
	@Override
	public String toString() {
		return "HashBiDictionary [bKeys=" + bKeys + ", bValues=" + bValues + "]";
	}
	
	
}
