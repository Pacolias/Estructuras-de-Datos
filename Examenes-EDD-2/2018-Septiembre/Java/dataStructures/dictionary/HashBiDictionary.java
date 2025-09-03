package dataStructures.dictionary;
import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

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
		if(bKeys.isDefinedAt(k)){
			V valor = bKeys.valueOf(k);
			bKeys.delete(k);
			bValues.delete(valor);
		}

		bKeys.insert(k, v);

		if(bValues.isDefinedAt(v)){
			K clave = bValues.valueOf(v);
			bValues.delete(v);
			bKeys.delete(clave);
		}
		
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
			V v = bKeys.valueOf(k);
			bKeys.delete(k);
			bValues.delete(v);
		}
	}
	
	public void deleteByValue(V v) {
		if(bValues.isDefinedAt(v)){
			K k = bValues.valueOf(v);
			bValues.delete(v);
			bKeys.delete(k);
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
			throw new IllegalArgumentException();

		BiDictionary<K, V> biDict = new HashBiDictionary<>();
		
		for(Tuple2<K,V> par : dict.keysValues())
			biDict.insert(par._1(), par._2());

		return biDict;
	}

	private static <K,V extends Comparable<? super V>> boolean isInjective(Dictionary<K,V> dict) {
		Set<K> claves = new HashSet<>();
		Set<V> valores = new HashSet<>();

		for(K k : dict.keys())
			claves.insert(k);
		
		for(V v : dict.values())
			valores.insert(v);
		
		return claves.size() == valores.size();
	}
	
	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> dict) {
		BiDictionary<K, W> biDict = new HashBiDictionary<>();

		V v;
		W w;

		for(K k : bKeys.keys()){
			v = bKeys.valueOf(k);
			
			if(dict.isDefinedKeyAt(v)){
				w = dict.valueOf(v);
				biDict.insert(k,w);
			}
		}

		return biDict;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> dict) {
		Set<K> claves = new HashSet<>();
		Set<K> valores = new HashSet<>();

		for(K k : dict.keys())
			claves.insert(k);
		
		for(K k : dict.values())
			valores.insert(k);

		boolean sonIguales = true;

		for(K k : claves){
			if(!valores.isElem(k))
				sonIguales = false;
		}

		for(K k : valores){
			if(!claves.isElem(k))
				sonIguales = false;
		}

		return sonIguales && (claves.size() == valores.size());
	}
	
	// Solo alumnos con evaluación por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		List<K> res = new ArrayList<>();

		if(bd.isDefinedKeyAt(k)){
			res.append(k);
			K clave = bd.valueOf(k);

			while(!clave.equals(k)){
				res.append(clave);
				clave = bd.valueOf(clave);
			}
		}

		return res;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		List<List<K>> res = new ArrayList<>();
		Set<K> claves = new HashSet<>();
		List<K> ciclo;

		for(K k : bd.keys()){
			if(!claves.isElem(k)){
				ciclo = orbitOf(k, bd);
				res.append(ciclo);

				for(K elem : ciclo)
					claves.insert(elem);
			}
		}

		return res;
	}

    // =====================================
	
	
	@Override
	public String toString() {
		return "HashBiDictionary [bKeys=" + bKeys + ", bValues=" + bValues + "]";
	}
	
	
}
