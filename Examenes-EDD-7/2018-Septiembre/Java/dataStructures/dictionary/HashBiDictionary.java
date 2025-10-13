package dataStructures.dictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.set.HashSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de septiembre de 2018.
 *
 * Apellidos, Nombre: Molina Cuenca, Paco
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
		if(bKeys.isDefinedAt(k))
			bValues.delete(bKeys.valueOf(k));
		
		if(bValues.isDefinedAt(v))
			bKeys.delete(bValues.valueOf(v));

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
			bValues.delete(bKeys.valueOf(k));
			bKeys.delete(k);
		}
	}
	
	public void deleteByValue(V v) {
		if(bValues.isDefinedAt(v)){
			bKeys.delete(bValues.valueOf(v));
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
			throw new IllegalArgumentException("toBidictionary on non injective dictionary");

		BiDictionary<K, V> res = new HashBiDictionary<>();

		for(Tuple2<K, V> par : dict.keysValues())
			res.insert(par._1(), par._2());

		return res;
	}

	private static <K,V extends Comparable<? super V>> boolean isInjective(Dictionary<K,V> dict){
		Set<K> keys = new HashSet<>();
		Set<V> values = new HashSet<>();

		for(K k : dict.keys())
			keys.insert(k);

		for(V v : dict.values())
			values.insert(v);

		return keys.size() == values.size();
	}
	
	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
		BiDictionary<K, W> res = new HashBiDictionary<>();

		for(K k : keys()){
			if(bdic.isDefinedKeyAt(valueOf(k)))
				res.insert(k, bdic.valueOf(valueOf(k)));
		}

		return res;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
		Set<K> keys = new HashSet<>();
		Set<K> values = new HashSet<>();

		for(K k : bd.keys())
			keys.insert(k);

		for(K k : bd.values())
			values.insert(k);

		boolean sonIguales = true; 

		for(K k : bd.keys()){
			if(!values.isElem(k))
				sonIguales = false;
		}
	
		for(K k : bd.values()){
			if(!keys.isElem(k))
				sonIguales = false;
		}

		return sonIguales;
	}
	
	// Solo alumnos con evaluación por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		if(!isPermutation(bd))
			throw new IllegalArgumentException();

		List<K> res = new LinkedList<>();
		res.append(k);
		K val = bd.valueOf(k);

		while(!val.equals(k)){
			res.append(val);
			val = bd.valueOf(val);
		}

		return res;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		if(!isPermutation(bd))
			throw new IllegalArgumentException();

		List<List<K>> res = new LinkedList<>();
		List<K> ciclo;
		Set<K> keys = new HashSet<>();

		for(K k : bd.keys()){
			if(!keys.isElem(k)){
				ciclo = orbitOf(k, bd);
				res.append(ciclo);

				for(K clave : ciclo)
					keys.insert(clave);
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
