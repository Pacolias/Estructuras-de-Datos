/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de febrero de 2018.
 * Apellidos, Nombre: Paco Molina Cuenca
 * Titulacion, Grupo: Doble Grado Matemáticas e Ingeniería Informática.
 */

package dataStructures.set;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.tuple.Tuple2;

public class DisjointSetDictionary<T extends Comparable<? super T>> implements DisjointSet<T> {

    private Dictionary<T, T> dic;

    /**
     * Inicializa las estructuras necesarias.
     */
    public DisjointSetDictionary() {
        dic = new HashDictionary<>();
    }

    /**
     * Devuelve {@code true} si el conjunto no contiene elementos.
     */
    @Override
    public boolean isEmpty() {
        return dic.isEmpty();
    }

    /**
     * Devuelve {@code true} si {@code elem} es un elemento del conjunto.
     */
    @Override
    public boolean isElem(T elem) {
        return dic.isDefinedAt(elem);
    }

    /**
     * Devuelve el numero total de elementos del conjunto.
     */

    @Override
    public int numElements() {
        return dic.size();
    }

    /**
     * Agrega {@code elem} al conjunto. Si {@code elem} no pertenece al
     * conjunto, crea una nueva clase de equivalencia con {@code elem}. Si
     * {@code elem} pertencece al conjunto no hace nada.
     */
    @Override
    public void add(T elem) {
        if(!dic.isDefinedAt(elem))
            dic.insert(elem, elem);
    }

    /**
     * Devuelve el elemento canonico (la raiz) de la clase de equivalencia la
     * que pertenece {@code elem}. Si {@code elem} no pertenece al conjunto
     * devuelve {@code null}.
     */
    private T root(T elem) {
        if(!dic.isDefinedAt(elem))
            return null;
        else 
            return elem.equals(dic.valueOf(elem)) ? elem : root(dic.valueOf(elem));
    }

    /**
     * Devuelve {@code true} si {@code elem} es el elemento canonico (la raiz)
     * de la clase de equivalencia a la que pertenece.
     */
    private boolean isRoot(T elem) {
        return elem.equals(root(elem));
    }

    /**
     * Devuelve {@code true} si {@code elem1} y {@code elem2} estan en la misma
     * clase de equivalencia.
     */
    @Override
    public boolean areConnected(T elem1, T elem2) {
        return root(elem1).equals(root(elem2));
    }

    /**
     * Devuelve una lista con los elementos pertenecientes a la clase de
     * equivalencia en la que esta {@code elem}. Si {@code elem} no pertenece al
     * conjunto devuelve la lista vacia.
     */
    @Override
    public List<T> kind(T elem) {
        List<T> res = new LinkedList<>();

        if(isElem(elem)){
            for(T e : dic.keys()){
                if(areConnected(e, elem))
                    res.append(e);
            }
        }

        return res;
    }

    /**
     * Une las clases de equivalencias de {@code elem1} y {@code elem2}. Si
     * alguno de los dos argumentos no esta en el conjunto lanzara una excepcion
     * {@code IllegalArgumenException}.
     */
    @Override
    public void union(T elem1, T elem2) {
        if(!(isElem(elem1) && isElem(elem2)))
            throw new IllegalArgumentException("union on at least one element which is not part of the DS");

        T root1 = root(elem1);
        T root2 = root(elem2);

        if(root1.compareTo(root2) < 0)
            dic.insert(root2, root1);
        else if(root1.compareTo(root2) > 0)
            dic.insert(root1, root2);
    }

    // ====================================================
    // A partir de aqui solo para alumnos a tiempo parcial
    // que no sigan el proceso de evaluacion continua.
    // ====================================================

    /**
     * Aplana la estructura de manera que todos los elementos se asocien
     * directamente con su representante canonico.
     */
    @Override
    public void flatten() {
        Set<Tuple2<T, T>> set = new HashSet<>();

        for(T elem : dic.keys())
            set.insert(new Tuple2<>(elem, root(elem)));

        for(Tuple2<T,T> par : set){
            dic.delete(par._1());
            dic.insert(par._1(), par._2()); 
        }
    }

    /**
     * Devuelve una lista que contiene las clases de equivalencia del conjunto
     * como listas.
     */
    @Override
    public List<List<T>> kinds() {
        Set<T> set = new HashSet<>();
        List<List<T>> res = new LinkedList<>();
        List<T> clase;

        for(T elem : dic.keys()){
            if(!set.isElem(elem)){
                clase = kind(elem);
                res.append(clase);

                for(T e : clase)
                    set.insert(e);
            }
        }

        return res;
    }

    /**
     * Devuelve una representacion del conjunto como una {@code String}.
     */
    @Override
    public String toString() {
        return "DisjointSetDictionary(" + dic.toString() + ")";
    }
}
