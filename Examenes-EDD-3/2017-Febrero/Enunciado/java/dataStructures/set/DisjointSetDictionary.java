/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de febrero de 2018.
 *
 * Apellidos, Nombre:
 * Titulacion, Grupo:
 */

package dataStructures.set;

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.ArrayList;
import dataStructures.list.List;

public class DisjointSetDictionary<T extends Comparable<? super T>> implements DisjointSet<T> {

    private Dictionary<T, T> dic;

    /**
     * Inicializa las estructuras necesarias.
     */
    public DisjointSetDictionary() {
        dic = new AVLDictionary<>();
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
        if (!isElem(elem)) {
            dic.insert(elem, elem);
        }
    }

    /**
     * Devuelve el elemento canonico (la raiz) de la clase de equivalencia la
     * que pertenece {@code elem}. Si {@code elem} no pertenece al conjunto
     * devuelve {@code null}.
     */
    private T root(T elem) {
        if (elem == null || !isElem(elem))
            return null;
        else {
            while (dic.valueOf(elem) != null && !dic.valueOf(elem).equals(elem))
                elem = dic.valueOf(elem);
        }
        return elem;
    }

    /**
     * Devuelve {@code true} si {@code elem} es el elemento canonico (la raiz)
     * de la clase de equivalencia a la que pertenece.
     */
    private boolean isRoot(T elem) {
        if (elem != null && isElem(elem))
            return (elem.equals(dic.valueOf(elem)));
        return false;
    }

    /**
     * Devuelve {@code true} si {@code elem1} y {@code elem2} estan en la misma
     * clase de equivalencia.
     */
    @Override
    public boolean areConnected(T elem1, T elem2) {
        if (elem1 != null && elem2 != null && isElem(elem1) && isElem(elem2))
            return (root(elem1).equals(root(elem2)));
        return false;
    }

    /**
     * Devuelve una lista con los elementos pertenecientes a la clase de
     * equivalencia en la que esta {@code elem}. Si {@code elem} no pertenece al
     * conjunto devuelve la lista vacia.
     */
    @Override
    public List<T> kind(T elem) {
        if (elem == null || !isElem(elem))
            return null;
        List<T> res = new ArrayList<>();
        for (T x : dic.keys()) {
            if (areConnected(elem,x)){
                res.append(x);
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
        if (!isElem(elem1) || !isElem(elem2)) {
            throw new IllegalArgumentException("Alguno de los elementos no está en el conjunto");
        } else {
            if (root(elem1).compareTo(root(elem2)) > 0) {
                dic.insert(root(elem1), root(elem2));
            } else {
                dic.insert(root(elem2), root(elem1));
            }
        }
    }

/*
    @Override
    public void union(T elem1, T elem2) {
        if (!dic.isDefinedAt(elem1) || !dic.isDefinedAt(elem2)) {
            throw new IllegalArgumentException("Alguno de los elementos no está en el conjunto");
        } else {
            if (root(elem1).compareTo(root(elem2)) > 0) {
                dic.insert(root(elem1), root(elem2));
            } else {
                dic.insert(root(elem2), root(elem1));
            }
        }
    }
*/
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
        // TODO
    }

    /**
     * Devuelve una lista que contiene las clases de equivalencia del conjunto
     * como listas.
     */
    @Override
    public List<List<T>> kinds() {
        // TODO
        return null;
    }

    /**
     * Devuelve una representacion del conjunto como una {@code String}.
     */
    @Override
    public String toString() {
        return "DisjointSetDictionary(" + dic.toString() + ")";
    }
}
