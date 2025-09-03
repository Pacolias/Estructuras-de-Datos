/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de febrero de 2018.
 * Apellidos, Nombre: Paco Molina Cuenca
 * Titulacion, Grupo: Doble Grado Matemáticas e Ingeniería Informática.
 */

package dataStructures.set;

import java.util.Iterator;

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.ArrayList;
import dataStructures.list.LinkedList;
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
        if(!isElem(elem))
            dic.insert(elem, elem);
    }

    /**
     * Devuelve el elemento canonico (la raiz) de la clase de equivalencia la
     * que pertenece {@code elem}. Si {@code elem} no pertenece al conjunto
     * devuelve {@code null}.
     */
    private T root(T elem) {
        return !isElem(elem) ? null : rootRec(elem);
    }

    private T rootRec(T elem){
        return dic.valueOf(elem).equals(elem) ? elem : rootRec(dic.valueOf(elem));
    }

    /**
     * Devuelve {@code true} si {@code elem} es el elemento canonico (la raiz)
     * de la clase de equivalencia a la que pertenece.
     */
    private boolean isRoot(T elem) {
        return isElem(elem) ? elem.equals(dic.valueOf(elem)) : false;
    }

    /**
     * Devuelve {@code true} si {@code elem1} y {@code elem2} estan en la misma
     * clase de equivalencia.
     */
    @Override
    public boolean areConnected(T elem1, T elem2) {
        return isElem(elem1) && isElem(elem2) ? root(elem1).equals(root(elem2)) : false;
    }

    /**
     * Devuelve una lista con los elementos pertenecientes a la clase de
     * equivalencia en la que esta {@code elem}. Si {@code elem} no pertenece al
     * conjunto devuelve la lista vacia.
     */
    @Override
    public List<T> kind(T elem) {
        List<T> claseEquivalencia = new LinkedList<>();
        
        if(isElem(elem)){
            for(T elemento : dic.keys()){
                if(areConnected(elem, elemento))
                    claseEquivalencia.append(elemento);
            }
        }

        return claseEquivalencia;
    }

    /**
     * Une las clases de equivalencias de {@code elem1} y {@code elem2}. Si
     * alguno de los dos argumentos no esta en el conjunto lanzara una excepcion
     * {@code IllegalArgumenException}.
     */
    @Override
    public void union(T elem1, T elem2) {
        if(!isElem(elem1) || !isElem(elem2))
            throw new IllegalArgumentException("Alguno de los elementos proporcionados como parametros no forma parte del conjunto");
        
        T raiz1 = root(elem1);
        T raiz2 = root(elem2);

        int comparacion = raiz1.compareTo(raiz2);

        if(comparacion < 0){
            dic.delete(raiz2);
            dic.insert(raiz2, raiz1);

        } else if(comparacion > 0){
            dic.delete(raiz1);
            dic.insert(raiz1, raiz2);
        }
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
        List<T> elementos = new LinkedList<>();
        T raiz = null;

        for(T elemento : dic.keys())
            elementos.append(elemento);
        
        for(T elemento : elementos){
            raiz = root(elemento);
            dic.delete(elemento);
            dic.insert(elemento, raiz);
        }
    }

    /**
     * Devuelve una lista que contiene las clases de equivalencia del conjunto
     * como listas.
     */
    @Override
    public List<List<T>> kinds() {
        List<List<T>> clasesEquivalencia = new LinkedList<>();

        for(T elemento : dic.keys()){
            if(!elementoYaAniadido(elemento, clasesEquivalencia))
                clasesEquivalencia.append(kind(elemento));
        }

        return clasesEquivalencia;
    }

    private boolean elementoYaAniadido(T elemento, List<List<T>> clases){
        boolean esAniadido = false;
        Iterator<List<T>> itClases = clases.iterator();
        List<T> clase = null;
        Iterator<T> itElems = null;

        while(!esAniadido && itClases.hasNext()){
            clase = itClases.next();
            itElems = clases.iterator();

            while(!esAniadido && itElems.hasNext()){
                if(elemento.equals(itElems.next()))
                    esAniadido = true;
            }
        }

        return esAniadido;
    }

    /**
     * Devuelve una representacion del conjunto como una {@code String}.
     */
    @Override
    public String toString() {
        return "DisjointSetDictionary(" + dic.toString() + ")";
    }
}
