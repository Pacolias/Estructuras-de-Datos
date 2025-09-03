/**
 * Estructuras de Datos. Grado en Informática, IS e IC. UMA.
 * Examen de Febrero 2015.
 *
 * Implementación del TAD Deque
 *
 * Apellidos: Molina Cuenca
 * Nombre: Paco
 * Grado en Ingeniería ...
 * Grupo:
 * Número de PC:
 */

package dataStructures.doubleEndedQueue;

public class LinkedDoubleEndedQueue<T> implements DoubleEndedQueue<T> {

    private static class Node<E> {
        private E elem;
        private Node<E> next;
        private Node<E> prev;

        public Node(E x, Node<E> nxt, Node<E> prv) {
            elem = x;
            next = nxt;
            prev = prv;
        }
    }

    private Node<T> first, last;

    /**
     *  Invariants:
     *  if queue is empty then both first and last are null
     *  if queue is non-empty:
     *      * first is a reference to first node and last is ref to last node
     *      * first.prev is null
     *      * last.next is null
     *      * rest of nodes are doubly linked
     */

    /**
     * Complexity: O(1)
     */
    public LinkedDoubleEndedQueue() {
        first = null;
        last = null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public boolean isEmpty() {
        return first == null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addFirst(T x) {
        Node<T> nuevoNodo = new Node<>(x, first, null);

        if(isEmpty()){
            last = nuevoNodo;
        
        } else{
            if(first.next != null) 
                first.next.prev = nuevoNodo;
        }
        first = nuevoNodo;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addLast(T x) {
        Node<T> nuevoNodo = new Node<>(x, null, last);

        if(isEmpty()){
            first = nuevoNodo;
        
        } else{
            if(last.prev != null) 
                last.prev.next = nuevoNodo;
        }
        last = nuevoNodo;
    }

    /**
     * Complexity: o(1)
     */
    @Override
    public T first() {
        if(isEmpty())
            throw new EmptyDoubleEndedQueueException("Operacion first en cola vacia");
        
        return first.elem;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T last() {
        if(isEmpty())
            throw new EmptyDoubleEndedQueueException("Operacion last en cola vacia");
        
        return last.elem;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void deleteFirst() {
        if(isEmpty())
            throw new EmptyDoubleEndedQueueException("Operacion deleteFirst en cola vacia");
        
        Node<T> siguienteNodo = first.next;
        
        if(siguienteNodo == null){
            first = null;
            last = null;
        
        } else {
            siguienteNodo.prev = null;
            first = siguienteNodo;
        }
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void deleteLast() {
        if(isEmpty())
            throw new EmptyDoubleEndedQueueException("Operacion deleteLast en cola vacia");
        
        Node<T> nodoPrevio = last.prev;
        
        if(nodoPrevio == null){
            first = null;
            last = null;
        
        } else {
            nodoPrevio.next = null;
            last = nodoPrevio;
        }
    }

    /**
     * Returns representation of queue as a String.
     */
    @Override
    public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
        String s = className+"(";
        for (Node<T> node = first; node != null; node = node.next)
            s += node.elem + (node.next != null ? "," : "");
        s += ")";
        return s;
    }
}
