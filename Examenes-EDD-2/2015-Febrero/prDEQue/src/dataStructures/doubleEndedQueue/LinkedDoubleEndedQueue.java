/**
 * Estructuras de Datos. Grado en Informática, IS e IC. UMA.
 * Examen de Febrero 2015.
 *
 * Implementación del TAD Deque
 *
 * Apellidos:
 * Nombre:
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
        return first == null && last == null;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addFirst(T x) {
        if(isEmpty()){//Cola vacia
            Node<T> node = new Node(x, null, null);
            first = node;
            last = node;
        
        } else{//Cola con mas de un elemento
            Node<T> node = new Node(x, first, null);
            first.prev = node;
            first = node;
        }
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public void addLast(T x) {
        if(isEmpty()){//Cola vacia
            Node<T> node = new Node(x, null, null);
            first = node;
            last = node;
        
        } else{//Cola con mas de un elemento
            Node<T> node = new Node(x, null, last);
            last.next = node;
            last = node;
        }
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T first() {
        return first == null ? null : first.elem;
    }

    /**
     * Complexity: O(1)
     */
    @Override
    public T last() {
        return last == null ? null : last.elem;
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteFirst() {
        if(first == last){//Cola vacia o con un unico elemento
            first = null;
            last = null;
        
        } else{//Cola con mas de un elemento
            first.next.prev = null;
            first = first.next;
        }
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteLast() {
        if(first == last){//Cola vacia o con un unico elemento
            first = null;
            last = null;
        
        } else{//Cola con mas de un elemento
            last.prev.next = null;
            last = last.prev;
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
