/******************************************************************************
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. de Informática UMA.
******************************************************************************/

package dataStructures.vector;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

public class SparseVector<T> implements Iterable<T> {

    protected interface Tree<T> {

        T get(int sz, int i);

        Tree<T> set(int sz, int i, T x);
    }

    // Unif Implementation

    protected static class Unif<T> implements Tree<T> {

        private T elem;

        public Unif(T e) {
            elem = e;
        }

        @Override
        public T get(int sz, int i) {
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
        }
        
        @Override
        public String toString() {
            return "Unif(" + elem + ")";
        }
    }

    // Node Implementation

    protected static class Node<T> implements Tree<T> {

        private Tree<T> left, right;

        public Node(Tree<T> l, Tree<T> r) {
            left = l;
            right = r;
        }

        @Override
        public T get(int sz, int i) {
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
        }

        protected Tree<T> simplify() {
        }

        @Override
        public String toString() {
            return "Node(" + left + ", " + right + ")";
        }
    }

    // SparseVector Implementation

    private int size;
    private Tree<T> root;

    public SparseVector(int n, T elem) {
    }

    public int size() {
    }

    public T get(int i) {
    }

    public void set(int i, T x) {
    }

    @Override
    public Iterator<T> iterator() {
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
}
