/******************************************************************************
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 * Data Structures. E.T.S.I. de Informática UMA.
******************************************************************************/

package dataStructures.vector;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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
            return elem;
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if(sz == 1)
                return new Unif<>(x);
            else if(i < sz/2)
                return new Node<>(set(sz/2, i, x), new Unif<>(elem)).simplify();
            else 
                return new Node<>(new Unif<>(elem), set(sz/2, i - sz/2, x)).simplify();
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
            return i < sz/2 ? left.get(sz/2, i) : right.get(sz/2, i - sz/2);
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if(i < sz/2)
                left = left.set(sz/2, i, x);
            else 
                right = right.set(sz/2, i - sz/2, x);

            return this;
        }

        protected Tree<T> simplify() {
            if(left instanceof Unif<?> && right instanceof Unif<?>){
                Unif<T> leftLeaf = (Unif<T>) left;
                Unif<T> rightLeaf = (Unif<T>) right;

                return leftLeaf.elem.equals(rightLeaf.elem) ? new Unif<>(leftLeaf.elem) : this;
            }

            return this;
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
        if(n < 0)
            throw new VectorException("non positive integer");

        size = (int) Math.pow(2, n);
        root = new Unif<>(elem);
    }

    public int size() {
        return size;
    }

    public T get(int i) {
        return root.get(size, i);
    }

    public void set(int i, T x) {
        root = root.set(size, i, x);
    }

    @Override
    public Iterator<T> iterator() {
        List<T> aux = new ArrayList<>();

        for(int i = 0; i < size; i++)
            aux.add(get(i));

        return aux.iterator();
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }

    public SparseVector(SparseVector<T> sp){
        size = sp.size();
        root = new Unif<>(sp.get(0));

        for(int i = 0; i < size; i++){
            this.set(i, sp.get(i));
        }
    }

    public int depthOf(int i){
        return depthOfRec(i, 0, size, root);
    }

    private int depthOfRec(int i, int depth, int size, Tree<T> root){
        if(root instanceof Unif<?>)
            return depth;

        return i < size/2 ? depthOfRec(i, depth + 1, size/2, root.left) : depthOfRec(i - size/2, depth + 1, size/2, root.right);
    }
}
