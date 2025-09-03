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
            return elem;
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if(sz == 1)
                return new Unif(x);
            else if(i <= sz/2 - 1)
                return new Node<>(set(sz/2, i, x), new Unif<>(elem));
            else 
                return new Node<>(new Unif<>(elem), set(sz/2, i - sz/2, x));
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
            return i <= sz/2 - 1 ? left.get(sz/2, i) : right.get(sz/2, i - sz/2);
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            return i <= sz/2 - 1 ?  new Node<>(left.set(sz/2, i), right).simplify() : new Node<>(left, right.set(sz/2, i - sz/2)).simplify();
        }

        protected Tree<T> simplify() {
            if(left instanceof Unif<T> unifLeft && right instanceof Unif<T> unifRight){
                if(unifLeft.elem.equals(unifRight.elem))
                    return new Unif<>(unifLeft.elem);
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
            throw new VectorException("El tamaño pasado como parametro es negativo");
        
        size = (int) Math.pow(2, n);
        root = new Unif<>(elem);
    }

    public int size() {
        return size;
    }

    public T get(int i) {
        if(i < 0 || i >= size)
            throw new VectorException("Indice fuera de rango");

        return root.get(size, i);
    }

    public void set(int i, T x) {
        if(i < 0 || i >= size)
            throw new VectorException("Indice fuera de rango");

        root = root.set(size, i, x);
    }

    @Override
    public Iterator<T> iterator() {
        List<T> elementos = new ArrayList<>();

        for(int i = 0; i < size; i++)
            elementos.add(get(i));
        
        return elementos.iterator();
    }

    public SparseVector(SparseVector<T> sp){
        size = sp.size();
        root = cloneTree(sp.root);
    }

    private Tree<T> cloneTree(Tree<T> raiz){
        if(raiz instanceof Unif<T> unif)
            return new Unif<>(unif.elem);
        else if(raiz instanceof Node<T> node)
            return new Node<>(cloneTree(node.left), cloneTree(node.right));
    }

    public int depthOf(int i){
        if(i < 0 || i >= size)
            throw new VectorException("Indice fuera de rango");
        
        return profundidadRec(root, i, 0);
    }

    private int profundidadRec(Tree<T> root, int i, int acc){
        if(root instanceof Unif<T> unif){
            return acc;
        } else if (root instanceof Node<T> node){
            return i < size/2 ? profundidadRec(node.left, i, acc + 1) : profundidadRec(node.right, i - size/2, acc + 1);
        }
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
}
