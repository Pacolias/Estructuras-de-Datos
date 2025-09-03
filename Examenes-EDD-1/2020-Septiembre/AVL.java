/**
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 */

import dataStructures.list.List;
import dataStructures.list.LinkedList;

import java.util.Iterator;


class Bin {
    private int remainingCapacity; // capacity left for this bin
    private List<Integer> weights; // weights of objects included in this bin

    public Bin(int initialCapacity) {
        this.remainingCapacity = initialCapacity;
        weights = new LinkedList();
    }

    // returns capacity left for this bin
    public int remainingCapacity() {
        return remainingCapacity;
    }

    // adds a new object to this bin
    public void addObject(int weight) {
        if(weight > remainingCapacity)
            throw new RuntimeException("El objeto no cabe en el cubo, es demasiado pesado");
        
        remainingCapacity -= weight;
        weights.append(weight);
    }

    // returns an iterable through weights of objects included in this bin
    public Iterable<Integer> objects() {
        return weights.iterator();
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        sb.append(remainingCapacity);
        sb.append(", ");
        sb.append(weights.toString());
        sb.append(")");
        return sb.toString();
    }
}

// Class for representing an AVL tree of bins
public class AVL {
    static private class Node {
        Bin bin; // Bin stored in this node
        int height; // height of this node in AVL tree
        int maxRemainingCapacity; // max capacity left among all bins in tree rooted at this node
        Node left, right; // left and right children of this node in AVL tree

        // recomputes height of this node
        void setHeight() {
            if(bin == null){
                height = 0;
            } else if(left == null && right == null){
                height = 1;
            } else if(left == null){
                height = 1 + right.height;
            } else if(right == null){
                height = 1 + left.height;
            } else{
                height = 1 + Math.max(left.height, right.height);
            }
        }

        // recomputes max capacity among bins in tree rooted at this node
        void setMaxRemainingCapacity() {
            if(bin == null){
                maxRemainingCapacity = 0;
            } else if(left == null && right == null){
                maxRemainingCapacity = bin.remainingCapacity();
            } else if(left == null){
                maxRemainingCapacity = Math.max(bin.remainingCapacity(), maxRemainingCapacity(right));
            } else if(right == null){
                maxRemainingCapacity = Math.max(bin.remainingCapacity(), maxRemainingCapacity(left));
            } else{
                maxRemainingCapacity = Math.max(bin.remainingCapacity(), maxRemainingCapacity(right), maxRemainingCapacity(left));
            }        
        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Node nodeLeft = new Node();
            nodeLeft.bin = this.bin;
            nodeLeft.right = right.left;
            nodeLeft.left = this.left;

            nodeLeft.setHeight();
            nodeLeft.setMaxRemainingCapacity();

            Node node = new Node();
            node.left = nodeLeft;
            node.right = this.right.right;
            node.bin = this.right.bin;

            node.setHeight();
            node.setMaxRemainingCapacity();

            return node;
        }
    }
    
    private static int maxRemainingCapacity(Node node) {
        return node == null ? 0 : node.maxRemainingCapacity;
    }

    private static int height(Node node) {
        return node == null ? 0 : node.height;
    }

    private Node root; // root of AVL tree

    public AVL() {
        root = null;
    }

    // adds a new bin at the end of right spine.
    private void addNewBin(Bin bin) {
        root = addNewBin(bin, root);
    }

    private Node addNewBinRec(Bin bin, Node root) {
        if(root == null){
            Node newNode = new Node();
            newNode.bin = bin;
            newNode.left = null;
            newNode.right = null;
            newNode.setHeight();
            newNode.setMaxRemainingCapacity();

            return newNode;

        } else{
            root.right = addNewBinRec(bin, root.right);
            root.setHeight();
            root.setMaxRemainingCapacity();

            if(height(root.right) > height(root.left) + 1)
                root = root.rotateLeft();
            
            return root;
        }
    }

    // adds an object to first suitable bin. Adds
    // a new bin if object cannot be inserted in any existing bin
    public void addFirst(int initialCapacity, int weight) {
        root = addFirstRec(initialCapacity, weight, root);
    }

    private Node addFirstRec(int initialCapacity, int weight, Node root) {
        if(root == null){
            Node newNode = new Node();
            newNode.bin = new Bin(initialCapacity);
            newNode.left = null;
            newNode.right = null;
            newNode.bin.addObject(weight);

            newNode.setHeight();
            newNode.setMaxRemainingCapacity();
            
            return newNode;

        } else if(root.maxRemainingCapacity < weight){
            Bin cubo = new Bin(initialCapacity);
            root = addNewBinRec(cubo, root);
        
        } else if(maxRemainingCapacity(root.left) >= weight){
            root.left = addFirstRec(initialCapacity, weight, root.left);
        
        } else if(root.bin.remainingCapacity() >= weight){
            root.bin.addObject(weight);
        
        } else{
            root.right = addFirstRec(initialCapacity, weight, root.right);
        }

        root.setHeight();
        root.setMaxRemainingCapacity();

        if(height(root.right) > height(root.left) + 1)
            root = root.rotateLeft();

        return root;
    }

    public void addAll(int initialCapacity, int[] weights) {
        for(int weight : weights)
            addFirst(initialCapacity, weight);
    }

    public List<Bin> toList() {
        return toListRec(root);
    }

    private List<Bin> toListRec(Node root) {
        List<Bin> resultado = new LinkedList<>();

        if(root == null){
            // Nada
        } else if(root.left == null && root.right == null){
            resultado.append(root.bin);
        
        } else{
            for(Bin cubo : toListRec(root.left))
                resultado.append(cubo);

            resultado.append(root.bin);

            for(Bin cubo : toListRec(root.right))
                resultado.append(cubo);
        }

        return resultado;
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        stringBuild(sb, root);
        sb.append(")");
        return sb.toString();
    }

    private static void stringBuild(StringBuilder sb, Node node) {
        if(node==null)
            sb.append("null");
        else {
            sb.append(node.getClass().getSimpleName());
            sb.append("(");
            sb.append(node.bin);
            sb.append(", ");
            sb.append(node.height);
            sb.append(", ");
            sb.append(node.maxRemainingCapacity);
            sb.append(", ");
            stringBuild(sb, node.left);
            sb.append(", ");
            stringBuild(sb, node.right);
            sb.append(")");
        }
    }
}

class LinearBinPacking {
    public static List<Bin> linearBinPacking(int initialCapacity, List<Integer> weights) {
        List<Bin> resultado = new LinkedList<>();

        for(Integer weight : weights)
            insertarPeso(initialCapacity, weight, resultado);
        
        return resultado;
    }

    private static void insertarPeso(int initialCapacity, Integer weight, List<Bin> lista) {
        
        Iterator<Bin> iterador = lista.iterator();
        boolean esInsertado = false;
        Bin cubo = null;

        while(!esInsertado && iterador.hasNext()){
            cubo = iterador.next();

            if(cubo.remainingCapacity() >= weight){
                cubo.addObject(weight);
                esInsertado = true;
            }
        }

        if(!esInsertado){
            cubo = new Bin(initialCapacity);
            cubo.addObject(weight);
            lista.append(cubo);
        }
    }

	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        List<Integer> listaAux = new LinkedList<>();
        Iterator<Bin> itBins = bins.iterator();
        
        while(itBins.hasNext()){
            Iterable<Integer> pesos = itBins.next().objects();
            Iterator<Integer> itPesos = pesos.iterator();

            while(itPesos.hasNext())
                listaAux.append(itPesos.next());
        }

        return listaAux.iterator();
	}
}
