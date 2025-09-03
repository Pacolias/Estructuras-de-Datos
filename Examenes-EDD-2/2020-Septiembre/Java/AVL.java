/**
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería Informática
 */

import dataStructures.list.LinkedList;
import dataStructures.list.List;


class Bin {
    private int remainingCapacity; // capacity left for this bin
    private List<Integer> weights; // weights of objects included in this bin

    public Bin(int initialCapacity) {
        remainingCapacity = initialCapacity;
        weights = new LinkedList<>();
    }

    // returns capacity left for this bin
    public int remainingCapacity() {
        return remainingCapacity;
    }

    // adds a new object to this bin
    public void addObject(int weight) {
        if(weight > remainingCapacity)
            throw new IllegalArgumentException("Object too heavy for the given bin");
        
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
            height = 1 + Math.max(height(left), height(right));
        }

        // recomputes max capacity among bins in tree rooted at this node
        void setMaxRemainingCapacity() {
            maxRemainingCapacity = Math.max(Math.max(bin.remainingCapacity(), maxRemainingCapacity(left)), maxRemainingCapacity(right));
        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Node newRoot = right;
            right = newRoot.left;
            newRoot.left = this;

            // Actualizar alturas y maxRemainingCapacity
            this.setHeight();
            this.setMaxRemainingCapacity();
            newRoot.setHeight();
            newRoot.setMaxRemainingCapacity();

            return newRoot;
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
        root = addNewBinRec(bin, root);
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

            if(height(root.right) > height(root.left) + 1)
                root = root.rotateLeft();
            
            root.setHeight();
            root.setMaxRemainingCapacity();

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
            newNode.bin.addObject(weight);
            newNode.left = null;
            newNode.right = null;
            newNode.setHeight();
            newNode.setMaxRemainingCapacity();
            return newNode;
        
        } else{
            if(root.left != null && root.left.maxRemainingCapacity >= weight){
                root.left = addFirstRec(initialCapacity, weight, root.left);

            }else if(root.bin != null && root.bin.remainingCapacity() >= weight){
                root.bin.addObject(weight);

            }else if(root.right != null && root.right.maxRemainingCapacity >= weight){
                root.right = addFirstRec(initialCapacity, weight, root.right);

            }else{ 
                Bin bin = new Bin(initialCapacity);
                bin.addObject(weight);
                root.right = addNewBinRec(bin, root.right);
            }

            return root;
        }
    }

    public void addAll(int initialCapacity, int[] weights) {
        for(int w : weights)
            addFirst(initialCapacity, w);
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
        List<Bin> res = new LinkedList<>();

        for(Integer w : weights)
            linearBinPackingRec(initialCapacity, w, res);

        return res;
    }

    public static void linearBinPackingRec(int initialCapacity, Integer weight, List<Bin> res){
        boolean isSettedUp = false;
        int i = 0;

        while(i < res.size() && !isSettedUp){
            if(res.get(i).remainingCapacity() >= weight){
                isSettedUp = true;
                res.get(i).addObject(weight);
            }
            i++;
        }

        if(!isSettedUp){
            Bin newBin = new Bin(initialCapacity);
            newBin.addObject(weight);
            res.append(newBin);
        }
    }

	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        List<Integer> aux = new LinkedList<>();

        for(Bin bin : bins){
            for(Integer weight : bin.objects())
                aux.append(weight);
        }

        return aux.iterator();
	}
}
