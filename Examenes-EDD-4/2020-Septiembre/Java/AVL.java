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
        remainingCapacity -= weight;
        weights.append(weight);
    }

    // returns an iterable through weights of objects included in this bin
    public Iterable<Integer> objects() {
        return weights;
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
            maxRemainingCapacity = Math.max(Math.max(maxRemainingCapacity(left), maxRemainingCapacity(right)), bin.remainingCapacity());
        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Node leftChild = new Node();
            leftChild.bin = bin;
            leftChild.left = left;
            leftChild.right = right.left;
            leftChild.height = 0;
            leftChild.maxRemainingCapacity = 0;
            
            leftChild.setHeight();
            leftChild.setMaxRemainingCapacity();

            Node newNode = new Node();

            newNode.bin = right.bin;
            newNode.left = leftChild;
            newNode.right = right.right;

            newNode.setHeight();
            newNode.setMaxRemainingCapacity();

            return newNode;
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
        Node newNode = new Node();

        if(root == null){
            newNode.bin = bin;
            newNode.setHeight();
            newNode.setMaxRemainingCapacity();
        
        } else{
            newNode.bin = root.bin;
            newNode.left = root.left;
            newNode.right = root.right;
            newNode.setHeight();
            newNode.setMaxRemainingCapacity();

            newNode.right = addNewBinRec(bin, newNode.right);

            if(height(newNode.right) > height(newNode.left) + 1)
                newNode = newNode.rotateLeft();
        }

        return newNode;
    }

    // adds an object to first suitable bin. Adds
    // a new bin if object cannot be inserted in any existing bin
    public void addFirst(int initialCapacity, int weight) {
        root = addFirstRec(initialCapacity, weight, root);
    }

    private Node addFirstRec(int initialCapacity, int weight, Node root) {
        if(root == null || maxRemainingCapacity(root) < weight){
            Bin bin = new Bin(initialCapacity);
            bin.addObject(weight);

            root = addNewBinRec(bin, root);

        } else if(maxRemainingCapacity(root.left) >= weight){
            root.left = addFirstRec(initialCapacity, weight, root.left);

        } else if(root.bin.remainingCapacity() >= weight){
            root.bin.addObject(weight);
        
        } else{
            root.right = addFirstRec(initialCapacity, weight, root.right);
        }

        root.setMaxRemainingCapacity();

        return root;
    }

    public void addAll(int initialCapacity, int[] weights) {
        for(int i = 0; i < weights.length; i++)
            addFirst(initialCapacity, weights[i]);
    }

    public List<Bin> toList() {
        return toListRec(root);
    }

    private List<Bin> toListRec(Node root) {
        List<Bin> res = new LinkedList<>();

        if(root != null){
            List<Bin> leftList = toListRec(root.left);
            List<Bin> rightList = toListRec(root.right);

            for(Bin bin : leftList)
                res.append(bin);

            res.append(root.bin);

            for(Bin bin : rightList)
                res.append(bin);
        }

        return res;
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

        for(Integer weight : weights)
            linearBinPackingRec(initialCapacity, weight, res);

        return res;
    }

    private static void linearBinPackingRec(int initialCapacity, int weight, List<Bin> res){
        if(res.isEmpty()){
            Bin bin = new Bin(initialCapacity);
            bin.addObject(weight);
            res.append(bin);

        } else{

            boolean pesoAniadido = false;
            int i = 0;

            while(i < res.size() && !pesoAniadido){
                if(res.get(i).remainingCapacity() >= weight){
                    res.get(i).addObject(weight);
                    pesoAniadido = true;
                }
                i++;
            }

            if(!pesoAniadido){
                Bin bin = new Bin(initialCapacity);
                bin.addObject(weight);
                res.append(bin);
            }
        }
    }

	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        List<Integer> res = new LinkedList<>();

        for(Bin bin : bins){
            for(Integer weight : bin.objects())
                res.append(weight);
        }

        return res;
	}
}
