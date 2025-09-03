public class BinarySearchTree {
    private static class Node {
        int value;
        Node left, right;

        Node(Node l, Node r, int val) {
            left = l;
            right = r;
            value = val;
        }
    }

    private Node root;

    public BinarySearchTree() {
        root = null;
    }

    public void insert(int x) {
        root = insertRec(root, x);
    }

    private Node insertRec(Node node, int x) {
        // implementación habitual del insert
        return node;
    }

    public static void generalSum(BinarySearchTree tree) {
        generalSum(tree.root, 0);
    }

    private static int generalSum(Node node, int acc) {
        // lógica recursiva aquí
        return acc;
    }
}
