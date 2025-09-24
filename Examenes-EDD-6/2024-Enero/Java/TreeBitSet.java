//Student's name: Paco Molina Cuenca
//Student's group: 
//Identity number (DNI if Spanish/passport if Erasmus):

import dataStructures.list.LinkedList;
import dataStructures.list.List;

public class TreeBitSet {
  private static final int BITS_PER_LEAF = LongBits.BITS_PER_LONG;
  
  private interface Tree {
    long size();
    boolean contains(long element, long capacity);
    void add(long element, long capacity);
    List<Long> toList(long capacity);
    Tree cloneTree();
  }

  private final Tree root;
  private final long capacity;

  // returns true if capacity is 64 * 2^n for some n >= 0
  private static boolean isValidCapacity(long capacity) {
    if (capacity <= 0) {
      return false;
    }
    while (capacity > BITS_PER_LEAF) {
      if (capacity % 2 != 0) {
        return false;
      }
      capacity /= 2;
    }
    return capacity == BITS_PER_LEAF;
  }

//-------------------------------------------------------------------
// DO NOT MODIFY ANY CODE ABOVE THIS LINE
//-------------------------------------------------------------------

  private static class Leaf implements Tree {
    private long bitset;
    
    public Leaf(long bitset){
      this.bitset = bitset;
    }

    public long size(){
      return LongBits.countOnes(bitset);
    }

    public boolean contains(long element, long capacity){
      return LongBits.getBit(bitset, (int) element);
    }

    public void add(long element, long capacity){
      bitset = LongBits.setBit(bitset, (int) element);
    }

    public List<Long> toList(long capacity){
      return LongBits.toList(bitset);
    }

    public Tree cloneTree(){
      return new Leaf(bitset);
    }
  }

  private static class Node implements Tree{
    private final Tree left, right;
    
    public Node(Tree left, Tree right){
      this.left = left;
      this.right = right;
    }

    public long size(){
        return left.size() + right.size();
    }

    public boolean contains(long element, long capacity){
      return element < capacity/2 ? left.contains(element, capacity/2) : right.contains(element - capacity/2, capacity/2);
    }

    public void add(long element, long capacity){
      if(element < capacity/2)
        left.add(element, capacity/2);
      else 
        right.add(element - capacity/2, capacity/2);
    }

    public List<Long> toList(long capacity){
      List<Long> res = new LinkedList<>();
      
      for(Long bit : left.toList(capacity/2))
        res.append(bit);

      for(Long bit : right.toList(capacity/2))
        res.append(bit + capacity/2);

      return res;
    }

    public Tree cloneTree(){
      return new Node(left.cloneTree(), right.cloneTree());
    }
  }


  // * Exercise 1 * -
  private static Tree makeTree(long capacity){
    return capacity <= BITS_PER_LEAF ? new Leaf(0) : new Node(makeTree(capacity/2), makeTree(capacity/2));
  }

  // * Exercise 2 * -
  public TreeBitSet(long capacity){
    if(!isValidCapacity(capacity))
      throw new IllegalArgumentException("not valid capacity");

    this.capacity = capacity;
    root = makeTree(capacity);
  }

  // * Exercise 3 * -
  public long capacity(){
    return capacity;
  }

  // * Exercise 4 * -
  private boolean outOfRange(long element){
    return element < 0 || element >= capacity;
  }

  // * Exercise 5 * -
  public long size(){
    return root.size();
  }

  // * Exercise 6 * -
  public boolean isEmpty(){
    return size() == 0;
  }

  // * Exercise 7 * -
  public boolean contains(long element){
    return root.contains(element, capacity);
  }

  // * Exercise 8 * -
  public void add(long element){
    if(outOfRange(element))
      throw new IllegalArgumentException("index out of bounds");

    root.add(element, capacity);
  }

  // * Exercise 9 * -
  public List<Long> toList(){
    return root.toList(capacity);
  }


  //-------------------------------------------------------------------
  // Only for students without continuous assessment
  //-------------------------------------------------------------------
  
  private TreeBitSet(long capacity, Tree root){
    if(!isValidCapacity(capacity))
      throw new IllegalArgumentException("not valid capacity");

    this.capacity = capacity;
    this.root = makeTree(capacity);

    for(Long bit : root.toList(capacity))
      this.add(bit);
  }
  
  // * Exercise 10 * -
  public static TreeBitSet union(TreeBitSet set1, TreeBitSet set2){
	  if(set1.capacity() != set2.capacity())
      throw new IllegalArgumentException("sets have different capacity");

    Tree root = unionRec(set1.root, set2.root);
    TreeBitSet tbs = new TreeBitSet(set1.capacity(), root);

    return tbs;
  }
  
  private static Tree unionRec(Tree tree1, Tree tree2){
	    if(tree1 instanceof Leaf && tree2 instanceof Leaf){
        Leaf leaf1 = (Leaf) tree1;
        Leaf leaf2 = (Leaf) tree2;

        return new Leaf(LongBits.or(leaf1.bitset, leaf2.bitset));

      } else{
        Node node1 = (Node) tree1;
        Node node2 = (Node) tree2;

        return new Node(unionRec(node1.left, node2.left), unionRec(node1.right, node2.right));
      }
  }
  
  // * Exercise 11 * -
  public static TreeBitSet extendedUnion(TreeBitSet set1, TreeBitSet set2){
	    Tree root = extendedUnionRec(set1.root, set1.capacity(), set2.root, set2.capacity());
      long capacity = set1.capacity() <= set2.capacity() ? set2.capacity() : set1.capacity();
      return new TreeBitSet(capacity, root);
  }

  private static Tree extendedUnionRec(Tree tree1, long tam1, Tree tree2, long tam2){
 	    if(tree1 instanceof Leaf && tree2 instanceof Leaf){
        Leaf leaf1 = (Leaf) tree1;
        Leaf leaf2 = (Leaf) tree2;

        return new Leaf(LongBits.or(leaf1.bitset, leaf2.bitset));

      } else if(tree1 instanceof Leaf && tree2 instanceof Node){
        Leaf leaf1 = (Leaf) tree1;
        Node node2 = (Node) tree2;

        return new Node(extendedUnionRec(leaf1, tam1, node2.left, tam2/2), node2.right);

      } else if(tree1 instanceof Node && tree2 instanceof Leaf){
        Node node1 = (Node) tree1;
        Leaf leaf2 = (Leaf) tree2;
        
        return new Node(extendedUnionRec(node1.left, tam1/2, leaf2, tam2), node1.right);

      } else{
        Node node1 = (Node) tree1;
        Node node2 = (Node) tree2;

        return new Node(extendedUnionRec(node1.left, tam1/2, node2.left, tam2/2), extendedUnionRec(node1.right, tam1/2, node2.right, tam2/2));
      }
  }

  //-------------------------------------------------------------------
  // Basic program for testing your implementation
  //-------------------------------------------------------------------
  public static void main(String[] args) {
    TreeBitSet set = new TreeBitSet(64*2*2*2);
    set.add(0);
    set.add(1);
    set.add(2);
    set.add(3);
    set.add(4);
    set.add(5);
    set.add(6);
    set.add(7);
    set.add(8);
    set.add(9);
    set.add(10);
    set.add(120);
    set.add(128);
    set.add(270);

    System.out.println(set.size());

    System.out.println(set.contains(6));
    System.out.println(set.contains(128));
    System.out.println(set.contains(270));

    System.out.println(set.contains(11));
    System.out.println(set.contains(272));

    System.out.println(set.toList());
  }
}
