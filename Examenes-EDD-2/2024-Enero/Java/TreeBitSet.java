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
    
    public Leaf(long bitset) {
      this.bitset = bitset;
    }

    public long size() {
      return LongBits.countOnes(bitset);
    }

    public boolean contains(long element, long capacity) {
      return LongBits.getBit(bitset, (int) element);
    }

    public void add(long element, long capacity) {
      bitset = LongBits.setBit(bitset, (int) element);
    }

    public List<Long> toList(long capacity) {
      List<Long> res = new LinkedList<>();

      for(Long bit : LongBits.toList(bitset))
        res.append(bit);
        
      return res;
    }

    public Tree cloneTree() {
      return new Leaf(bitset);
    }
  }

  private static class Node implements Tree {
    private final Tree left, right;
    
    public Node(Tree left, Tree right) {
      this.left = left;
      this.right = right;
    }

    public long size() {
      return left.size() + right.size();
    }

    public boolean contains(long element, long capacity) {
      long half = capacity/2;
      return element < half ? left.contains(element, half) : right.contains(element - half, half);
    }

    public void add(long element, long capacity) {
      long half = capacity/2;
      if(element < half)
        left.add(element, half);
      else 
        right.add(element - half, half);
    }

    public List<Long> toList(long capacity) {
      List<Long> leftList = left.toList(capacity/2);
      List<Long> rightList = right.toList(capacity/2);
      List<Long> res = new LinkedList<>();

      for(Long bit : leftList)
        res.append(bit);
      
      for(Long bit : rightList)
        res.append(bit + (capacity/2));
      
      return res;
    }

    public Tree cloneTree() {
      return new Node(left.cloneTree(), right.cloneTree());
    }
  }


  // * Exercise 1 * -
  private static Tree makeTree(long capacity) {
    return capacity <= BITS_PER_LEAF ? new Leaf(0) : new Node(makeTree(capacity/2), makeTree(capacity/2));
  }

  // * Exercise 2 * -
  public TreeBitSet(long capacity) {
    if(!isValidCapacity(capacity)){
      if(capacity < 0)
        throw new IllegalArgumentException("capacity must be positive");
      else 
        throw new IllegalArgumentException("capacity must be 64 multiplied by a power of 2");
    } else {
      root = makeTree(capacity);
      this.capacity = capacity;
    }

  }

  // * Exercise 3 * -
  public long capacity() {
    return capacity;
  }

  // * Exercise 4 * -
  private boolean outOfRange(long element) {
    return element < 0 || element >= capacity;
  }

  // * Exercise 5 * -
  public long size() {
    return root.size();
  }
  
  // El método size en la clase TreeBitSet devuelve el tamaño total del conjunto, que es el número de elementos distintos en el conjunto.
  //   root es el árbol subyacente que representa el conjunto en la implementación del árbol de bits.
  //   root.size() invoca el método size del árbol root. 
  //   Este método es parte de la interfaz Tree (ya sea Leaf o Node), y su implementación depende de si el nodo es una hoja (Leaf) o un nodo interno (Node).
  
  // Por lo tanto, el método size() en la clase TreeBitSet simplemente delega la llamada al método size del árbol raíz (root). 
  // La implementación específica de size en los nodos (Leaf o Node) se encargará de calcular el tamaño efectivo del conjunto dependiendo de la estructura del árbol 
  // y cómo se manejan las hojas y los nodos internos.

  // * Exercise 6 * -
  public boolean isEmpty() {
    return root.size() == 0;
  }

  // * Exercise 7 * -
  public boolean contains(long element) {
    return outOfRange(element) ? false : root.contains(element, capacity);
  }

  // * Exercise 8 * -
  public void add(long element) {
    if(outOfRange(element))
      throw new IllegalArgumentException("element is out of range");
    
    root.add(element, capacity);
  }

  // * Exercise 9 * -
  public List<Long> toList() {
    return root.toList(capacity);
  }


  //-------------------------------------------------------------------
  // Only for students without continuous assessment
  //-------------------------------------------------------------------
  
  /*
  private TreeBitSet(long capacity, Tree root) {
	    
  }
  
  // * Exercise 10 * -
  public static TreeBitSet union(TreeBitSet set1, TreeBitSet set2) {
	   
  }
  
  private static Tree unionTrees(Tree tree1, Tree tree2) {
	    
  }
  
  private static Tree unionTreesRecursive(Tree tree1, Tree tree2) {
	   
  }

  // * Exercise 11 * -
  public static TreeBitSet extendedUnion(TreeBitSet set1, TreeBitSet set2) {
	    
  }
  */

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
