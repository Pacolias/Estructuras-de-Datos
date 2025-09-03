
/**
 * Huffman trees and codes.
 *
 * Data Structures, Grado en Informatica. UMA.
 *
 *
 * Student's name: Paco Molina Cuenca
 * Student's group: Doble Grado Matemáticas e Ingeniería informática
 */

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.priorityQueue.BinaryHeapPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.priorityQueue.WBLeftistHeapPriorityQueue;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
        Dictionary<Character, Integer> res = new AVLDictionary<>();

        for(int i = 0; i < s.length(); i++){
            if(!res.isDefinedAt(s.charAt(i)))
                res.insert(s.charAt(i), 1);
            else 
                res.insert(s.charAt(i), res.valueOf(s.charAt(i)) + 1);
        }

        return res;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
        Dictionary<Character, Integer> dict = weights(s);
        PriorityQueue<WLeafTree<Character>> res = new WBLeftistHeapPriorityQueue<>();

        for(Tuple2<Character, Integer> par : dict.keysValues())
            res.enqueue(new WLeafTree<Character>(par._1(), par._2()));

        return res;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
        if(s.length() < 2)
            throw new HuffmanException("huffmanTree on string without at least two characters");
        
        PriorityQueue<WLeafTree<Character>> pq = huffmanLeaves(s);
        WLeafTree<Character> primero = pq.first();;
        WLeafTree<Character> segundo;

        while(!pq.isEmpty()){
            primero = pq.first();
            pq.dequeue();

            if(!pq.isEmpty()){
                segundo = pq.first();
                pq.dequeue();
                pq.enqueue(new WLeafTree<>(primero, segundo));
            }
        }

        return primero;
    }

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();

        for(Tuple2<Character, List<Integer>> par : d1.keysValues())
            res.insert(par._1(), par._2());
        
        for(Tuple2<Character, List<Integer>> par : d2.keysValues())
            res.insert(par._1(), par._2());

        return res;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        for(List<Integer> list : d.values())
            list.prepend(i);
        return d;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();

        if(ht.isLeaf())
            res.insert(ht.elem(), new LinkedList<>());

        return ht.isLeaf() ? res : joinDics(prefixWith(0, huffmanCode(ht.leftChild())), prefixWith(1, huffmanCode(ht.rightChild())));
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        List<Integer> codigo = new LinkedList<>();

        for(int i = 0; i < s.length(); i++){
            for(Integer num : hc.valueOf(s.charAt(i)))
                codigo.append(num);
        }
        
        return codigo;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        if(bits.isEmpty()){
            return "";

        } else{
            String mensaje = "";
            Tuple2<Character, List<Integer>> par = takeSymbol(bits, ht);
            return par._1() + decode(par._2(), ht);
        }
    }

    private static Tuple2<Character, List<Integer>> takeSymbol(List<Integer> bits, WLeafTree<Character> ht){
        if(ht.isLeaf()){
            return new Tuple2<>(ht.elem(), bits);

        } else{
            if(bits.get(0) == 0){
                bits.remove(0);
                return takeSymbol(bits, ht.leftChild());

            } else{
                bits.remove(0);
                return takeSymbol(bits, ht.rightChild());
            }
        }
    }
}
