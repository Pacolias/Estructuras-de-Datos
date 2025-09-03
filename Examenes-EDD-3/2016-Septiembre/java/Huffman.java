
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
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.priorityQueue.WBLeftistHeapPriorityQueue;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
        Dictionary<Character, Integer> res = new AVLDictionary<>();

        for(Character c : s.toCharArray()){
            if(res.isDefinedAt(c))
                res.insert(c, 1 + res.valueOf(c));
            else 
                res.insert(c, 1);
        }

        return res;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
        PriorityQueue<WLeafTree<Character>> res = new WBLeftistHeapPriorityQueue<>();

        for(Tuple2<Character, Integer> par : weights(s).keysValues())
            res.enqueue(new WLeafTree<>(par._1(), par._2()));

        return res;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
        if(weights(s).size() < 2)
            throw new HuffmanException("huffmanTree: the string must have at least two different symbols");
        
        PriorityQueue<WLeafTree<Character>> pq = huffmanLeaves(s);
        WLeafTree<Character> primero = pq.first();
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
        Dictionary<Character, List<Integer>> res = new AVLDictionary<>();

        for(Tuple2<Character, List<Integer>> par : d.keysValues()){
            par._2().prepend(i);
            res.insert(par._1(), par._2());
        }

        return res;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        if(ht.isLeaf()){
            Dictionary<Character, List<Integer>> res = new AVLDictionary<>();
            res.insert(ht.elem(), new LinkedList<>());
            
            return res;
        
        } else{
            return joinDics(prefixWith(0, huffmanCode(ht.leftChild())), prefixWith(1, huffmanCode(ht.rightChild())));
        }

    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        List<Integer> code = new LinkedList<>();

        for(Character c : s.toCharArray()){
            for(Integer num : hc.valueOf(c))
                code.append(num);
        }

        return code;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        String msg = "";
        List<Integer> resto = bits;
        Tuple2<Character, List<Integer>> par;

        while(!resto.isEmpty()){ 
            par = takeSymbol(resto, ht);
            msg += par._1();
            resto = par._2();
        }

        return msg;
    }

    private static Tuple2<Character, List<Integer>> takeSymbol(List<Integer> codigo, WLeafTree<Character> ht){
        if(ht.isLeaf()){
            return new Tuple2<>(ht.elem(), codigo);

        } else{
            Integer first = codigo.get(0);
            codigo.remove(0);
            return first == 0 ? takeSymbol(codigo, ht.leftChild()) : takeSymbol(codigo, ht.rightChild());
        }
    }
}
