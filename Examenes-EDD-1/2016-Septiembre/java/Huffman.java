
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
import dataStructures.priorityQueue.EmptyPriorityQueueException;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.priorityQueue.WBLeftistHeapPriorityQueue;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
        Dictionary<Character, Integer> pesos = new AVLDictionary<>();

        for(Character caracter : s.toCharArray()){

            if(pesos.isDefinedAt(caracter))
                pesos.insert(caracter, pesos.valueOf(caracter) + 1);
            else 
                pesos.insert(caracter, 1);
        }

        return pesos;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
        Dictionary<Character, Integer> pesos = weights(s);
        PriorityQueue<WLeafTree<Character>> cola = new WBLeftistHeapPriorityQueue<>();

        for(Tuple2<Character, Integer> par : pesos.keysValues())
            cola.enqueue(new WLeafTree(par._1(), par._2()));
        
        return cola;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
        if(weights(s).size() < 2)
            throw new HuffmanException("La palabra no tiene al menos dos caracteres distintos");
        
        return construirArbol(huffmanLeaves(s));
    }

    private static WLeafTree<Character> construirArbol(PriorityQueue<WLeafTree<Character>> cola){

        WLeafTree<Character> primero = cola.first();
        cola.dequeue();
        WLeafTree<Character> segundo;

        try{
            segundo = cola.first();
        } catch(EmptyPriorityQueueException e){
            segundo = null;
        }

        if(cola.isEmpty()){
            return primero;

        } else{
            cola.dequeue(); 
            return construirArbol(cola.enqueue(merge(primero, segundo)));
        }
    }

    private static WLeafTree<Character> merge(WLeafTree<Character> lt, WLeafTree<Character> rt){
        return new WLeafTree<>(lt, rt);
    }

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        Dictionary<Character, List<Integer>> dictUnion = new AVLDictionary<>();

        for(Tuple2<Character, List<Integer>> par : d1.keysValues())
            dictUnion.insert(par._1(), par._2());
        
        for(Tuple2<Character, List<Integer>> par : d2.keysValues())
            dictUnion.insert(par._1(), par._2());

        return dictUnion;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        Dictionary<Character, List<Integer>> dict = new AVLDictionary<>();
        List<Integer> nuevaLista = null;

        for(Tuple2<Character, List<Integer>> par : d.keysValues()){
            nuevaLista = new LinkedList<>();
            nuevaLista.append(i);

            for(Integer numero : par._2())
                nuevaLista.append(numero);

            dict.insert(par._1(), nuevaLista);
        }
        
        return dict;
    }

    // Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        if(ht.isLeaf()){
            Dictionary<Character, Integer> dict = new AVLDictionary<>();
            List<Integer> lista = new LinkedList<>();
            dict.insert(ht.elem(), lista);

            return dict;

        } else{ 
            return joinDics(prefixWith(0, huffmanCode(ht.leftChild())), prefixWith(1, huffmanCode(ht.rightChild())));
        }
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        List<Integer> codigo = new LinkedList<>();

        for(Character c : s.toCharArray()){
            for(Integer numero : hc.valueOf(c))
                codigo.append(numero);
        }

        return codigo;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        if(bits.isEmpty()){
            return "";
        
        } else{
            Tuple2<Character, List<Integer>> par = takeSymbol(bits, ht);
            return par._1() + decode(par._2(), ht);
        } 
    }

    private static Tuple2<Character, List<Integer>> takeSymbol(List<Integer> bits, WLeafTree<Character> ht){
        if(ht.isLeaf()){
            return new Tuple2<>(ht.elem(), bits);

        } else{
            int bit = bits.get(0);
            List<Integer> resto = new LinkedList<>();

            for(int i = 1; i < bits.size(); i++)
                resto.append(bits.get(i));
            
            if(bit == 0){
                return takeSymbol(resto, ht.leftChild());
            
            } else{
                return takeSymbol(resto, ht.rightChild());            
            }
        }
    }
}
