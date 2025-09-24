package dataStructures.graph;

import java.util.*;

public class DiGraphBFT {

    public static <V> List<V> bft(DiGraph<V> g, V start) {
        Set<V> visited = new HashSet<>();
        Queue<V> queue = new LinkedList<>();
        List<V> result = new ArrayList<>();

        queue.add(start);

        while (!queue.isEmpty()) {
            V v = queue.poll();
            if (!visited.contains(v)) {
                visited.add(v);
                result.add(v);
                for (V u : g.successors(v)) {
                    if (!visited.contains(u)) {
                        queue.add(u);
                    }
                }
            }
        }

        return result;
    }
}

