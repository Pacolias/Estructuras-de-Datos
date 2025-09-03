package dataStructures.graph;

public record DiEdge<V>(V source, V target) {
    @Override
    public String toString() {
        return source + " -> " + target;
    }
}

