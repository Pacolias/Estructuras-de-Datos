package sparseMatrix;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

public class SparseMatrix {
    private final int rows;
    private final int cols;
    private final TreeMap<Index, Integer> values;

    public SparseMatrix(int rows, int cols) {
        this.rows = rows;
        this.cols = cols;
        values = new TreeMap<>();
    }

    // Devuelve el valor en una posición (privado)
    private int value(Index idx) {
        return values.getOrDefault(idx, 0);
    }

    // Modifica el valor en una posición (privado)
    private void update(Index idx, int val) {
        if(val == 0)
            values.remove(idx);
        else 
            values.put(idx, val);
    }

    // Devuelve un índice validado
    private Index index(int r, int c) {
        if(r < 0 || c < 0 || r >= rows || c >= cols)
            throw new IllegalArgumentException("index out of bounds");
        
        return new Index(r, c);
    }

    // Pública: set
    public void set(int r, int c, int val) {
        if(r < 0 || c < 0 || r >= rows || c >= cols)
            throw new IllegalArgumentException("index out of bounds");
        
        update(index(r, c), val);
    }

    // Pública: get
    public int get(int r, int c) {
        return value(index(r, c));
    }

    // Pública: suma de dos matrices
    public SparseMatrix add(SparseMatrix other) {
        if(rows != other.rows || cols != other.cols)
            throw new IllegalArgumentException("matrices do not have the same dimension");

        SparseMatrix sm = new SparseMatrix(rows, cols);
        Set<Index> indices = new HashSet<>();

        indices.addAll(values.keySet());
        indices.addAll(other.values.keySet());
        
        for(Index idx : indices)
            sm.set(idx.getRow(), idx.getColumn(), this.value(idx) + other.value(idx));
        
        return sm;
    }

    // Pública: traspuesta
    public SparseMatrix transpose() {
        SparseMatrix sm = new SparseMatrix(cols, rows);

        for(Index idx : values.keySet())
            sm.set(idx.getColumn(), idx.getRow(), this.value(idx));
        
        return sm;
    }

    // Pública: fromList
    public static SparseMatrix fromList(int rows, int cols, List<Integer> data) {
        if(data.size() % 3 != 0)
            throw new IllegalArgumentException("list's length is not multiple of 3");
        
        SparseMatrix sm = new SparseMatrix(rows, cols);

        for(int i = 0; i < data.size(); i += 3)
            sm.set(data.get(i), data.get(i + 1), data.get(i + 2));

        return sm;
    }
}
