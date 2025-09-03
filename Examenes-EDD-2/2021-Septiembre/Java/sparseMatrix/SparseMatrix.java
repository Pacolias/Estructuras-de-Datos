package sparseMatrix;

import java.util.List;
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
        return values.get(idx) == null ? 0 : values.get(idx);
    }

    // Modifica el valor en una posición (privado)
    private void update(Index idx, int val) {
        values.put(idx, val);
    }

    // Devuelve un índice validado
    private Index index(int r, int c) {
        return new Index(r, c);
    }

    // Pública: set
    public void set(int r, int c, int val) {
        if(r < 0 || c < 0 || r > rows || c > cols)
            throw new IllegalArgumentException("Invalid rows or columns");
        
        Index idx = new Index(r, c);

        if(val == 0 && values.containsKey(idx))
            values.remove(idx);
        else if(val != 0)
            values.put(idx, val);
    }

    // Pública: get
    public int get(int r, int c) {
        if(r < 0 || c < 0 || r > rows || c > cols)
            throw new IllegalArgumentException("Invalid rows or columns");

        return value(new Index(r, c));
    }

    // Pública: suma de dos matrices
    public SparseMatrix add(SparseMatrix other) {
        if(rows != other.rows || cols != other.cols)
            throw new IllegalArgumentException("The given matrices do not have the same dimension");
        
        SparseMatrix res = new SparseMatrix(rows, cols);

        for(Index idx : values.keySet())
            res.values.put(idx, values.get(idx));

        int value;

        for(Index idx : other.values.keySet()){
            if(values.get(idx) == null)
                value = 0;
            else 
                value = values.get(idx);

            res.set(idx.getRow(), idx.getColumn(), value + other.values.get(idx));
        }

        return res;
    }

    // Pública: traspuesta
    public SparseMatrix transpose() {
        SparseMatrix res = new SparseMatrix(rows, cols);

        for(Index idx : values.keySet())
            res.values.put(new Index(idx.getColumn(), idx.getRow()), values.get(idx));

        return res;
    }

    // Pública: fromList
    public static SparseMatrix fromList(int rows, int cols, List<Integer> data) {
        if(data.size() % 3 != 0)
            throw new IllegalArgumentException("data list size is not multiple of 3");

        SparseMatrix res = new SparseMatrix(rows, cols);

        for(int i = 0; i < data.size(); i += 3)
            res.set(data.get(i), data.get(i + 1), data.get(i + 2));

        return res;
    }
}
