package sparseMatrix;

import java.util.TreeMap;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

public class SparseMatrix {
    private final int rows;
    private final int cols;
    private final TreeMap<Index, Integer> values;

    public SparseMatrix(int rows, int cols) {
        if(rows < 0 || cols < 0)
            throw new IllegalArgumentException("Dimensiones no validas");

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
        if(val == 0)
            values.remove(idx);
        else 
            values.put(idx, val);
    }

    // Devuelve un índice validado
    private Index index(int r, int c) {
        if(r < 0 || r > rows || c < 0 || c > cols)
            throw new IllegalArgumentException("Dimensiones no validas");
        else 
            return new Index(r, c);
    }

    // Pública: set
    public void set(int r, int c, int val) {
        update(index(r, c), val);
    }

    // Pública: get
    public int get(int r, int c) {
        return value(index(r, c));
    }

    // Pública: suma de dos matrices
    public SparseMatrix add(SparseMatrix other) {
        if(this.rows != other.rows || this.cols != other.rows)
            throw new IllegalArgumentException("Las matrices no tienen la misma dimension");
        
        SparseMatrix resultado = new SparseMatrix(rows, cols);

        Set<Index> indices = new HashSet<>(values.keySet());
        indices.addAll(other.values.keySet());
        
        for(Index indice : indices){
            if(values.containsKey(indice) && other.values.containsKey(indice) && values.get(indice) + other.values.get(indice) != 0)
                resultado.update(indice, values.get(indice) + other.values.get(indice));
            else if(values.containsKey(indice))
                resultado.update(indice, values.get(indice));
            else if(other.values.containsKey(indice))
                resultado.update(indice, other.values.get(indice));        
        }

        return resultado;
    }

    // Pública: traspuesta
    public SparseMatrix transpose() {
        SparseMatrix resultado = new SparseMatrix(cols, rows);

        for(Index indice : values.keySet())
            resultado.update(new Index(indice.getColumn(), indice.getRow()), values.get(indice));

        return resultado;
    }

    // Pública: fromList
    public static SparseMatrix fromList(int rows, int cols, List<Integer> data) {
        SparseMatrix resultado = new SparseMatrix(rows, cols);
        int i = 0;

        while(i < data.size()){
            resultado.set(data.get(i), data.get(i + 1), data.get(i + 2));
            i += 3;
        }

        return resultado;
    }
}
