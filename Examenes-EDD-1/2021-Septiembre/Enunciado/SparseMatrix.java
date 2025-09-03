package sparseMatrix;

import java.util.TreeMap;
import java.util.List;

public class SparseMatrix {
    private final int rows;
    private final int cols;
    private final TreeMap<Index, Integer> values;

    public SparseMatrix(int rows, int cols) {
        this.rows = rows;
        this.cols = cols;
        this.values = new TreeMap<>();
    }

    // Devuelve el valor en una posición (privado)
    private int value(Index idx) {
        return values.getOrDefault(idx, 0);
    }

    // Modifica el valor en una posición (privado)
    private void update(Index idx, int val) {
        if (val == 0) {
            values.remove(idx);
        } else {
            values.put(idx, val);
        }
    }

    // Devuelve un índice validado
    private Index index(int r, int c) {
        if (r < 0 || r >= rows || c < 0 || c >= cols)
            throw new IndexOutOfBoundsException("Índice fuera de rango");
        return new Index(r, c);
    }

    // Pública: set
    public void set(int r, int c, int val) {
        Index idx = index(r, c);
        update(idx, val);
    }

    // Pública: get
    public int get(int r, int c) {
        Index idx = index(r, c);
        return value(idx);
    }

    // Pública: suma de dos matrices
    public SparseMatrix add(SparseMatrix other) {
        // Verificar dimensiones, lanzar excepción si no coinciden
        // Implementación eficiente
        return null; // por implementar
    }

    // Pública: traspuesta
    public SparseMatrix transpose() {
        return null; // por implementar
    }

    // Pública: fromList
    public static SparseMatrix fromList(int rows, int cols, List<Integer> data) {
        if (data.size() % 3 != 0)
            throw new IllegalArgumentException("La lista no es múltiplo de 3");
        SparseMatrix mat = new SparseMatrix(rows, cols);
        for (int i = 0; i < data.size(); i += 3) {
            int r = data.get(i);
            int c = data.get(i + 1);
            int v = data.get(i + 2);
            mat.set(r, c, v);
        }
        return mat;
    }
}
