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

    }

    // Modifica el valor en una posición (privado)
    private void update(Index idx, int val) {

    }

    // Devuelve un índice validado
    private Index index(int r, int c) {

    }

    // Pública: set
    public void set(int r, int c, int val) {

    }

    // Pública: get
    public int get(int r, int c) {

    }

    // Pública: suma de dos matrices
    public SparseMatrix add(SparseMatrix other) {

    }

    // Pública: traspuesta
    public SparseMatrix transpose() {

    }

    // Pública: fromList
    public static SparseMatrix fromList(int rows, int cols, List<Integer> data) {

    }
}
