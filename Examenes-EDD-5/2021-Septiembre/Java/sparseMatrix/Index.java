package sparseMatrix;

public class Index implements Comparable<Index> {
    private final int row;
    private final int column;

    public Index(int r, int c) {
        this.row = r;
        this.column = c;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    @Override
    public int compareTo(Index ind) {
        int cmp = Integer.compare(row, ind.row);
        if (cmp == 0)
            cmp = Integer.compare(column, ind.column);
        return cmp;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Index)) return false;
        Index other = (Index) obj;
        return row == other.row && column == other.column;
    }

    @Override
    public int hashCode() {
        return 31 * row + column;
    }
}
