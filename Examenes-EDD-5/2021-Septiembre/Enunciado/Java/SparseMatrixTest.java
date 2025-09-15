package sparseMatrix;

import java.util.Arrays;
import java.util.List;

public class SparseMatrixTest {
    public static void main(String[] args) {
        System.out.println("--- Test: fromList ---");
        List<Integer> data = Arrays.asList(0, 0, 1, 1, 1, 2, 2, 2, 3);
        SparseMatrix sm1 = SparseMatrix.fromList(3, 3, data);
        printMatrix(sm1, 3, 3);

        System.out.println("\n--- Test: get ---");
        System.out.println(sm1.get(0, 0)); // 1
        System.out.println(sm1.get(1, 1)); // 2
        System.out.println(sm1.get(0, 1)); // 0 (no valor)

        System.out.println("\n--- Test: set ---");
        sm1.set(0, 1, 9);
        printMatrix(sm1, 3, 3);
        sm1.set(1, 1, 0);  // eliminar
        printMatrix(sm1, 3, 3);

        System.out.println("\n--- Test: transpose ---");
        SparseMatrix smT = sm1.transpose();
        printMatrix(smT, 3, 3);

        System.out.println("\n--- Test: add ---");
        List<Integer> data2 = Arrays.asList(0, 0, 5, 1, 2, 4, 2, 2, -3);
        SparseMatrix sm2 = SparseMatrix.fromList(3, 3, data2);
        SparseMatrix sum = sm1.add(sm2);
        printMatrix(sum, 3, 3);
    }

    private static void printMatrix(SparseMatrix sm, int rows, int cols) {
        for(int r = 0; r < rows; r++) {
            for(int c = 0; c < cols; c++) {
                System.out.print(sm.get(r, c) + " ");
            }
            System.out.println();
        }
    }
}

