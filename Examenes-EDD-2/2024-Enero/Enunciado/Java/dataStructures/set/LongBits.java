import java.util.ArrayList;
import java.util.List;

public class LongBits {

    public static final int bitsPerLong = 64;

    // Devuelve true si el bit en la posición 'pos' está activado (1)
    public static boolean getBit(long bits, int pos) {
        return (bits & (1L << pos)) != 0;
    }

    // Activa el bit en la posición 'pos'
    public static long setBit(long bits, int pos) {
        return bits | (1L << pos);
    }

    // Devuelve el número de bits a 1
    public static int countOnes(long bits) {
        return Long.bitCount(bits);
    }

    // Devuelve una lista con los índices de bits activados
    public static List<Long> toList(long bits) {
        List<Long> result = new ArrayList<>();
        for (int i = 0; i < bitsPerLong; i++) {
            if (getBit(bits, i)) {
                result.add((long) i);
            }
        }
        return result;
    }

    // Hace OR entre dos bitsets
    public static long or(long b1, long b2) {
        return b1 | b2;
    }
}
