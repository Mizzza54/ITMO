package M;

import java.util.ArrayList;
import java.util.Scanner;

/**
 * @author Michael Gerasimov
 * start: 23.12.2020
 * @version -
 */
public class M {

    static long Prime = 31;
    static ArrayList<Long> PrimeDegree;

    static ArrayList<Long> initHashPref(String str) {
        ArrayList<Long> hashPref = new ArrayList<>(str.length() + 1);
        for (int i = 0; i < str.length() + 1; i++) {
            hashPref.add(0L);
        }
        hashPref.set(0, (long) 0);
        for (int i = 0; i < str.length(); i++) {
            hashPref.set(i + 1, hashPref.get(i)*Prime + str.charAt(i));
        }
        return hashPref;
    }

    static long hashOfSubString(int l, int r, ArrayList<Long> hashPref) { // [l..r]

        if (r < l) {
            return 0;
        }

        return hashPref.get(r + 1) - hashPref.get(l) * PrimeDegree.get(r - l + 1);
    }

    static boolean compare(int length, String str1, ArrayList<Long> hashPref1, String str2, ArrayList<Long> hashPref2) {
        if (str1.length() - length + 1 <= 0) {
            return false;
        }

        ArrayList<Long> hashesOfLength = new ArrayList<>(str1.length() - length + 1);
        for (int i = 0; i < str1.length() - length + 1; i++) {
            hashesOfLength.add(0L);
        }

        for (int i = 0; i < str1.length() - length + 1; i++) {

            if (length + i - 1 < 0) {
                return false;
            }

            hashesOfLength.set(i, hashOfSubString(i, length + i - 1, hashPref1));
        }

        for (int i = 0; i < str2.length(); i++) {

            if (length + i >= hashPref2.size()) {
                continue;
            }

            long hashSub = hashOfSubString(i, length + i - 1, hashPref2);
            if (hashesOfLength.contains(hashSub)) {
            /*
             * if совпали несколько случайных символов подстрок
                    return true
                else
                    continue
             */
                return true;
            }
        }
        return false;
    }

    static int binSearch(int length, String str1, ArrayList<Long> hash1, String str2, ArrayList<Long> hash2) {
        int l = 0;
        int r = length + 1;
        while (l < r - 1) {
            int m = (l + r) / 2;
            if (compare(m, str1, hash1, str2, hash2)) {
                l = m;
            } else {
                r = m;
            }
        }

        if (r - 1 < 0) {
            return 0;
        }

        return r - 1;
    }

    public static void main(String[] args) {

        String str1_raw, str2_raw, str1, str2;
        Scanner scanner = new Scanner(System.in);

        str1_raw = scanner.next();
        str2_raw = scanner.next();


        if (str1_raw.length() > str2_raw.length()) {
            str1 = str2_raw;
            str2 = str1_raw;
        } else {
            str1 = str1_raw;
            str2 = str2_raw;
        }

        int MaxLength = Math.max(str1.length(), str2.length());
        int MinLength = Math.min(str1.length(), str2.length());

        PrimeDegree = new ArrayList<>(MaxLength + 1);

        for (int i = 0; i < MaxLength + 1; i++) {
            PrimeDegree.add(0L);
        }

        PrimeDegree.set(0, (long) 1);
        for (int i = 1; i < MaxLength + 1; i++) {
            PrimeDegree.set(i, PrimeDegree.get(i - 1) * Prime);
        }

        ArrayList<Long> hashPref1 = initHashPref(str1);
        ArrayList<Long> hashPref2 = initHashPref(str2);

        int length = binSearch(MinLength, str1, hashPref1, str2, hashPref2);


        ArrayList<Long> hashesOfLength = new ArrayList<>(str1.length() - length + 1);

        for (int i = 0; i < str1.length() - length + 1; i++) {
            hashesOfLength.add(0L);
        }

        for (int i = 0; i < str1.length() - length + 1; i++) {
            hashesOfLength.set(i, (hashOfSubString(i, length + i - 1, hashPref1)));
        }

        String ans = "~";

        for (int i = 0; i < str2.length() - length + 1; i++) {

            long hashSub = hashOfSubString(i, length + i - 1, hashPref2);
            if (hashesOfLength.contains(hashSub)) {
            /*
             * if совпали несколько случайных символов подстрок
                    return true
                else
                    continue
             */

                if (ans.compareTo(str2.substring(i, i + length)) > 0) {
                    ans = str2.substring(i, i + length);
                }
            }
        }

        System.out.println(ans);
    }
}
