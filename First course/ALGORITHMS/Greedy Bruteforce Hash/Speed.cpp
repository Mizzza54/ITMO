#include <iostream>
#include <utility>

/**
 * @author Michale Gerasimov
 * start: 08.06.2020
 * @version -
 */

int main() {
    FILE *in = fopen("dowry.in", "r");
    FILE *out = fopen("dowry.out", "w");

    int n;
    long long L, R;
    fscanf(in, "%d %lld %lld", &n, &L, &R);
    std::pair<int, int> array[n];
    for (int i = 0; i < n; i++) {
        int weight, price;
        fscanf(in, "%d %d", &weight, &price);
        array[i].first = weight;
        array[i].second = price;
    }

    long long MaxPrice = 0;
    int MaxCount = 0;
    long long MaxMask = 0;
    for (long long mask = 0; mask < (1 << n); mask++) {
        long long price = 0;
        long long weight = 0;
        int count = 0;
        for (int i = 0; i < n; i++) {
            long long tmp = mask & (1 << i);

            if (tmp > 0) {
                weight += array[i].first;
                price += array[i].second;
                count++;
            }

            if (weight > R) {
                break;
            }
        }
        if (price > MaxPrice && L <= weight && weight <= R) {
            MaxPrice = price;
            MaxCount = count;
            MaxMask = mask;
        }
    }

    if (MaxCount == 0) {
        fprintf(out, "0\n");
    } else {
        fprintf(out, "%d\n", MaxCount);

        for (int i = 0; i < n; i++) {
            long long tmp = MaxMask & (1 << i);
            if (tmp > 0) {
                fprintf(out, "%d\n", i + 1);
            }
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}