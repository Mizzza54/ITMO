#include <iostream>
#include <stdio.h>

// Инвертировать значение x: 1->0 или 0->1
int invert(int x) {
    return 1 ^ x;
}

// Возвращаем 1, если число положительное, и 0, если отрицательное
int sign(int x) {
    return invert((x >> 31) & 1);
}

int getMax(int a, int b) {
    int k = sign(a - b);
    int not_k = invert(k);
    return a * k + b * not_k;
}

int getMin(int a, int b) {
    int k = sign(a - b);
    int not_k = invert(k);
    return a * not_k + b * k;
}

int main() {
    printf("%d\n", getMax(9,-5));
    printf("%d\n", getMin(10,-3));
}
