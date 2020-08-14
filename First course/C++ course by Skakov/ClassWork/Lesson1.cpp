#include <stdio.h>

int invert(int x) {
    return 1 ^ x;
}

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

int Task1() {  // Задание: Медиана по трем числам. Найти среднее число.
    int a, b, c;
    scanf("%d %d %d", &a, &b, &c);
    int mid = a + b + c - getMax(getMax(a,b),c) - getMin(getMin(a,b),c);
    printf("%d\n", mid);
    return 0;
}

int main() {
    unsigned int h;  // Создание переменной под названием x типа unsigned int
    typedef unsigned int uint;  // Создание типа под названием uint, который характеризует тип unsigned int
    Task1();
}
