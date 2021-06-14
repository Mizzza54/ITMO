/**
* @author Michael Gerasimov
* start: 13.04.2021
* @version -
*/
#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

long long MOD = 104857601;

long long NormalizeValue(long long x) {
    return x % MOD + ((x < 0) ? MOD : 0);
}

vector<long long> buildInversion(vector<long long>& array) {
    vector<long long> result(array.size());
    for (int i = 0; i < result.size(); i++) {
        result[i] = i % 2 == 0 ? NormalizeValue(array[i]) : NormalizeValue(-array[i]);
    }
    return result;
}

long long getElement(vector<long long>& array, int index) {
    return index >= array.size() ? 0L : array[index];
}

long long add(long long x, long long y) {
    return NormalizeValue(x + y);
}

long long subtraction(long long x, long long y) {
    return NormalizeValue(x - y);
}

long long multiply(long long x, long long y) {
    return NormalizeValue(x * y);
}

vector<long long> multiply(vector<long long>& x, vector<long long>& y)
{
    vector<long long> result(x.size() + y.size() + 1, 0);

    for (int i = 0; i < result.size(); i += 2) {
        for (int j = 0; j < i + 1; j++) {
            result[i] = add(result[i], multiply(getElement(x, j), getElement(y, i - j)));
        }
    }

    return result;
}

long long getNth(long long n, int k, vector<long long>& beginValues, vector<long long>& denominator) {

    vector<long long> result;
    while (n >= k) {
        for (int i = k; i <= 2 * k - 1; i++) {
            beginValues[i] = 0;
            for (int j = 1; j <= k; j++) {
                beginValues[i] = subtraction(getElement(beginValues, i),
                                             multiply(getElement(denominator, j),
                                                      getElement(beginValues, i - j)));
            }
        }
        vector<long long> inversionDenominator = buildInversion(denominator);

        result = multiply(denominator, inversionDenominator);
        int start = static_cast<int>(n % 2);
        for (int i = start; i < 2 * k; i += 2) {
            beginValues[i / 2] = getElement(beginValues, i);
        }
        for (int i = 0; i <= k; i++) {
            denominator[i] = getElement(result, i * 2);
        }
        n /= 2;
    }
    return getElement(beginValues, (int)n);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    long long n;
    int k;
    cin >> k >> n;
    vector<long long> beginValues(2 * k + 1);
    vector<long long> denominator(k + 1);
    n--;
    for (int i = 0; i < k; i++) {
        cin >> beginValues[i];
    }

    denominator[0] = 1;
    for (int i = 1, temp; i < k + 1; i++) {
        cin >> temp;
        denominator[i] = (-temp + MOD) % MOD;
    }

    cout << getNth(n, k, beginValues, denominator);
}