#include <iostream>
#include <string>

using namespace std;

int* zFunction(string s) {
    int* zf = new int[s.size()];
    int left = 0, right = 0;
    for (int i = 1; i < s.size(); i++) {
        zf[i] = max(0, min(right - i, zf[i - left]));

        while (i + zf[i] < s.size() && s[zf[i]]== s[i + zf[i]]) {
            zf[i]++;
        }

        if (i + zf[i] > right) {
            left = i;
            right = i + zf[i];
        }
    }
    return zf;
}

int main() {
    string str;
    cin >> str;
    int* p = zFunction(str);
    for (int i = 1; i < str.length(); i++) {
        cout << p[i] << " ";
    }
    return 0;
}