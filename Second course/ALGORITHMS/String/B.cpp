#include <iostream>
#include <string>

using namespace std;

int* prefixFunction(string s) {
    int* p = new int[s.size()];
    p[0] = 0;
    for (int i = 1; i < s.length(); i++) {
        int k = p[i - 1];
        while (k > 0 && s[i] != s[k]) {
            k = p[k - 1];
        }
        if (s[i] == s[k]) {
            k++;
        }
        p[i] = k;
    }
    return p;
}

int main() {
    string str;
    cin >> str;
    int* p = prefixFunction(str);
    for (int i = 0; i < str.length(); i++) {
        cout << p[i] << " ";
    }
    return 0;
}