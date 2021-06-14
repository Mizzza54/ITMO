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
    int* pref = prefixFunction(str);
    if (str.size() % (str.size() - pref[str.size() - 1]) == 0) {
        cout << str.size() - pref[str.size() - 1];
    } else {
        cout << str.size();
    }

    return 0;
}