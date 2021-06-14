#include <iostream>
#include <string>
#include <vector>

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

vector<int> kmp(const string& P, const string& T) {
    int pl = P.size();
    int tl = T.size();
    int* pref = prefixFunction(P + "#" + T);
    vector <int> answer;
    for (int i = pl + 1; i < pl + tl + 1; i++) {
        if (pref[i] == pl) {
            answer.push_back(i - 2 * pl);
        }
    }
    return answer;
}

int main() {
    string P, T;
    cin >> P;
    cin >> T;
    vector<int> Answer = kmp(P, T);
    cout << Answer.size() << endl;
    for (int i : Answer) {
        cout << i + 1 << " ";
    }
    return 0;
}