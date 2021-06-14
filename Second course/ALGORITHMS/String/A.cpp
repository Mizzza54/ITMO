#include <iostream>
#include <string>

using namespace std;

int Prime = 31;
long long *PrimeDegree;
long long *hashStr;

void my_hash(const string& str, int board) {
    hashStr[0] = str[0];
    for (int i = 1; i < board; i++) {
        hashStr[i] = hashStr[i - 1] + PrimeDegree[i] * str[i];
    }
}

int main() {
    string str;
    cin >> str;
    PrimeDegree = new long long[str.size()];
    hashStr = new long long[str.size()];
    PrimeDegree[0] = 1;
    for (int i = 1; i < str.length(); i++) {
        PrimeDegree[i] = PrimeDegree[i - 1] * Prime;
    }

    my_hash(str, str.size());

    int m;
    cin >> m;

    for (int i = 0; i < m; i++) {
        int left1, right1, left2, right2;
        cin >> left1 >> right1 >> left2 >> right2;

        left1 -= 1;
        right1 -= 1;
        left2 -= 1;
        right2 -= 1;

        long long firstHash = hashStr[right1];
        long long secondHash = hashStr[right2];
        if (left1 != 0) firstHash -= hashStr[left1 - 1];
        if (left2 != 0) secondHash -= hashStr[left2 - 1];

        if (firstHash * PrimeDegree[left2] == secondHash * PrimeDegree[left1]) {
            cout << "Yes" << endl;
        } else {
            cout << "No" << endl;
        }
    }
    return 0;
}