#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <unordered_set>


using namespace std;

long long Prime = 31;
vector <long long> PrimeDegree;
string ans = "~";

void initHashPref(const string& str, vector<long long>& hashPref) {
    hashPref.resize(str.size() + 1);
    hashPref[0] = 0;
    for (int i = 0; i < str.size(); i++) {
        hashPref[i + 1] = hashPref[i] * Prime + str[i];
    }
}

long long hashOfSubString(int l, int r, const vector<long long> & hashPref) { // [l..r]

    if (r < l) {
        return 0;
    }

    return hashPref[r + 1] - hashPref[l] * PrimeDegree[r - l + 1];
}

bool compare(int length, const string& str1, const vector<long long>& hashPref1, const string& str2, const vector<long long>& hashPref2) {
    if (str1.size() - length + 1 <= 0) {
        return false;
    }

    string curAns = "~";
    unordered_set<long long> hashesOfLength;

    for (int i = 0; i < str1.size() - length + 1; i++) {

        if (length + i - 1 < 0) {
            return false;
        }

        hashesOfLength.insert((hashOfSubString(i, length + i - 1, hashPref1)));
    }

    for (int i = 0; i < str2.size(); i++) {

        if (length + i >= hashPref2.size()) {
            continue;
        }

        long long hashSub = hashOfSubString(i, length + i - 1, hashPref2);
        if (hashesOfLength.count(hashSub)) {

            return true;
        }
    }

    return false;
}

int binSearch(int length, const string& str1, const vector<long long>& hash1, const string& str2, const vector<long long>& hash2) {
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

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);

    string str1_raw, str2_raw, str1, str2;
    cin >> str1_raw;
    cin >> str2_raw;


    if (str1_raw.size() > str2_raw.size()) {
        str1 = str2_raw;
        str2 = str1_raw;
    } else {
        str1 = str1_raw;
        str2 = str2_raw;
    }

    int MaxLength = max(str1.size(), str2.size());
    int MinLength = min(str1.size(), str2.size());

    PrimeDegree.resize(MaxLength + 1);
    PrimeDegree[0] = 1;
    for (int i = 1; i < MaxLength + 1; i++) {
        PrimeDegree[i] = PrimeDegree[i - 1] * Prime;
    }


    vector<long long> hashPref1;
    vector<long long> hashPref2;
    initHashPref(str1, hashPref1);
    initHashPref(str2, hashPref2);
    int length = binSearch(MinLength, str1, hashPref1, str2, hashPref2);

    if (str1.size() - length + 1 <= 0) {
        return 0;
    }

    unordered_set<long long> hashesOfLength;
    for (int i = 0; i < str1.size() - length + 1; i++) {
        hashesOfLength.insert(hashOfSubString(i, length + i - 1, hashPref1));
    }

    for (int i = 0; i < str2.size() - length + 1; i++) {

        if (length + i >= hashPref2.size()) {
            continue;
        }

        long long hashSub = hashOfSubString(i, length + i - 1, hashPref2);
        if (hashesOfLength.count(hashSub)) {

            if (length < 0) {
                return 0;
            }

            if (str2.substr(i, length) < ans) {
                ans = str2.substr(i, length);
            }
        }
    }

    cout << ans;
    return 0;
}