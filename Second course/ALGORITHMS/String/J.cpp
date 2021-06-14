#include <cstdio>
#include <iostream>
#include <algorithm>
#include <vector>
#include "string"

using namespace std;

const int sizeAlphabet = 256;
const int MaxLength = 400001;

vector<int> initSuffixArray(const string& str) {
    int n = str.length();
    vector<int> suffixArray(MaxLength);
    vector<int> count(sizeAlphabet, 0);
    vector<int> classesArray(MaxLength);

    for (int i = 0; i < n; i++) {
        count[str[i]]++;
    }
    for (int i = 1; i < sizeAlphabet; i++) {
        count[i] += count[i - 1];
    }
    for (int i = 0; i < n; i++) {
        suffixArray[--count[str[i]]] = i;
    }

    classesArray[suffixArray[0]] = 0;
    int classesCount = 1;
    for (int i = 1; i < n; i++) {
        if (str[suffixArray[i]] != str[suffixArray[i - 1]]) {
            classesCount++;
        }
        classesArray[suffixArray[i]] = classesCount - 1;
    }

    vector<int> temp_pn(MaxLength);
    vector<int> temp_cn(MaxLength);

    for (int h = 0; (1 << h) < n; h++) {
        for (int i = 0; i < n; i++) {
            temp_pn[i] = suffixArray[i] - (1 << h);
            if (temp_pn[i] < 0)  {
                temp_pn[i] += n;
            }
        }
        vector<int> count(classesCount, 0);

        for (int i = 0; i < n; i++) {
            ++count[classesArray[temp_pn[i]]];
        }
        for (int i = 1; i < classesCount; i++) {
            count[i] += count[i - 1];
        }
        for (int i = n - 1; i >= 0; i--) {
            suffixArray[--count[classesArray[temp_pn[i]]]] = temp_pn[i];
        }

        temp_cn[suffixArray[0]] = 0;
        classesCount = 1;

        for (int i = 1; i < n; i++) {
            int mid1 = (suffixArray[i] + (1 << h)) % n;
            int mid2 = (suffixArray[i - 1] + (1 << h)) % n;
            if (classesArray[suffixArray[i]] != classesArray[suffixArray[i - 1]] || classesArray[mid1] != classesArray[mid2]) {
                classesCount++;
            }
            temp_cn[suffixArray[i]] = classesCount - 1;
        }
        classesArray = temp_cn;
    }

    return suffixArray;
}

vector<int> initLCPArray(const string& str, vector<int> suffixArray) {
    int n = str.length();
    vector<int> lcp(MaxLength);
    vector<int> pos(MaxLength);

    for (int i = 0; i < n; i++) {
        pos[suffixArray[i]] = i;
    }

    int k = 0;

    for (int i = 0; i < n; i++) {
        if (k > 0) {
            k--;
        }

        if (pos[i] == n - 1) {
            lcp[n - 1] = -1;
            k = 0;
            continue;
        } else {
            int j = suffixArray[pos[i] + 1];
            while (max(i + k, j + k) < n && str[i + k] == str[j + k]) {
                k++;
            }
            lcp[pos[i]] = k;
        }
    }

    return lcp;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    string str;
    cin >> str;
    str += '$';
    int n = (int) str.length();
    vector<int> suffixArray, lcpArray;
    suffixArray = initSuffixArray(str);
    lcpArray = initLCPArray(str, suffixArray);

    for (int i = 1; i < n; i++) {
        cout << suffixArray[i] + 1 << " ";
    }
    cout << endl;
    for (int i = 1; i < n - 1; i++) {
        cout << lcpArray[i] << " ";
    }

    return 0;
}