#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

vector <vector <int>> Transfers, ReverseTransfers;
vector<int> FinalStates, topsort;
vector <bool> useful;
enum cl {white, gray, black};
vector <cl> colors;
int sizeDFA = 0;

void dfsUseful(int v) {
    useful[v] = true;
    for (int i = 0; i < ReverseTransfers[v].size(); i++) {
        if (!useful[ReverseTransfers[v][i]]) {
            dfsUseful(ReverseTransfers[v][i]);
        }
    }
}

bool dfsCycles(int v) {
    colors[v] = gray;
    for (int i = 0; i < Transfers[v].size(); i++) {
        int cur = Transfers[v][i];
        if (colors[cur] == white) {
            if (dfsCycles(cur)) {
                return true;
            }
        } else if (useful[cur] && colors[cur] == gray) {
            return true;
        }
    }
    colors[v] = black;
    topsort.push_back(v);
    return false;
}

int paths() {
    reverse(topsort.begin(), topsort.end());

    int answer = 0;
    vector<int> paths(sizeDFA, 0);
    paths[0] = 1;
    for(int i = 0; i < topsort.size(); i++) {
        int cur = topsort[i];
        for (size_t j = 0; j < ReverseTransfers[cur].size(); j++) {
            paths[cur] = (paths[cur] + paths[ReverseTransfers[cur][j]]) % 1000000007;
        }
    }

    for(int i = 0; i < FinalStates.size(); i++) {
        answer = (answer + paths[FinalStates[i]]) % 1000000007;
    }
    return answer;
}

int getAns() {
    useful.assign(sizeDFA, false);
    colors.assign(sizeDFA, white);

    for (int i: FinalStates) {
        dfsUseful(i);
    }

    if (dfsCycles(0)) {
        return -1;
    } else {
        return paths();
    }
}

int main() {
    freopen("problem3.in", "r", stdin);
    freopen("problem3.out", "w", stdout);

    int n, m, k;
    cin >> n >> m >> k;
    sizeDFA = n;

    for(int i = 0, a; i < k; i++) {
        cin >> a;
        FinalStates.push_back(--a);
    }

    Transfers.resize(sizeDFA);
    ReverseTransfers.resize(sizeDFA);

    for(int i = 0, a, b; i < m; i++) {
        char ch;
        cin >> a >> b >> ch;
        a--;
        b--;
        Transfers[a].push_back(b);
        ReverseTransfers[b].push_back(a);
    }

    fclose(stdin);

    cout << getAns() << endl;
    fclose(stdout);
    return 0;
}