#include <iostream>
#include <vector>
#include <algorithm>
#include <deque>
using namespace std;

vector < vector<int> > graph;
deque <int> answer;

void swapSubQueue(int begin, int end) {
    int size = end - begin + 1;
    for (int i = 0; i * 2 < size; i++) {
        int temp = answer[i + begin];
        answer[i + begin] = answer[end - i];
        answer[end - i] = temp;
    }
}

void findHamiltonianCycle(int n) {
    for (int i = 0; i < n; i++) {
        answer.push_back(i);
    }

    for (int k = 0; k <= n * (n - 1); k++) {
        if (graph[answer[0]][answer[1]] == 0) {
            int i = 2;
            while (graph[answer[0]][answer[i]] == 0 || graph[answer[1]][answer[i + 1]] == 0) {
                i++;
            }
            swapSubQueue(1, i);
        }
        answer.push_back(answer.front());
        answer.pop_front();
    }
}



int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int n;
    string string;
    cin >> n;
    graph.resize(n, vector <int> (n, 0));

    for (int i = 1; i < n; i++) {
        cin >> string;
        for (int j = 0; j < int(string.length()); j++) {
            if (string[j] == '1') {
                graph[i][j] = 1;
                graph[j][i] = 1;
            } else {
                graph[i][j] = 0;
                graph[j][i] = 0;
            }
        }
    }
    findHamiltonianCycle(n);
    for (int i = 0; i < n; i++) {
        cout << answer[i] + 1 << " ";
    }
    return 0;
}