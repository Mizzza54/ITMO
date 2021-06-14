#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>

using namespace std;

struct Task {
public:
    long long deadline;
    long long weight;

public:
    Task(long long deadline, long long weight) {
        this->deadline = deadline;
        this->weight = weight;
    }
};

auto TaskComparator = [](Task o1, Task o2) {
    if (o1.deadline == o2.deadline) {
        return o1.weight > o2.weight;
    } else {
        return o1.deadline > o2.deadline;
    }
};

auto WeightComparator = [](long long o1, long long o2) {
    return o1 > o2;
};

int main() {
    freopen("schedule.in", "r", stdin);
    freopen("schedule.out", "w", stdout);
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    int n = 0;
    cin >> n;

    priority_queue <Task, vector<Task>, decltype(TaskComparator)> queueTasks(TaskComparator);

    long long answer = 0;
    for (int i = 0; i < n; i++) {
        long long deadline = 0, weight = 0;
        cin >> deadline >> weight;
        if (deadline == 0) {
            answer += weight;
        } else {
            queueTasks.push(Task(deadline, weight));
        }
    }

    priority_queue <long long, vector<long long>, decltype(WeightComparator)> queueWeight(WeightComparator);


    long long timeCount = 0;
    while (!queueTasks.empty()) {
        Task task = queueTasks.top();
        queueTasks.pop();
        if (timeCount < task.deadline) {
            timeCount++;
        } else {
            answer += queueWeight.top();
            queueWeight.pop();
        }
        queueWeight.push(task.weight);
    }

    cout << answer;
}
