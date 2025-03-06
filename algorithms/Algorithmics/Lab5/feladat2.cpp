#include <iostream>
#include <vector>
#include <unordered_set>

using namespace std;

void backtrack(vector<string>& path, const vector<vector<string>>& friends, unordered_set<string>& visited, int n) {
    if (path.size() == n) {
        for (const string& person : path) {
            cout << person << " ";
        }
        cout << path[0] << endl;
        return;
    }

    string current_person = path.back();

    for (const string& friend_person : friends[current_person]) {
        if (visited.find(friend_person) == visited.end()) {
            path.push_back(friend_person);
            visited.insert(friend_person);

            backtrack(path, friends, visited, n);

            path.pop_back();
            visited.erase(friend_person);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;

    vector<vector<string>> friends(n);
    unordered_set<string> all_people;

    for (int i = 0; i < m; ++i) {
        string person1, person2;
        cin >> person1 >> person2;

        friends[person1].push_back(person2);
        friends[person2].push_back(person1);

        all_people.insert(person1);
        all_people.insert(person2);
    }

    for (const string& person : all_people) {
        vector<string> path = { person };
        unordered_set<string> visited = { person };
        backtrack(path, friends, visited, n);
    }

    return 0;
}
