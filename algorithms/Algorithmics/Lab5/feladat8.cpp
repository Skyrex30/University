#include <iostream>
#include <string>
#include <vector>

using namespace std;

struct Names{
	string name;
	int num;
};

int searchForName(string s, Names* names, int n) {
	for (int i = 0; i < n; i++) {
		if (names[i].name == s) return i;
	}
	return -1;
}

void output(int n, vector<string> namesOfBorrowers) {
	for(int i = 0; i < namesOfBorrowers.size(); i++){
		cout << namesOfBorrowers[i] << " ";
	}
	cout << "Laci";
}

bool checkFriends(int i, int start, vector<vector<int>> people) {
	for (int j = 0; j < people[i].size(); j++) {
		if (people[i][j] == 1 && j != start) return false;
	}
	return true;
}

void backtrack(int i, int start, Names* names, int n, int count, vector<vector<int>>& people, vector<bool>& borrowed, vector<string>& namesOfBorrowers) {
	if (borrowed[i]) { 
		return;
	}

	namesOfBorrowers.push_back(names[i].name); 
	borrowed[i] = true; 
	count++;

	if (count == n || (checkFriends(i, start, people) && people[i][start] == 1)) {
		output(n, namesOfBorrowers); 
		cout << endl; 
	}
	
	else {
		for (int j = 0; j < n; j++) {
			if (people[i][j] == 1 && !borrowed[j]) {
				backtrack(j, start, names, n, count, people, borrowed, namesOfBorrowers);
			}
		}
	}

	borrowed[i] = false;
	count--;
	namesOfBorrowers.pop_back();
}


int main() {
	int n, m;
	cin >> n >> m;
	vector<vector<int>> people(n, vector<int>(n, 0));
	vector<bool> borrowed(n, false);
	vector<string> namesOfBorrowers;

	Names* nums = new Names[n];
	int db = -1;

	for (int i = 0; i < m; i++) {
		string s1, s2;
		cin >> s1 >> s2;
		if (searchForName(s1, nums, n) == -1) {
			db++;
			nums[db].name = s1;
			nums[db].num = db;
		}
		if (searchForName(s2, nums, n) == -1) {
			db++;
			nums[db].name = s2;
			nums[db].num = db;
		}
		int index1 = searchForName(s1, nums, n);
		int index2 = searchForName(s2, nums, n);

		people[index1][index2] = people[index2][index1] = 1;
	}
	int indexOfLaci = searchForName("Laci", nums, n);
	if (indexOfLaci == -1) return 0;
	backtrack(indexOfLaci, indexOfLaci, nums, n, 0, people, borrowed, namesOfBorrowers);

	delete[] nums;

	return 0;
}