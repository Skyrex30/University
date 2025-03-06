/*
Kijelentés
Adottak az elsőéves egyetemisták névsorai (abc sorrendben), csoportonként.
Állítsunk elő ezekből egyetlen névsort, mely szintén abc sorrendben van
*/

#include <iostream>
#include <string>
#include <vector>

using namespace std;

void input(vector<vector<string>>& names) {
    int groups = 0;
    cin >> groups;

    for (int i = 0; i < groups; i++) {
        int numberOfStudents = 0;
        cin >> numberOfStudents;
        vector<string> group;

        for (int j = 0; j < numberOfStudents; j++) {
            string name;
            int length;
            cin >> length;
            getline(cin >> ws, name);
            group.push_back(name);
        }
        names.push_back(group);
    }
}

void mergeSort(vector<string> a, vector<string> b, vector<string> &c) {
    int i = 0, j = 0;

    while (i < a.size() && j < b.size()) {
    
        if (a[i] <= b[j]) {
            c.push_back(a[i]);
            i++;
        }
        else {
            c.push_back(b[j]);
            j++;
        }
    }

    while (i < a.size()) {
        c.push_back(a[i]);
        i++;
    }

    while (j < b.size()) {
        c.push_back(b[j]);
        j++;
    }
}
void sortNames(vector<vector<string>> names, vector<string>& sortedNames) {
    if (names.size() > 0) {
        sortedNames = names[0];
    }

    for (int i = 1; i < names.size(); i++) {
        vector<string> temp;
        mergeSort(sortedNames, names[i], temp);
        vector<string> swap = sortedNames;
        sortedNames = temp;
        temp = swap;    
    }
}

void output(vector<string> sortedNames) {
    for (int i = 0; i < sortedNames.size(); ++i) {
        cout << sortedNames[i] << endl;
    }
}

int main()
{
    vector<vector<string>> names;
    input(names);

    vector<string> sortedNames;
    sortNames(names, sortedNames);

    output(sortedNames);

    return 0;
}