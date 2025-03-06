/*
Nagy Csongor, 513-as csoport, 2023-24_Lab6/07

Kijelentés
Egy online áruház 𝑛 terméket kínál eladásra és mindegyikre ismert a profit
mely az eladásából származik és a határidő, ameddig el lehet adni. Mivel az
áruháznak nagyon kevés alkalmazottja van, egy napon csak egy terméket tud
eladni. Határozzuk meg a maximális profitot amire az áruház szert tehet!
*/


#include <iostream>
#include <vector>
#include <algorithm>
#include <fstream>

using namespace std;

void input(int &n, vector<pair<int, int>>& a) {
    ifstream fin("test7.txt");
    fin >> n;
    for (int i = 0; i < n; i++) {
        int num1, num2;
        fin >> num1 >> num2;
        a.push_back(make_pair(num1, num2));
    }
}

bool compare(pair<int, int>& a, pair<int, int>& b) {
    if (a.first > b.first)
        return true;
    else if (a.first == b.first)
        return a.second > b.second;
    else
        return false;
}

int searchForMax(int n, vector<pair<int, int>> a) {
    int max = 0;
    for (int i = 0; i < n; i++) {
        if (a[i].second > max) {
            max = a[i].second;
        }
    }
    return max;
}

int solve(int n, vector<pair<int, int>>& a) {
    int size = searchForMax(n, a) + 1;
    vector<int> days(size, 0);
    int sum = 0;
    
    for (int i = 0; i < n; i++) {
        if (days[a[i].second] == 0) {
            days[a[i].second] = a[i].first;
        }
        else {
            for (int j = a[i].second; j > 0; j--) {
                if (days[j] == 0) {
                    days[j] = a[i].first;
                    break;
                }
            }
        }
    }


    for (int i = 0; i < size; i++) {
        sum += days[i];
    }

    return sum;
}

int main() {
    int n;
    ofstream fout("test7out.txt");

    vector<pair<int, int>> a;
    input(n, a);

    sort(a.begin(), a.end(), compare);

    fout << solve(n, a);

    return 0;
}
