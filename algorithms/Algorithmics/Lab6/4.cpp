#include <iostream>
#include <vector>

using namespace std;

void countSort(vector<pair<long long, long long>>& arr, long n, long exp) {
    const int range = 10;
    vector<pair<long long, long long>> output(n);
    vector<int> count(range, 0);

    for (long long i = 0; i < n; i++) {
        count[(arr[i].first / exp) % range]++;
    }

    for (int i = 1; i < range; i++) {
        count[i] += count[i - 1];
    }

    for (long long i = n - 1; i >= 0; i--) {
        output[count[(arr[i].first / exp) % range] - 1] = arr[i];
        count[(arr[i].first / exp) % range]--;
    }

    for (long long i = 0; i < n; i++) {
        arr[i] = output[i];
    }
}

void radixSort(vector<pair<long long, long long>>& arr) {
    long long n = arr.size();
    long long maxFirst = arr[0].first;


    for (long long i = 1; i < n; i++) {
        if (arr[i].first > maxFirst) {
            maxFirst = arr[i].first;
        }
    }

    for (long long exp = 1; maxFirst / exp > 0; exp *= 10) {
        countSort(arr, n, exp);
    }
}

void input(long n, vector<pair<long long, long long>>& arr) {
    for (long i = 0; i < n; i++) {
        long a, b;
        cin >> a >> b;
        arr.push_back(make_pair(a, b));
    }
}
bool solve(long long n, long long m, vector<pair<long long, long long>>& arr) {
    long long db = 0;
    for (long i = 0; i < m; i++) {
        if (arr[i].first == 0) break;
        for (long j = i; j < m; j++) {
            if (arr[j].first > arr[i].second) {
                arr[i].second = arr[j].second;
                arr[j].first = 0;
                arr[j].second = 0;
            }
        }
    }
    for (long i = 0; i < m; i++) {
        if (arr[i].first != 0) db++;
    }
    if (db <= n) {
        return true;
    }
    return false;
}

int main() {
    long long n, m;
    cin >> n >> m;
    if (m == 0) {
        cout << "igen";
        return 0;
    }
    vector<pair<long long, long long>> arr;

    input(m, arr);

    radixSort(arr);

    bool igen = solve(n, m, arr);
    if (igen == true) cout << "igen";
    else cout << "nem";

    return 0;
}
