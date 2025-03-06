/* Nagy Csongor, 513-s csoport
* Lab 7/03
Kijelentés
Adott egy 𝑛 egész számot tartalmazó sorozat. Határozzuk meg azt a legna-
gyobb összeget, amelyet a tömb egymás utáni elemeinek összeadásával kaphatunk
(vagyis a legnagyobb összegű tömbszakaszt keressük).
*/


#include <iostream>
#include <vector>
#include <climits>

using namespace std;

int max(int a, int b) {
    return (a > b) ? a : b;
}

long findMaxCrossingSum(vector<long>& arr, long low, long mid, long high) {
    long leftSum = INT_MIN;
    long sum = 0;
    for (long i = mid; i >= low; --i) {
        sum += arr[i];
        if (sum > leftSum) {
            leftSum = sum;
        }
    }

    long rightSum = INT_MIN;
    sum = 0;
    for (long i = mid + 1; i <= high; ++i) {
        sum += arr[i];
        if (sum > rightSum) {
            rightSum = sum;
        }
    }

    return leftSum + rightSum;
}

long findMaxSubarraySum(vector<long>& arr, long low, long high) {
    if (low == high) {
        return arr[low];
    }

    long mid = (low + high) / 2;

    long leftSum = findMaxSubarraySum(arr, low, mid);
    long rightSum = findMaxSubarraySum(arr, mid + 1, high);
    long crossSum = findMaxCrossingSum(arr, low, mid, high);

    return max(max(leftSum, rightSum), crossSum);
}

void input(int& n, vector<long>& a) {
    cin >> n;

    for (int i = 0; i < n; i++) {
        long num;
        cin >> num;
        a.push_back(num);
    }
}

int main() {
    int n;
    vector<long> arr;
        
    input(n, arr);

    long result = findMaxSubarraySum(arr, 0, n - 1);

    cout << result << endl;

    return 0;
}
