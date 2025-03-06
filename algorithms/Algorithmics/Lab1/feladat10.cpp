/*
Kijelent�s
�rjunk ki egy, a 60-as sz�mrendszerben megadott nemnegat�v sz�mot a 10-es
sz�mrendszerben! A 60-as sz�mrendszer sz�mjegyei: 0,1, . . . , 9, (10), (11), . . . ,
(59).
*/

#include <iostream>
#include <string>
#include <climits>

using namespace std;

int main() {
    string s;
    cin >> s;

    uint64_t num = 0;
    int base = 60;
    int index = 0;

    for (int i = 0; i < s.length(); i++) {
        uint64_t t = 0;

        if (s[i] == '(') {
            i++;
            while (s[i] != ')') {
                t = t * 10 + (s[i] - '0');
                i++;
            }
        }
        else {
            t = s[i] - '0';
        }

        if (num > (ULLONG_MAX - t) / base) {
            cout << "-1";
            return 0;
        }

        num = num * base + t;
        index++;
    }

    cout << num;

    return 0;
}
