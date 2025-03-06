/* Nagy Csongor, 513-s csoport
* Lab 7/04
Kijelentés
Adott a Hanoi-tornyok feladata, ahol az 1-es rúdról kell a 3-as rúdra át-
pakoljunk 𝑛 korongot, a 2-es rúd segítségével. Oldjuk meg a feladatot abban
az esetben, ha a 1-es rúdról a 3-as rudra nem szabad direkt módon korongot
mozgatni.
*/

#include <iostream>

using namespace std;

void towerOfHanoi(int n, int source, int auxiliary, int destination) {
    if (n == 1) {
        cout << source << " " << auxiliary << endl;
        cout << auxiliary << " " << destination << endl;
    } else {
        towerOfHanoi(n - 1, source, auxiliary, destination);
        
        cout << source << " " << auxiliary << endl;
        
        towerOfHanoi(n - 1, destination, auxiliary, source);
        
        cout << auxiliary << " " << destination << endl;
        
        towerOfHanoi(n - 1, source, auxiliary, destination);
    }
}
void input(int& n) {
    cin >> n;
}

int main() {
    int n;
    input(n);

    if (n == 0) return 0;

    towerOfHanoi(n, 1, 2, 3);

    return 0;
}
