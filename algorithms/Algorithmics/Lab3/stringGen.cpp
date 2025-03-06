#include <iostream>
#include <cstdlib>
#include <ctime>
#include <string>

using namespace std;

int main() {
    // Seed for random number generation
    srand(static_cast<unsigned int>(time(0)));

    // Generate a string of 10^5 characters (1-9)
    const int length = 100000;
    string randomString;

    for (int i = 0; i < length; ++i) {
        int digit = rand() % 9 + 1;  // Generate a random digit between 1 and 9
        randomString += to_string(digit);
    }

    // Print the generated string
    cout << randomString << endl;

    return 0;
}
