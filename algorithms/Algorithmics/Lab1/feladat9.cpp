/*
Kijelent�s
Szamitsuk ki, hany napot eltunk a mai nappal bezarolag! Ha valaki januar
1.-en szuletett, akkor januar 2.-an elt 1 napot.
*/

#include <iostream>

using namespace std;

bool isLeapYear(int year) {
    return (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
}

int daysInMonth(int year, int month) {
    int days[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    if (month == 2 && isLeapYear(year)) {
        return 29;
    }
    return days[month - 1];
}

int main() {
    int birthYear, birthMonth, birthDay, todayYear, todayMonth, todayDay;
    char dot;
    cin >> birthYear >> dot >> birthMonth >> dot >> birthDay;
    cin >> todayYear >> dot >> todayMonth >> dot >> todayDay;

    int days = 0;

    while (birthYear < todayYear || (birthYear == todayYear && birthMonth < todayMonth) || (birthYear == todayYear && birthMonth == todayMonth && birthDay < todayDay)) {
        days++;
        birthDay++;
        if (birthDay > daysInMonth(birthYear, birthMonth)) {
            birthDay = 1;
            birthMonth++;
        }
        if (birthMonth > 12) {
            birthMonth = 1;
            birthYear++;
        }
    }

    cout << days << endl;

    return 0;
}
