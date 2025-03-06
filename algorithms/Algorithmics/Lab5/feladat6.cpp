#include <iostream>
#include <vector>

using namespace std;

int dx[] = { 2, 1, -1, -2, -2, -1, 1, 2 };
int dy[] = { 1, 2, 2, 1, -1, -2, -2, -1 };

bool is_attacked_by_bishop(int x, int y, int bishop_x, int bishop_y) {
    return abs(x - bishop_x) == abs(y - bishop_y);
}

bool is_valid(int x, int y, int n, vector<vector<bool>>& visited) {
    return x >= 1 && x <= n && y >= 1 && y <= n && !visited[x][y];
}

void count_knight_paths(int x, int y, int target_x, int target_y, int bishop_x, int bishop_y, int n, vector<vector<bool>>& visited, int& path_count) {
    if (x == target_x && y == target_y) {
        path_count++;
        return;
    }

    for (int i = 0; i < 8; i++) {
        int new_x = x + dx[i];
        int new_y = y + dy[i];
        if (is_valid(new_x, new_y, n, visited) && !is_attacked_by_bishop(new_x, new_y, bishop_x, bishop_y)) {
            visited[new_x][new_y] = true;
            count_knight_paths(new_x, new_y, target_x, target_y, bishop_x, bishop_y, n, visited, path_count);
            visited[new_x][new_y] = false;
        }
    }
}

int main() {
    int n;
    int bishop_x, bishop_y;
    int knight_x, knight_y;
    int target_x, target_y;

    cin >> n >> bishop_x >> bishop_y >> knight_x >> knight_y >> target_x >> target_y;

    vector<vector<bool>> visited(n + 1, vector<bool>(n + 1, false));

    int path_count = 0;

    visited[knight_x][knight_y] = true;

    count_knight_paths(knight_x, knight_y, target_x, target_y, bishop_x, bishop_y, n, visited, path_count);

    cout << path_count << endl;

    return 0;
}
