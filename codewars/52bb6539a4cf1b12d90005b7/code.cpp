#include <set>
#include <vector>
using namespace std;

const multiset<size_t> expected_ships = {4, 3, 3, 2, 2, 2, 1, 1, 1, 1};

bool has_diagonals(vector<vector<int>> const& field) {
    for (int i = 0; i < 9; ++i) {
        for (int j = 0; j < 9; ++j) {
            if (field[i][j] && field[i+1][j+1]) return true;
            if (field[i+1][j] && field[i][j+1]) return true;
        }
    }
    return false;
}

size_t find_size_and_zero(vector<vector<int>>& field, int i, int j) {
    field[i][j] = 0;
    if (i < 9 && field[i+1][j]) return 1+find_size_and_zero(field, i+1, j);
    if (j < 9 && field[i][j+1]) return 1+find_size_and_zero(field, i, j+1);
    return 1;
}

multiset<size_t> find_ships(vector<vector<int>> const& field) {
    auto f = field;
    multiset<size_t> found;
    for (int i = 0; i < 10; ++i)
        for (int j = 0; j < 10; ++j)
            if (f[i][j])
                if (size_t sz = find_size_and_zero(f, i, j); sz > 0)
                    found.emplace(sz);
    return found;
}

bool validate_battlefield(vector<vector<int>> const& field) {
    return not has_diagonals(field) and find_ships(field) == expected_ships;
}
