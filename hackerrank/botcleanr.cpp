#include <iostream>
using namespace std;

int main() {
    int b_i, b_j, d_i = -1, d_j = -1;
    cin >> b_i >> b_j;
    for (int i = 0; i < 5; ++i)
        for (int j = 0; j < 5; ++j) {
            char c;
            cin >> c;
            if (c == 'd')
                d_i = i, d_j = j;
        }

    int diff_i = b_i - d_i;
    int diff_j = b_j - d_j;

    if (diff_i == 0 && diff_j == 0)
        cout << "CLEAN" << endl;
    else if (diff_i < 0)
        cout << "DOWN" << endl;
    else if (diff_i > 0)
        cout << "UP" << endl;
    else if (diff_j < 0)
        cout << "RIGHT" << endl;
    else if (diff_j > 0)
        cout << "LEFT" <<endl;

    return 0;
}

