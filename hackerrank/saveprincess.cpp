#include <iostream>
using namespace std;

int main() {
    int M, m_i = -1, m_j = -1, p_i = -1, p_j = -1;
    cin >> M;
    for (int i = 0; i < M; ++i)
        for (int j = 0; j < M; ++j) {
            char c;
            cin >> c;
            if (c == 'm')
                m_i = i, m_j = j;
            if (c == 'p')
                p_i = i, p_j = j;
        }

    int diff_i = m_i - p_i;
    int diff_j = m_j - p_j;

    for (; diff_i < 0; ++diff_i)
        cout << "DOWN" << endl;
    for (; diff_i > 0; --diff_i)
        cout << "UP" << endl;
    for (; diff_j < 0; ++diff_j)
        cout << "RIGHT" << endl;
    for (; diff_j > 0; --diff_j)
        cout << "LEFT" << endl;

    return 0;
}

