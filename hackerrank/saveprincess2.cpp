#include <iostream>
using namespace std;

int main() {
    int M, m_i, m_j, p_i = -1, p_j = -1;
    cin >> M >> m_i >> m_j;
    for (int i = 0; i < M; ++i)
        for (int j = 0; j < M; ++j) {
            char c;
            cin >> c;
            if (c == 'p')
                p_i = i, p_j = j;
        }

    int diff_i = m_i - p_i;
    int diff_j = m_j - p_j;

    if (diff_i < 0)
        cout << "DOWN" << endl;
    else if (diff_i > 0)
        cout << "UP" << endl;
    else if (diff_j < 0)
        cout << "RIGHT" << endl;
    else if (diff_j > 0)
        cout << "LEFT" <<endl;

    return 0;
}

