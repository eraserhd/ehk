#include <cstring>
#include <iostream>
#include <queue>
#include <string>
#include <utility>
using namespace std;

string W;
int T, N;
int E[26][26];
int entrances[26];
int exits[26];

bool can_be_opened() {
    int special_start_node = -1;
    int special_end_node = -1;

    for (int i = 0; i < 26; ++i) {
        if (entrances[i] == exits[i])
            continue;
        if (entrances[i]-1 == exits[i]) {
            if (special_end_node != -1)
                return false;
            special_end_node = i;
            continue;
        }
        if (entrances[i] == exits[i]-1) {
            if (special_start_node != -1)
                return false;
            special_start_node = i;
            continue;
        }
        return false;
    }

    bool seen[26] = {};
    queue<int> q;

    if (special_start_node != -1) {
        q.push(special_start_node);
        seen[special_start_node] = true;
    } else {
        for (int i = 0; i < 26; ++i)
            if (exits[i] > 0) {
                q.push(i);
                seen[i] = true;
                break;
            }
    }

    while (!q.empty()) {
        int p = q.front(); q.pop();

        for (int j = 0; j < 26; ++j)
            if (E[p][j] && !seen[j]) {
                seen[j] = true;
                q.push(j);
            }
    }

    for (int i = 0; i < 26; ++i)
        if (!seen[i] && (entrances[i] > 0 || exits[i] > 0)) {
            return false;
        }

    return true;
}

int main() {
    cin >> T;
    for (int t = 0; t < T; ++t) {
        memset(E, 0, sizeof(E));
        memset(entrances, 0, sizeof(entrances));
        memset(exits, 0, sizeof(exits));

        cin >> N;
        for (int n = 0; n < N; ++n) {
            cin >> W;

            int from = W[0] - 'a';
            int to = W[W.size()-1] - 'a';
            ++E[from][to];
            ++entrances[to];
            ++exits[from];
        }

        if (can_be_opened())
            cout << "Ordering is possible." << endl;
        else
            cout << "The door cannot be opened." << endl;
    }

    return 0;
}

