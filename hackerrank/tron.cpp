#include <algorithm>
#include <cstring>
#include <iostream>
#include <limits.h>
#include <map>
#include <queue>
#include <utility>
#include <vector>
using namespace std;

char me;
char other_player;
pair<int, int> r_pos;
pair<int, int> g_pos;

char board[15][16];

const int DI[] = {-1,1,0,0};
const int DJ[] = {0,0,-1,1};
const char *DN[] = {"UP","DOWN","LEFT","RIGHT"};

int main() {
    cin >> me;
    cin >> r_pos.first >> r_pos.second;
    cin >> g_pos.first >> g_pos.second;

    for (int i = 0; i < 15; ++i)
        for (int j = 0; j < 15; ++j)
            cin >> board[i][j];

    board[r_pos.first][r_pos.second] = 'r';
    board[g_pos.first][g_pos.second] = 'g';
    other_player = me == 'r' ? 'g' : 'r';

    // Avoid draws
    pair<int, int> other_pos = other_player == 'r' ? r_pos : g_pos;
    for (int d = 0; d < 4; ++d) {
        char& cell = board[other_pos.first + DI[d]][other_pos.second + DJ[d]];
        if (cell != 'r' && cell != 'g' && cell != '#')
            cell = other_player;
    }

    pair<int, int> my_pos = me == 'r' ? r_pos : g_pos;
    bool ok[4] = {true,true,true,true};
    int size[4];
    for (int d = 0; d < 4; ++d) {
        pair<int, int> start_pos = make_pair(my_pos.first + DI[d], my_pos.second + DJ[d]);
        if (board[start_pos.first][start_pos.second] != '-') {
            ok[d] = false;
            continue;
        }

        bool seen[15][15] = {};
        queue<pair<int, int> > q;
        seen[start_pos.first][start_pos.second] = true;
        q.push(start_pos);
        size[d] = 1;
        while (!q.empty()) {
            pair<int, int> t = q.front(); q.pop();
            for (int dd = 0; dd < 4; ++dd) {
                pair<int, int> nt = make_pair(t.first+DI[dd], t.second+DJ[dd]);
                if (seen[nt.first][nt.second])
                    continue;
                if (board[nt.first][nt.second] != '-')
                    continue;
                ++size[d];
                seen[nt.first][nt.second] = true;
                q.push(nt);
            }
        }
    }

    int best_size = -1;
    int best_d = -1;
    for (int d = 0; d < 4; ++d) {
        if (!ok[d])
            continue;
        if (best_size < size[d]) {
            best_size = size[d];
            best_d = d;
        }
    }

    if (best_d == -1)
        cout << "LEFT" << endl;
    else
        cout << DN[best_d] << endl;
    return 0;
}

