#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <limits.h>
#include <map>
#include <queue>
#include <utility>
#include <vector>
using namespace std;

const int RED_PLAYER = 0;
const int GREEN_PLAYER = 1;
const int EMPTY = 2;
const char COLORS[2] = {'r', 'g'};
const int DI[] = {-1,1,0,0};
const int DJ[] = {0,0,-1,1};
const int UP = 0;
const int DOWN = 1;
const int LEFT = 2;
const int RIGHT = 3;
const char *DN[] = {"UP","DOWN","LEFT","RIGHT"};

int me;
pair<int, int> positions[2];
char board[15][16] = {};

void read_state() {
    char me_char;
    cin >> me_char;
    me = me_char == 'r' ? RED_PLAYER : GREEN_PLAYER;
    cin >> positions[0].first >> positions[0].second;
    cin >> positions[1].first >> positions[1].second;

    for (int i = 0; i < 15; ++i)
        for (int j = 0; j < 15; ++j)
            cin >> board[i][j];

    board[positions[0].first][positions[0].second] = 'r';
    board[positions[1].first][positions[1].second] = 'g';
}

bool in_initial_faceoff() {
    for (int i = 0; i < 15; ++i)
        for (int j = 0; j < 15; ++j)
            if (i != positions[me].first && board[i][j] == COLORS[me])
                return false;
    return true;
}

int initial_faceoff_direction() {
    switch (me) {
    case RED_PLAYER: return RIGHT;
    case GREEN_PLAYER: return LEFT;
    }
}

pair<int, int> move(pair<int, int> const& from, int d) {
    return make_pair(from.first + DI[d], from.second + DJ[d]);
}

char at(pair<int, int> const& p) {
    if (p.first < 0 || p.first >= 15) return '#';
    if (p.second < 0 || p.second >= 15) return '#';
    return board[p.first][p.second];
}

bool empty(pair<int, int> const& p) {
    return at(p) == '-';
}

int main() {
    read_state();

    if (in_initial_faceoff()) {
        int d = initial_faceoff_direction();
        pair<int, int> new_position = move(positions[me], d);
        if (empty(new_position) && (abs(new_position.first-positions[!me].first) + abs(new_position.second-positions[!me].second) > 1))
            cout << DN[d] << endl;
        else {
            if (positions[!me].first < positions[me].first)
                cout << DN[DOWN] << endl;
            else
                cout << DN[UP] <<endl;
        }
    } else {
        int best_d = 0;
        int best_score = -1;

        for (int d = 0; d < 4; ++d) {
            pair<int, int> at = move(positions[me], d);
            if (!empty(at))
                continue;

            int neighbors = 0;
            for (int d2 = 0; d2 < 4; ++d2) {
                pair<int, int> t = move(at, d2);
                if (!empty(t))
                    ++neighbors;
            }

            board[at.first][at.second] = '*';

            int reachable = 0;
            queue<pair<int, int> > q;
            bool seen[15][15] = {};
            seen[at.first][at.second] = true;
            q.push(at);
            while (!q.empty()) {
                pair<int, int> t = q.front(); q.pop();
                for (int d = 0; d < 4; ++d) {
                    pair<int, int> nt = move(t, d);
                    if (!empty(nt))
                        continue;
                    if (seen[nt.first][nt.second])
                        continue;
                    seen[nt.first][nt.second] = true;
                    ++reachable;
                    q.push(nt);
                }
            }

            board[at.first][at.second] = '-';

            int score = reachable * 10 + neighbors;
            if (score > best_score) {
                best_d = d;
                best_score = score;
            }
        }

        cout << DN[best_d] <<endl;
    }

    return 0;
}

