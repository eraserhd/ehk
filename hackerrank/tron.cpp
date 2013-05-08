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

const int LP_SIZE = 20;
int LP_TABLE[15][15];
pair<int, int> LP_REVERSE[LP_SIZE+1];

bool fill_LP_TABLE() {
    memset(LP_TABLE,-1,sizeof(LP_TABLE));
    int count = 0;
    queue<pair<int, int> > q;
    q.push(positions[me]);
    while (!q.empty()) {
        pair<int, int> t = q.front(); q.pop();
        for (int d = 0; d < 4; ++d) {
            pair<int, int> next = move(t, d);
            if (!empty(next))
                continue;
            if (LP_TABLE[next.first][next.second] != -1)
                continue;
            LP_TABLE[next.first][next.second] = count;
            LP_REVERSE[count] = next;
            ++count;
            q.push(next);
        }
    }
    return count < LP_SIZE;
}

int next_move_for_longest_path() {
    static unsigned dp[1<<LP_SIZE];
    static unsigned char from[1<<LP_SIZE];
    memset(dp, 0, sizeof(dp));
    memset(from, 0, sizeof(from));

    queue<int> q;

    for (int d = 0; d < 4; ++d) {
        pair<int, int> first = move(positions[me], d);
        if (!empty(first))
            continue;
        int bit = LP_TABLE[first.first][first.second];
        dp[bit] |= 1<<bit;
        from[bit] |= 1<<d;
        q.push(bit);
    }

    while (!q.empty()) {
        int t = q.front(); q.pop();
        for (int x = dp[t]; x; x^=(x&-x)) {
            int n = __builtin_ctz(x);
            pair<int, int> at = LP_REVERSE[n];
            for (int d = 0; d < 4; ++d) {
                pair<int, int> to = move(at, d);
                if (!empty(to))
                    continue;
                int to_n = 1<<LP_TABLE[to.first][to.second];
                if (n&to_n) continue; // Already seen before getting here
                if (dp[n|to_n] & to_n) continue; // Already seen next step

                dp[n|to_n] |= to_n;
                from[n|to_n] |= from[n];
                q.push(n|to_n);
            }
        }
    }

    int best = -1;
    int best_popcount = 0;
    for (int i = 0; i < sizeof(dp)/sizeof(dp[0]); ++i)
        if (__builtin_popcount(dp[i]) > best_popcount) {
            best_popcount = __builtin_popcount(dp[i]);
            best = i;
        }

    return __builtin_ctz(from[best]);
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
        if (fill_LP_TABLE())
            cout << DN[next_move_for_longest_path()] << endl;
        else if (empty(move(positions[me], RIGHT)))
            cout << DN[RIGHT] <<endl;
        else if (empty(move(positions[me], LEFT)))
            cout << DN[LEFT] << endl;
        else if (positions[me].first > 7)
            cout << DN[DOWN] << endl;
        else
            cout << DN[UP] << endl;
    }

    return 0;
}

