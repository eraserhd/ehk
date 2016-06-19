#include <algorithm>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
using namespace std;

struct State {
    int tokens;
    char board[3][3];

    char winner() {
        for (int n = 0; n < 3; ++n) {
            if (board[n][0] == board[n][1] && board[n][1] == board[n][2] && board[n][2] != '_') return board[n][0];
            if (board[0][n] == board[1][n] && board[1][n] == board[2][n] && board[2][n] != '_') return board[0][n];
        }

        if (board[0][0] == board[1][1] && board[1][1] == board[2][2] && board[2][2] != '_') return board[0][0];
        if (board[2][0] == board[1][1] && board[1][1] == board[0][2] && board[0][2] != '_') return board[2][0];

        int blanks = 0;
        for (int i = 0; i < 3; ++i)
            for (int j = 0; j < 3; ++j)
                if (board[i][j] == '_')
                    ++blanks;

        if (!blanks)
            return 'O';

        return '_';
    }

    int check(int i, int j, int di, int dj, char who) {
        int b = (board[i][j] == '_') + (board[i+di][j+dj] == '_') + (board[i+2*di][j+2*dj] == '_');
        int w = (board[i][j] == who) + (board[i+di][j+dj] == who) + (board[i+2*di][j+2*dj] == who);
        return w >= 1 && b+w == 3;
    }

    pair<int, int> next_move(char who) {
        int bi = -1, bj = -1, bs = -1;

        for (int i = 0; i < 3; ++i)
            for (int j = 0; j < 3; ++j) {
                if (board[i][j] != '_') continue;
                board[i][j] = who;

                int s = 0;

                for (int n = 0; n < 3; ++n) {
                    s += check(n,0,0,1,who);
                    s += check(0,n,1,0,who);
                }

                s += check(0,0,+1,+1,who);
                s += check(2,0,-1,+1,who);

                board[i][j] = '_';

                if (s > bs)
                    bs = s, bi = i, bj = j;
            }
        return make_pair(bi,bj);
    }

    void fill_in_next_move(char who) {
        pair<int, int> b = next_move(who);
        board[b.first][b.second] = who;
    }
};

ostream& operator << (ostream& o, State const& s) {
    o << "State{ tokens = " << s.tokens << "; board = \"";
    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j)
            o << s.board[i][j];

    o << "\" }";
    return o;
}

int encode(State const& s) {
    int e = s.tokens;
    int m = 9;

    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j) {
            int v;
            if (s.board[i][j] == '_')
                v = 0;
            else if (s.board[i][j] == 'X')
                v = 1;
            else
                v = 2;

            e += m*v;
            m *= 3;
        }

    return e;
}

State decode(int e) {
    State s;
    s.tokens = e%9;
    e/=9;

    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j) {
            switch (e%3) {
            case 0: s.board[i][j] = '_'; break;
            case 1: s.board[i][j] = 'X'; break;
            case 2: s.board[i][j] = 'O'; break;
            }
            e/=3;
        }

    return s;
}

double memo[177147];
int best_bid[177147];

double compute(int e) {
    double& result = memo[e];
    if (result == -1.0) {
        State s = decode(e);

        //cout << s << endl;

        char w = s.winner();
        if ('X' == w)
            return result = 1.0;
        if ('O' == w)
            return result = 0.0;

        int bet_count = ((8 - s.tokens) + 1);
        double best_p = 0;

        for (int x_bet = s.tokens ? 1 : 0; x_bet <= s.tokens; ++x_bet) {
            double p = 0.;
            for (int o_bet = (8 - s.tokens) ? 1 : 0; o_bet <= (8 - s.tokens); ++o_bet) {
                State ns(s);

                if (x_bet < o_bet) {
                    ns.tokens += o_bet;
                    ns.fill_in_next_move('O');
                } else {
                    ns.tokens -= x_bet;
                    ns.fill_in_next_move('X');
                }

                p += compute(encode(ns));
            }

            p /= (8 - s.tokens + 1);
            if (p > best_p) {
                best_p = p;
                best_bid[e] = x_bet;
            }
        }

        return result = best_p; 
    }
    return result;
}

void fill_in_state_table() {
    State s;
    s.tokens = 4;
    memset(s.board, '_', sizeof(s.board));
    fill(memo, memo+sizeof(memo)/sizeof(memo[0]), -1.0);
    compute(encode(s));
}

vector<int> parse_string(string const& s) {
    istringstream in(s);
    vector<int> result;
    int n;
    while (in >> n)
        result.push_back(n);
    return result;
}


int main() {
    fill_in_state_table();

    char who;
    string what;
    string x_bids, o_bids;
    State s;

    cin >> who >> what;
    cin.ignore();
    getline(cin,x_bids);
    getline(cin,o_bids);

    vector<int> xb = parse_string(x_bids);
    vector<int> ob = parse_string(o_bids);

    s.tokens = 4;
    for (int i = 0; i < xb.size(); ++i) {
        if (xb[i] >= ob[i])
            s.tokens -= xb[i];
        else
            s.tokens += ob[i];
    }

    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j)
            cin >> s.board[i][j];

    if (what == "BID") {
        if (who == 'O') {
            s.tokens = 8 - s.tokens;
            for (int i = 0; i < 3; ++i)
                for (int j = 0; j < 3; ++j)
                    if (s.board[i][j] != '_')
                        s.board[i][j] = (s.board[i][j] == 'X' ? 'O' : 'X');
        }
        //cout << s << endl;
        compute(encode(s));
        cout << best_bid[encode(s)] << endl;
    } else {
        pair<int, int> move = s.next_move(who);
        cout << move.first << " " << move.second << endl;
    }

    return 0;
}

