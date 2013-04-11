#include <iostream>
#include <queue>
#include <set>
#include <string>
#include <utility>
using namespace std;

const int I_DELTA[] = {-1, 1, 0, 0};
const int J_DELTA[] = { 0, 0,-1, 1};

struct State {
    unsigned dirty : 25;
    unsigned extra : 7;

    State(unsigned i, unsigned j, unsigned dirty, unsigned first_move = 4)
        : dirty(dirty)
        , extra(first_move*5*5+i*5+j)
    {}

    inline string first_move() const {
        switch (extra/(5*5)) {
        case 0: return "UP";
        case 1: return "DOWN";
        case 2: return "LEFT";
        case 3: return "RIGHT";
        case 4: return "";
        }
    }

    int i() const {
        return extra/5%5; 
    }

    int j() const {
        return extra%5;
    }

    bool operator == (State const& rhs) const {
        if (dirty != rhs.dirty) return false;
        return extra%(5*5) == rhs.extra%(5*5);
    }

    bool operator < (State const& rhs) const {
        if (dirty < rhs.dirty) return true;
        if (dirty > rhs.dirty) return false;
        return (extra%(5*5)) < (rhs.extra%(5*5));
    }

    bool can_move(int d) {
        int n_i = i() + I_DELTA[d];
        int n_j = j() + J_DELTA[d];
        return (n_i >= 0 && n_j >= 0 && n_i < 5 && n_j < 5);
    }

    State move(int d) {
        int n_i = i() + I_DELTA[d];
        int n_j = j() + J_DELTA[d];
        unsigned n_dirty = dirty;
        n_dirty &= ~(1<<(n_i*5+n_j));
        unsigned n_move = extra/5/5;
        if (n_move == 4)
            n_move = d;
        return State(n_i, n_j, n_dirty, n_move);
    }
};

int main() {
    int b_i, b_j;
    cin >> b_i >> b_j;

    unsigned initial_dirty = 0;
    for (int i = 0; i < 5; ++i)
        for (int j = 0; j < 5; ++j) {
            char c;
            cin >> c;

            if (i == b_i && j == b_j && c == 'd') {
                cout << "CLEAN" << endl;
                return 0;
            }

            if (c == 'd')
                initial_dirty |= 1<<(i*5+j);
        }

    State initial(b_i, b_j, initial_dirty);
    std::queue<State> q;
    std::set<State> seen;

    seen.insert(initial);
    q.push(initial);
    while (!q.empty()) {
        State s = q.front(); q.pop();
        if (s.dirty == 0) {
            cout << s.first_move() << endl;
            return 0;
        }

        for (int d = 0; d < 4; ++d) {
            if (!s.can_move(d)) continue;
            State n_s = s.move(d);
            if (seen.count(n_s) == 0) {
                seen.insert(n_s);
                q.push(n_s);
            }
        }
    }

    return 0;
}

