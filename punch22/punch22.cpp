#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <queue>
#include <utility>
#include <vector>
using namespace std;

struct S {
    signed char first_punch[26];
    signed char mapping[128];

    S() {
        memset(first_punch, -1, sizeof(first_punch));
        memset(mapping, -1, sizeof(mapping));
    }
    S(S const& rhs) {
        memcpy(first_punch, rhs.first_punch, sizeof(first_punch));
        memcpy(mapping, rhs.mapping, sizeof(mapping));
    }

    int distance() const {
        int hole_count = 0;
        int used_bits = 0;

        int lengths[26] = {};
        int lookup[26][128];
        for (int v = 0; v < 128; ++v)
            if (mapping[v] != -1)
                lookup[mapping[v]][lengths[mapping[v]]++] = v;

        for (int i = 0; i < 26; ++i)
            for (int j = 0;  j < 26; ++j) {
                int v_missing = 1;
                for (int vi = 0; vi < lengths[j]; ++vi) {
                    int v = lookup[j][vi];
                    if (first_punch[i] != -1 && ((first_punch[i] & v) != first_punch[i])) continue;
                    used_bits += __builtin_popcount(v);
                    v_missing = 0;
                }
                hole_count += v_missing;
            }

        return hole_count * 10000 + used_bits;
    }
};

ostream& operator << (ostream& out, S const& rhs) {
    out << "distance = " << setbase(10) << rhs.distance() << endl;
    for (int i = 0; i < 26; ++i) {
        out << setw(2) << i << ": ";
        out << setw(2) << setbase(16);

        if (rhs.first_punch[i] == -1)
            out << "-- ";
        else
            out << int(rhs.first_punch[i]) << " ";

        for (int v = 0; v < 128; ++v)
            if (v != rhs.first_punch[i])
                if (rhs.mapping[v] == i)
                    out << int(v) << " ";

        out << endl;
    }
    return out;
}

struct q_greater {
    bool operator () (pair<int, S> const& l, pair<int, S> const& r) const {
        return l.first > r.first;
    }
};

int main() {
    std::priority_queue<std::pair<int, S>, std::vector<std::pair<int, S> >, q_greater> q;
    S start;
    q.push(make_pair(start.distance(), start));

    long ticks = 0;
    while (!q.empty()) {
        pair<int, S> top = q.top(); q.pop();

        if (!ticks--) {
            ticks = 100;
            cout << top.second << endl;
        }

        if (top.first == 0) {
            cout << "FINISHED:" << endl;
            cout << top.second << endl;
        }

        for (int i = 0; i < 128; ++i)
            if (top.second.mapping[i] == -1)
                for (int v = 0; v < 26; ++v) {
                    S next(top.second);
                    next.mapping[i] = v;
                    if (next.first_punch[v] == -1)
                        next.first_punch[v] = i;

                    q.push(make_pair(next.distance(), next));
                }
    }

    return 0;
}
