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

    bool operator < (S const& rhs) const {
        int a = memcmp(first_punch, rhs.first_punch, sizeof(first_punch));
        if (a < 0) return true;
        if (a > 0) return false;
        return memcmp(mapping, rhs.mapping, sizeof(mapping)) < 0;
    }

    int distance() const {
        int used_bits = 0;
        int hole_count = 0;

        int lengths[26] = {};
        int lookup[26][128];
        for (int v = 0; v < 128; ++v)
            if (mapping[v] != -1) {
                lookup[mapping[v]][lengths[mapping[v]]++] = v;
                used_bits += __builtin_popcount(v);
            }

        for (int i = 0; i < 26; ++i)
            for (int j = 0;  j < 26; ++j) {
                int v_missing = 1;
                for (int vi = 0; vi < lengths[j]; ++vi) {
                    int v = lookup[j][vi];
                    if (first_punch[i] != -1 && ((first_punch[i] & v) != first_punch[i])) continue;
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

int main() {
    std::priority_queue<std::pair<int, S>, std::vector<std::pair<int, S> >, std::greater<std::pair<int, S> > > q;
    S start;
    q.push(make_pair(start.distance(), start));

    long ticks = 0;
    while (!q.empty()) {
        pair<int, S> top = q.top(); q.pop();

        if (!ticks--) {
            ticks = 10000;
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
