#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <queue>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

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
    
    bool operator == (S const& rhs) const {
        return memcmp(first_punch, rhs.first_punch, sizeof(first_punch)) == 0 &&
               memcmp(mapping, rhs.mapping, sizeof(mapping)) == 0;
    }

    int distance() const {
        int used_values = 0;
        int hole_count = 0;

        int lengths[26] = {};
        int lookup[26][128];
        for (int v = 0; v < 128; ++v)
            if (mapping[v] != -1) {
                lookup[mapping[v]][lengths[mapping[v]]++] = v;
                ++used_values;
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

        return hole_count * 1000 + used_values;
    }
};

ostream& operator << (ostream& out, S const& rhs) {
    out << "distance = " << setbase(10) << rhs.distance() << endl;
    for (int i = 0; i < 26; ++i) {
        out << setw(0) << ('A'+i) << ": ";
        out << setw(7) << setbase(2);

        if (rhs.first_punch[i] == -1)
            out << "------- ";
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
    S start;

    int v_count = 0;
    for (int v = 0; v < 128 && v_count < 26; ++v) {
        if (__builtin_popcount(v) > 2) continue;
        start.first_punch[v_count] = v;
        start.mapping[v] = v_count;
        ++v_count;
    }

    std::priority_queue<std::pair<int, S>, std::vector<std::pair<int, S> >, std::greater<std::pair<int, S> > > q;
    q.push(make_pair(start.distance(), start));

    long ticks = 10000;
    while (!q.empty()) {
        pair<int, S> top = q.top(); q.pop();
        if (!ticks--) {
            start = top.second;
            break;
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

    cout << "start:" << endl;
    cout << start << endl;

    const int N = 10;
    std::vector<pair<int, S> > gen;
    gen.push_back(make_pair(start.distance(), start));
    S best = start;
    long ga_ticks = 0;
    for (;;) {

        if (!ga_ticks--) {
            ga_ticks = 100;
            cout << "best distance = " << best.distance() << endl;
            cout << best << endl;
        }

        std::vector<pair<int, S> > next_gen(gen);
        TR(gen, gen_it) {
            S const& current = gen_it->second;

            std::vector<std::pair<int, int> > unfilled;
            for (int i = 0; i < 26; ++i)
                for (int j = 0; j < 26; ++j) {

                    bool filled = false;
                    for (int v = 0; !filled && v < 128; ++v)
                        if ((current.first_punch[i] & v) == current.first_punch[i] && current.mapping[v] == j)
                            filled = true;

                    if (!filled)
                        unfilled.push_back(make_pair(i,j));
                }

            if (unfilled.empty()) {
                cout << "I WIN!" << endl;
                cout << current << endl;
                return 0;
            }

            pair<int, int> which = unfilled[rand()%unfilled.size()];
            int i = which.first;
            int j = which.second;

            // First, try for unmapped vs
            int unmapped_v = -1;
            int mapped_vs_length = 0;
            int mapped_vs[128];
            for (int v = 0; v < 128; ++v) {
                if ((current.first_punch[i] & v) != current.first_punch[i]) continue;
                if (current.mapping[v] == -1)
                    unmapped_v = v;
                else
                    mapped_vs[mapped_vs_length++] = v;
            }

            assert(mapped_vs_length);
            if (unmapped_v != -1) {
                S next(current);
                next.mapping[unmapped_v] = j;
                next_gen.push_back(make_pair(next.distance(), next));
            } else {
                S next(current);
                int v = mapped_vs[rand()%mapped_vs_length];
                next.mapping[v] = j;
                next_gen.push_back(make_pair(next.distance(), next));
            }
        }

        sort(ALL(next_gen));
        next_gen.erase(unique(ALL(next_gen)), next_gen.end());
        if (next_gen.size() > N)
            next_gen.erase(next_gen.begin()+N, next_gen.end());

        gen = next_gen;
        best = gen.front().second;
    }

    return 0;
}
