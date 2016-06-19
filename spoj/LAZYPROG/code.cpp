#include <algorithm>
#include <cstdio>
#include <iostream>
#include <limits.h>
#include <map>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()

struct C {
    C() : a(0), b(0), d(0), paid_for(0) {}
    int a, b, d, paid_for;
};

vector<C> cs;

bool process_order_less(C const& lhs, C const& rhs) {
    if (lhs.d < rhs.d) return true;
    if (lhs.d > rhs.d) return false;
    return lhs.a > rhs.a;
}

int gcd(int a, int b) {
    return b ? gcd(b, a%b) : a;
}

double solve() {
    sort(ALL(cs), process_order_less);

    int total_b = 0; 
    multimap<int, int> cs_by_a;

    for (int c = 0; c < cs.size(); ++c) {
        cs_by_a.insert(make_pair(cs[c].a, c));
        total_b += cs[c].b;

        if (total_b <= cs[c].d) continue;

        int t_needed = total_b - cs[c].d;
        while (t_needed) {
            int c_to_adjust = cs_by_a.rbegin()->second;
            int adjust_by = min(cs[c_to_adjust].b - cs[c_to_adjust].paid_for, t_needed);

            t_needed -= adjust_by;
            total_b -= adjust_by;

            cs[c_to_adjust].paid_for += adjust_by;
            if (cs[c_to_adjust].paid_for == cs[c_to_adjust].b)
                cs_by_a.erase(--cs_by_a.end());
        }
    }

    double r = 0.;
    for (int c = 0; c < cs.size(); ++c) {
        if (!cs[c].paid_for) continue;

        r += double(cs[c].paid_for) / cs[c].a;
    }

    return r;
}

int main() {
    int t;
    cin >> t;
    for (int i = 0; i < t; ++i) {
        int N;
        cin >> N;
        cs.clear();
        for (int j = 0; j < N; ++j) {
            C c;
            cin >> c.a >> c.b >> c.d;
            cs.push_back(c);
        }

        printf("%0.2f\n", solve());
    }

    return 0;
}

