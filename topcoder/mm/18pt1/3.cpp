#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <list>
#include <numeric>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

namespace MM18 BEGIN

int B;
string from, to;

struct RollingHash {
    enum {
        K = 10,
        BASE = 97,
        N = 912727
    };

    typedef int value_type;

    value_type value;
    int history[K];
    value_type powers[K+1];
    int hindex;

    RollingHash() : value(0), hindex(0) {
        memset(history, 0, sizeof(history));
        powers[0] = 1;
        for (int k = 1; k <= K; ++k)
            powers[k] = powers[k-1]*BASE%N;
    }

    int update(int cv) {
        assert(cv >= 0);
        assert(cv < 97);
        value = (((value*BASE%N)
                  + (N-(value_type)history[hindex]*powers[K]%N)%N)%N
                    + cv%N)%N;
        history[hindex] = cv;
        hindex = (hindex+1)%K;
        return (int)value;
    }
};

vector<int> from_hash_next;
int from_hash[RollingHash::N];

bool suffix_less(int l, int r) {
    if (l==r)
        return false;
    while (l < from.size() && r < from.size() && from[l] == from[r])
        ++l, ++r;
    if (l == from.size() && r == from.size())
        return false;
    if (l == from.size())
        return true;
    if (r == from.size())
        return false;
    return from[l] < from[r];
}

int bsearch_cmp(int start_fi, int ti, pair<int, int>& best) {
    int size = 0;
    int fi = start_fi;
    while (fi < from.size() && ti < to.size() && from[fi] == to[ti])
        ++fi, ++ti, ++size;
    if (size > best.second) {
        best.first = start_fi;
        best.second = size;
    }
    if (ti == to.size())
        return 0;
    if (fi == from.size())
        return -1;
    return to[ti] - from[fi];
}

pair<int, int> find_best_suffix(RollingHash::value_type H, int ti) {
    pair<int, int> best(0,0);
    int i = from_hash[H];
    while (i != -1) {
        bsearch_cmp(i, ti, best);
        i = from_hash_next[i];
    }
    return best;
}

void hash_from() {
    memset(from_hash, -1, sizeof(from_hash));
    from_hash_next.resize(from.size());
    //fill(ALL(from_hash_next), -1);

    RollingHash rh;
    for (int k = 0; k < RollingHash::K; ++k)
        rh.update(k >= from.size() ? 0 : from[k]-32+1);
    for (string::size_type i = 0; i < from.size(); ++i) {
        from_hash_next[i] = from_hash[rh.value];
        from_hash[rh.value] = i;
        string::size_type ii = i+RollingHash::K;
        rh.update(ii >= from.size() ? 0 : from[ii]-32+1);
    }
}

string transform() {
    hash_from();

    vector<pair<int,int> > ranges;
    string MID;

    string::size_type ti = 0, uptoti;
    pair<int, int> best_suffix;
    RollingHash rh;
    for (int k = 0; k < RollingHash::K; ++k)
        rh.update(k >= to.size() ? 0 : to[k]-32+1);
    while (ti < to.size()) {
        best_suffix = find_best_suffix(rh.value, ti);
        if (best_suffix.second > B) {
            bool added = false;
            int M_adjust = 0;
            if (!ranges.empty()) {
                int dist = best_suffix.first - (ranges.back().first+ranges.back().second);
                if (abs(dist) < B) {
                    added = true;
                    ranges.back().second += best_suffix.second + dist;
                    while (dist > 0) {
                        MID += 'D';
                        --dist;
                    }
                    while (dist < 0) {
                        MID += 'I';
                        ++dist;
                        ++M_adjust;
                    }
                }
            }
            if (!added)
                ranges.push_back(best_suffix);
            MID += string(best_suffix.second - M_adjust, 'M');
            uptoti = ti+best_suffix.second;
        } else {
            MID += 'I';
            uptoti = ti+1;
        }
        while (ti < uptoti) {
            rh.update(ti+RollingHash::K >= to.size()
                      ? 0 : to[ti+RollingHash::K]-32+1);
            ++ti;
        }
    }

    ostringstream out;
    TR(ranges, ri)
        out << ri->first << "-" << ri->first+ri->second-1 << " ";
    out << MID;
    return out.str();
}

END

struct BlockEditDistance {
    vector<string> transform(const vector<string>& versions, int B) {
        MM18::B = B;
        MM18::to = versions.back();
        vector<string> result;
        for (int i = 0; i < versions.size()-1; ++i) {
            MM18::from = versions[i];
            result.push_back(MM18::transform());
        }
        return result;
    }
};
