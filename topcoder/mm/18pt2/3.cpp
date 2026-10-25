#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits.h>
#include <list>
#include <numeric>
#include <queue>
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

int B, S;

struct Block {
    int offset, size, cost;
    Block() : size(-1), cost(B) {}
    Block(int offset, int size, int cost = B) : offset(offset), size(size), cost(cost) {}
};

string from, to;
vector<Block> blocks;
string MID;

#define RKH_BASE 97
#define RKH_K 5
#define RKH_N (1<<17)
#define RKH_MOD(x) ((x)&(RKH_N-1))
#define RKH_POWER_K 27105
int rkh_value;
char rkh_history[RKH_K];
int rkh_history_index;

void rkh_init() {
    rkh_value = 0;
    rkh_history_index = 0;
    memset(rkh_history, 0, sizeof(rkh_history));
}

int rkh_update(int cv) {
    rkh_value = RKH_MOD(
            RKH_MOD(rkh_value*RKH_BASE)
              + RKH_MOD(RKH_N-RKH_MOD(rkh_history[rkh_history_index]*RKH_POWER_K))
        + cv);
    rkh_history[rkh_history_index] = cv;
    if (++rkh_history_index == RKH_K)
        rkh_history_index = 0;
    return rkh_value;
}

vector<int> from_hash_next;
int from_hash[RKH_N];
vector<int> to_hashes;

void hash_from() {
    memset(from_hash, -1, sizeof(from_hash));
    from_hash_next.resize(from.size());

    rkh_init();
    for (int k = 0; k < RKH_K; ++k)
        rkh_update(k >= from.size() ? 0 : from[k]-32+1);
    for (string::size_type i = 0; i < from.size(); ++i) {
        from_hash_next[i] = from_hash[rkh_value];
        from_hash[rkh_value] = i;
        string::size_type ii = i+RKH_K;
        rkh_update(ii >= from.size() ? 0 : from[ii]-32+1);
    }
}

void push_block(Block block, string::size_type ti) {
    bool added = false;
    int M_adjust = 0;
    if (!blocks.empty()) {
        int dist = block.offset - (blocks.back().offset+blocks.back().size);
        if (abs(dist) < B) {
            added = true;
            blocks.back().size += block.size + dist;
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
        blocks.push_back(block);
    MID += string(block.size - M_adjust, 'M');
}

string transform() {
    hash_from();
    blocks.clear();
    MID.clear();

    string::size_type ti = 0;
    while (ti < to.size()) {
        Block best;
        int i = from_hash[to_hashes[ti]];
        while (i != -1) {
            int size = 0;
            int fii = i, tii = ti;
            while (fii < from.size() && tii < to.size() && from[fii] == to[tii])
                ++fii, ++tii, ++size;
            int cost = B;

            if (!blocks.empty()) {
                int dist = i-(blocks.back().offset+blocks.back().size);
                if (dist > 0)
                    cost = max(0, min(cost, dist));
                else
                    cost = min(cost, abs(dist));
            }
            if (size-cost >= best.size-best.cost) {
                best.offset = i;
                best.size = size;
                best.cost = cost;
            }
            i = from_hash_next[i];
        }

        if (best.size > best.cost) {
            push_block(best, ti);
            ti += best.size;
        } else {
            MID += 'I';
            ++ti;
        }
    }

    ostringstream out;
    TR(blocks, bi)
        out << bi->offset << "-" << bi->offset+bi->size-1 << " ";
    out << MID;
    return out.str();
}

void preprocess() {
    to_hashes.resize(to.size());
    rkh_init();
    for (int k = 0; k < RKH_K; ++k)
        rkh_update(k >= to.size() ? 0 : to[k]-32+1);

    for (string::size_type ti = 0; ti < to.size(); ++ti) {
        to_hashes[ti] = rkh_value;
        rkh_update(ti+RKH_K >= to.size() ? 0 : to[ti+RKH_K]-32+1);
    }
}

END

struct BlockEditDistance2 {
    vector<string> transform(const vector<string>& versions, int B, int S) {
        MM18::B = B;
        MM18::S = S;
        MM18::to = versions.back();

        MM18::preprocess();
        vector<string> result;
        for (int i = 0; i < versions.size()-1; ++i) {
            MM18::from = versions[i];
            result.push_back(MM18::transform());
        }
        return result;
    }
};