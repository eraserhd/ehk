#include <algorithm>
#include <cassert>
#include <cmath>
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

struct Block {
    int offset, size, cost;
    Block() : size(-1), cost(B) {}
    Block(int offset, int size, int cost = B) : offset(offset), size(size), cost(cost) {}
};

string from, to;
vector<Block> blocks;
string MID;

struct RollingHash {
    enum {
        K = 8,
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
vector<RollingHash::value_type> to_hashes;

void bsearch_cmp(int start_fi, int ti, Block& best) {
}

void hash_from() {
    memset(from_hash, -1, sizeof(from_hash));
    from_hash_next.resize(from.size());

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

void push_block(const Block& block) {
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
	    if (!blocks.empty())
		cost = min(cost, abs(i-(blocks.back().offset+blocks.back().size)));
	    if (size-cost > best.size-best.cost) {
		best.offset = i;
		best.size = size;
		best.cost = cost;
	    }
	    i = from_hash_next[i];
	}

        if (best.size > best.cost) {
	    push_block(best);
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
    RollingHash rh;
    for (int k = 0; k < RollingHash::K; ++k)
        rh.update(k >= to.size() ? 0 : to[k]-32+1);

    for (string::size_type ti = 0; ti < to.size(); ++ti) {
	to_hashes[ti] = rh.value;
	rh.update(ti+RollingHash::K >= to.size() ? 0 : to[ti+RollingHash::K]-32+1);
    }
}

END

struct BlockEditDistance {
    vector<string> transform(const vector<string>& versions, int B) {
        MM18::B = B;
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