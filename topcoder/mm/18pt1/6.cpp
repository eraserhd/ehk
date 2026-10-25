#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
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
string MID_data;
string MID;

#define RKH_BASE 97
#define RKH_N 912727
#define RKH_K 8
#define RKH_POWER_K 54834 // RKH_BASE^RKH_K%RKH_N
int rkh_value;
char rkh_history[RKH_K];
int rkh_history_index;

void rkh_init() {
    rkh_value = 0;
    rkh_history_index = 0;
    memset(rkh_history, 0, sizeof(rkh_history));
}

int rkh_update(int cv) {
    rkh_value = (((rkh_value*RKH_BASE%RKH_N)
	      + (RKH_N-(int)rkh_history[rkh_history_index]*RKH_POWER_K%RKH_N)%RKH_N)%RKH_N
		+ cv)%RKH_N;
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

void push_block(const Block& block, string::size_type ti) {
    bool added = false;
    int M_adjust = 0;
    if (!blocks.empty()) {
	int dist = block.offset - (blocks.back().offset+blocks.back().size);
	if (abs(dist) < B) {
	    added = true;
	    string::size_type bti = blocks.back().offset+blocks.back().size;
	    blocks.back().size += block.size + dist;
	    string::size_type istart = MID.find_last_not_of("I");
	    string::size_type iend = string::npos;
	    if (istart != string::npos) {
		++istart;
		iend = MID.find('D', istart);
		if (iend == string::npos)
		    iend = MID.size();
	    }
	    while (dist > 0) {
		// Need to insert a D because we want to push a block which
		// has a gap from the last block.  If we have a run of Is
		// before here, we can change one of the to an M instead _if_
		// the I corresponds to the letter we would be deleting.
		bool gotit = false;
		if (istart != string::npos) {
		    string::size_type iat = MID_data.find(from[bti], istart);
		    if (iat != string::npos && iat < iend) {
			MID[iat] = 'M';
			istart = iat+1;
			gotit = true;
		    }
		}
		if (!gotit) {
		    //istart = string::npos;
		    if (istart != string::npos) {
			MID.insert(istart, 1, 'D');
			MID_data.insert(istart, 1, '?');
			++istart;
			++iend;
		    } else {
			MID += 'D';
			MID_data += '?';
		    }
		}
		--dist, ++bti;
	    }
	    while (dist < 0) {
		MID += 'I';
		MID_data += to[ti++];
		++dist;
		++M_adjust;
	    }
	}
    }
    if (!added)
	blocks.push_back(block);
    MID += string(block.size - M_adjust, 'M');
    MID_data += string(block.size - M_adjust, '?');
}

void optimize_ID_run(string& new_r, string& I, string& D) {
    const int LIMIT = 800;
    if (!I.empty() && !D.empty()) {
	// Remove characters which don't appear in both I and D.  This
	// works in O(2|I| + 2|D|)
	string workI = I, workD = D;
	int workIsize, workDsize;
	if (I.size()*D.size() >= 500) {
	    char h[97] = {0};
	    TR(I, i)
		h[(*i)-32] = 1;
	    TR(D, i)
		h[(*i)-32] |= 2;
	    string::iterator id = workI.begin();
	    TR(workI, i)
		if (h[(*i)-32] == 3)
		    *id++ = *i;
	    workIsize = id-workI.begin();
	    id = workD.begin();
	    TR(workD, i)
		if (h[(*i)-32] == 3)
		    *id++ = *i;
	    workDsize = id-workD.begin();
	} else {
	    workIsize = workI.size();
	    workDsize = workD.size();
	}

	if ((workIsize+1)*(workDsize+1) > LIMIT) {
	    new_r.resize(new_r.size()+I.size()+D.size(), 'I');
	    fill_n(new_r.rbegin(), D.size(), 'D');
	    I.clear();
	    D.clear();
	    return;
	}

	// Longest common subsequence.  This works in O(|workI|*|workD|)
	static short c[LIMIT];
#define C(i,j) c[(i)*(workDsize+1)+(j)]
	for (int i = 1; i <= workIsize; ++i)
	    C(i,0) = 0;
	for (int j = 0; j <= workDsize; ++j)
	    C(0,j) = 0;

	for (int i = 1; i <= workIsize; ++i) {
	    for (int j = 1; j <= workDsize; ++j) {
		if (workI[i-1] == workD[j-1]) {
		    C(i,j) = C(i-1,j-1)+1;
		} else if (C(i-1,j) >= C(i,j-1)) {
		    C(i,j) = C(i-1,j);
		} else {
		    C(i,j) = C(i,j-1);
		}
	    }
	}
	new_r.resize(new_r.size()+I.size()+D.size()-C(workIsize,workDsize));
	string::iterator ai = new_r.end()-1;
	int i = workIsize, j = workDsize;
	int ii = I.size()-1, di = D.size()-1;
	while (i != 0 && j != 0) {
	    if (C(i-1,j) == C(i,j))
		--i;
	    else if (C(i,j-1) == C(i,j))
		--j;
	    else {
		char c = workD[j-1];
		while (I[ii] != c) {
		    --ii;
		    *ai = 'I';
		    --ai;
		}
		while (D[di] != c) {
		    --di;
		    *ai = 'D';
		    --ai;
		}
		*ai = 'M';
		--ai;
		--i, --j, --ii, --di;
	    }
	}
	while (ii >= 0) {
	    *ai = 'I';
	    --ai, --ii;
	}
	while (di >= 0) {
	    *ai = 'D';
	    --ai, --di;
	}
    } else {
	new_r.resize(new_r.size()+I.size()+D.size(), 'I');
	fill_n(new_r.rbegin(), D.size(), 'D');
    }
    I.clear();
    D.clear();
}

void optimize_ID_runs(string& r) {
    string D, I, new_r;
    vector<Block>::iterator bi = blocks.begin();
    string::iterator si, se;
    if (bi != blocks.end()) {
	si = from.begin()+bi->offset;
	se = from.begin()+bi->offset+bi->size;
    }
    string::size_type ri = 0, di = 0;
    while (ri < r.size() || di < to.size()) {
	switch (r[ri]) {
	case 'D':
	    D += *si;
	    if (++si == se) {
		++bi;
		if (bi != blocks.end()) {
		    si = from.begin()+bi->offset;
		    se = from.begin()+bi->offset+bi->size;
		}
	    }
	    break;
	case 'I':
	    I += to[di++];
	    break;
	case 'M':
	    optimize_ID_run(new_r, I, D);
	    ++di;
	    if (++si == se) {
		++bi;
		if (bi != blocks.end()) {
		    si = from.begin()+bi->offset;
		    se = from.begin()+bi->offset+bi->size;
		}
	    }
	    new_r += 'M';
	    break;
	}
	++ri;
    }
    optimize_ID_run(new_r, I, D);
    r = new_r;
}

string transform() {
    hash_from();
    blocks.clear();
    MID.clear();
    MID_data.clear();

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
	    push_block(best, ti);
	    ti += best.size;
	} else {
	    MID += 'I';
	    MID_data += to[ti];
	    ++ti;
	}
    }

    //optimize_ID_runs(MID);

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