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
    string::size_type position;
    Block() : size(-1), cost(B) {}
    Block(int offset, int size, int cost = B) : offset(offset), size(size), cost(cost) {}
};

string from, to;
list<Block> blocks;
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

void push_block(const Block& block) {
    bool added = false;
    int M_adjust = 0;
    if (!blocks.empty()) {
        int dist = block.offset - (blocks.back().offset+blocks.back().size);
        if (dist > 0) {
            if (abs(dist) < B) {
                added = true;
                string::size_type istart = MID.find_last_not_of("I");
                if (istart == string::npos)
                    istart = MID.size();
                else
                    ++istart;
                int I_count = MID.size()-istart;
                int ti_ofs = block.position-I_count, ti_end = block.position;
                int td_ofs = blocks.back().offset+blocks.back().size;
                int td_end = td_ofs+dist;
                blocks.back().size += block.size + dist;
                MID.erase(MID.begin()+istart, MID.end());
                while (td_ofs < td_end || ti_ofs < ti_end) {
                    if (ti_end-ti_ofs > 2*S && td_end-td_ofs > 2*S) {
                        int cd_ofs = td_ofs, ci_ofs = ti_ofs, size = 0;
                        while (cd_ofs < td_end && ci_ofs < ti_end &&
                               from[cd_ofs] == to[ci_ofs])
                            ++cd_ofs, ++ci_ofs, ++size;

                        if (size >2*S) {
                            MID.append(size, 'M');
                            td_ofs = cd_ofs;
                            ti_ofs = ci_ofs;
                            continue;
                        }
                    }
                    if (td_ofs < td_end) {
                        MID += 'D';
                        ++td_ofs;
                        continue;
                    }
                    MID += 'I';
                    ++ti_ofs;
                }
            }
        } else {
            if (abs(dist) < B) {
                added = true;
                blocks.back().size += block.size + dist;
                while (dist < 0) {
                    MID += 'I';
                    ++dist;
                    ++M_adjust;
                }
            }
        }
    }
    if (!added)
        blocks.push_back(block);
    MID.append(block.size - M_adjust, 'M');
}

bool find_best(string::size_type ti, int lofs, Block& best) {
    int i = from_hash[to_hashes[ti]];
    while (i != -1) {
        int size = 0;
        int fii = i, tii = ti;
        while (fii < from.size() && tii < to.size() && from[fii] == to[tii])
            ++fii, ++tii, ++size;
        int cost = B;

        if (lofs != -1)
            cost = min(cost, abs(i-lofs)+S);
        if (size-cost >= best.size-best.cost) {
            best.offset = i;
            best.size = size;
            best.cost = cost;
            best.position = ti;
        }
        i = from_hash_next[i];
    }
    return (best.size > best.cost);
}

void select_blocks(list<Block>& result) {
    string::size_type ti = 0;
    int lofs;
    while (ti < to.size()) {
        Block best;
        lofs = -1;
        if (!result.empty())
            lofs = result.back().offset+result.back().size;
        if (find_best(ti, lofs, best)) {
            // Check for a longer block in a few clicks, in case a mispeling in
            // the next N characters (or lack of one) is causing a shorter match
            Block next_best;
            const int NEXT_OFS = 4;
            bool took_next = false;
            if (ti+NEXT_OFS < to.size() && best.size > NEXT_OFS+1 &&
                find_best(ti+NEXT_OFS, lofs, next_best)) {
                
                while (next_best.position > ti &&
                       from[next_best.offset-1] == to[next_best.position-1]) {
                    --next_best.position;
                    --next_best.offset;
                    ++next_best.size;
                }
                 
                if (next_best.size-next_best.cost > best.size-best.cost+NEXT_OFS+1) {
                    result.push_back(next_best);
                    ti = next_best.position+next_best.size;
                    took_next = true;
                }
            }
            // End check
            if (!took_next) {
                result.push_back(best);
                ti += best.size;
            }
        } else
            ++ti;
    }
}

string transform() {
    hash_from();
    blocks.clear();
    MID.clear();

    list<Block> work;
    select_blocks(work);
    string::size_type ti = 0;
    TR(work,wi) {
        MID.append(wi->position - ti, 'I');
        ti = wi->position;
        push_block(*wi);
        ti += wi->size;
    }
    MID.append(to.size() - ti, 'I');

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