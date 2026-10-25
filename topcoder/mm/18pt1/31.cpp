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

// This is just (int) 1.5*log(n).
char I_adjust_table[] = {
0,0,0,1,1,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
};
const char I_adjust_end = 8;

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
//#define RKH_K 5
#define RKH_N (1<<17)
#define RKH_MOD(x) ((x)&(RKH_N-1))
//#define RKH_POWER_K 27105
int rkh_value;
char rkh_history[10];
int rkh_history_index;


template<int K> struct rkh_traits {};
template<> struct rkh_traits<5> {
    enum { POWER_K = 27105 };
};
template<> struct rkh_traits<3> {
    enum { POWER_K = 126241 };
};

void rkh_init() {
    rkh_value = 0;
    rkh_history_index = 0;
    memset(rkh_history, 0, sizeof(rkh_history));
}

template<int K>
int rkh_update(int cv) {
    rkh_value = RKH_MOD(
            RKH_MOD(rkh_value*RKH_BASE)
              + RKH_MOD(RKH_N-RKH_MOD(rkh_history[rkh_history_index]*rkh_traits<K>::POWER_K))
        + cv);
    rkh_history[rkh_history_index] = cv;
    if (++rkh_history_index == K)
        rkh_history_index = 0;
    return rkh_value;
}

vector<int> from_hash_next;
int from_hash[RKH_N];
vector<int> to_hashes;

template<int K>
void hash_from() {
    memset(from_hash, -1, sizeof(from_hash));
    from_hash_next.resize(from.size());

    rkh_init();
    for (int k = 0; k < K; ++k)
        rkh_update<K>(k >= from.size() ? 0 : from[k]-32+1);
    for (string::size_type i = 0; i < from.size(); ++i) {
        from_hash_next[i] = from_hash[rkh_value];
        from_hash[rkh_value] = i;
        string::size_type ii = i+K;
        rkh_update<K>(ii >= from.size() ? 0 : from[ii]-32+1);
    }
}

const int OPTIMIZE_ID_LIMIT = (1024*1024);
bool optimize_ID_run(string::size_type istart, const string& D, int& cost) {
    string I = MID_data.substr(istart);
    if (I.empty() || I.size() <= 2 || D.size() <= 2)
        return false;
    assert(!I.empty());
    assert(!D.empty());

    // Remove characters which don't appear in both I and D.  This
    // works in O(2|I| + 2|D|)
    string workI = I, workD = D;
    int workIsize, workDsize;
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

    if ((workIsize+1)*(workDsize+1) > OPTIMIZE_ID_LIMIT)
        return false;
    if (workIsize <= 2 || workDsize <= 2)
        return false;

    // Longest common subsequence.  This works in O(|workI|*|workD|)
    static short c[OPTIMIZE_ID_LIMIT];
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
    string::size_type new_size = istart+I.size()+D.size()-C(workIsize,workDsize);
    MID.resize(new_size);
    MID_data.resize(new_size);
    string::size_type ai = MID.size()-1;
    string::size_type ai2 = MID_data.size()-1;
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
                MID[ai] = 'I';
                MID_data[ai2] = I[ii];
                --ii, --ai, --ai2;
            }
            while (D[di] != c) {
                MID[ai] = 'D';
                MID_data[ai2] = D[di];
                --di, --ai, --ai2, ++cost;
            }
            MID[ai] = 'M';
            MID_data[ai2] = c;
            --ai, --ai2;
            --i, --j, --ii, --di, --cost;
        }
    }
    while (ii >= 0) {
        MID[ai] = 'I';
        MID_data[ai2] = I[ii];
        --ai, --ai2, --ii;
    }
    while (di >= 0) {
        MID[ai] = 'D';
        MID_data[ai2] = D[di];
        --ai, --ai2, --di, ++cost;
    }
    return true;
}

void push_block(Block block, string::size_type ti) {
    bool added = false;
    int M_adjust = 0;
    if (!blocks.empty()) {
        int dist = block.offset - (blocks.back().offset+blocks.back().size);
        if (dist > 0) {
            string::size_type istart = MID.find_last_not_of("I"), iend = MID.size();
            if (istart != string::npos)
                ++istart;
            else
                istart = MID.size();
            if (dist < B+(iend-istart)+20) {
                string::size_type istart_save = istart, iilen_save = iend-istart;
                string MID_data_save(MID_data.substr(istart));
                int old_block_size = blocks.back().size;
                added = true;
                string::size_type bti = blocks.back().offset+blocks.back().size;
                blocks.back().size += block.size + dist;
                int cost = 0;
                if (!optimize_ID_run(istart, from.substr(bti,dist), cost)) {
                    cost = 0;
                    while (dist > 0) {
                        // Need to insert a D because we want to push a block which
                        // has a gap from the last block, but it is cheaper to D a run
                        // than incur cost B.

                        // If we have a run of Is before here, we can change one of the
                        // to an M instead _if_  the I corresponds to the letter we
                        // would be deleting.
                        
                        // Also, we now insert the D at the start of the preceding run
                        // of Is to get us more opportunities to take the above.
                        bool gotit = false;
                        if (istart != string::npos) {
                            string::size_type iat = MID_data.find(from[bti], istart);
                            if (iat != string::npos && iat < iend) {
                                MID[iat] = 'M';
                                istart = iat+1;
                                gotit = true;
                                --cost;
                            }
                        }
                        if (!gotit) {
                            ++cost;
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
                }
                
                if (cost > B) {
                    added = false;
                    blocks.back().size = old_block_size;
                    MID.replace(MID.begin()+istart_save, MID.end(), iilen_save, 'I');
                    MID_data.replace(MID_data.begin()+istart_save, MID_data.end(), MID_data_save);
                }
            }
        } else {
            if (abs(dist) < B) {
                added = true;
                blocks.back().size += block.size + dist;
                while (dist < 0) {
                    MID += 'I';
                    MID_data += to[ti++];
                    ++dist;
                    ++M_adjust;
                }
            }
        }
    } else {
        // First block.  See if we can bring back the start pointer and replace some preceding
        // Is.  FIXME: Should be an LCS-like way to do this
        string::size_type tti = ti;
        while (tti > 0 && block.offset > 0) {
            char ch = from[block.offset-1];
            string::size_type ans = MID_data.find_last_of(ch,tti-1);
            if (ans == string::npos)
                break;
            MID[ans] = 'M';
            --block.offset;
            ++block.size;
            ++M_adjust;
            tti = ans;
        }
    }
    if (!added)
        blocks.push_back(block);
    MID += string(block.size - M_adjust, 'M');
    MID_data += string(block.size - M_adjust, '?');
}

string transform() {
    blocks.clear();
    MID.clear();
    MID_data.clear();

    string::size_type ti = 0, ii;
    int I_count = 0;
    int I_adjust;
    while (ti < to.size()) {
        I_adjust = (I_count < sizeof(I_adjust_table)/sizeof(I_adjust_table[0])) ?
               I_adjust_table[I_count] : I_adjust_end;
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
                    cost = max(0, min(cost, dist-I_adjust));
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
            ii = MID.find_last_not_of("I");
            if (ii != string::npos)
                I_count = MID.size()-(ii+1);
            else
                I_count = 0;
        } else {
            MID += 'I';
            MID_data += to[ti];
            ++ti;
            ++I_count;
        }
    }

    // FIXME: Should be an LCS-like way to do this
    while (!blocks.empty() && blocks.back().offset+blocks.back().size < from.size() &&
           I_count > 0) {
        char ch = from[blocks.back().offset+blocks.back().size];
        string::size_type iif = MID_data.find(ch, MID_data.size()-I_count);
        if (iif == string::npos)
            break;
        MID[iif] = 'M';
        ++blocks.back().size;
        I_count = MID.size()-(iif+1);
    }

    ostringstream out;
    TR(blocks, bi)
        out << bi->offset << "-" << bi->offset+bi->size-1 << " ";
    out << MID;
    return out.str();
}

template<int K>
void preprocess() {
    to_hashes.resize(to.size());
    rkh_init();
    for (int k = 0; k < K; ++k)
        rkh_update<K>(k >= to.size() ? 0 : to[k]-32+1);

    for (string::size_type ti = 0; ti < to.size(); ++ti) {
        to_hashes[ti] = rkh_value;
        rkh_update<K>(ti+K >= to.size() ? 0 : to[ti+K]-32+1);
    }
}

END

struct BlockEditDistance {
    vector<string> transform(const vector<string>& versions, int B) {
        MM18::B = B;
        MM18::to = versions.back();
        MM18::preprocess<5>();
        string::size_type size = 0;

        vector<string> result;
        for (int i = 0; i < versions.size()-1; ++i) {
            MM18::from = versions[i];
            size += MM18::from.size();
            MM18::hash_from<5>();
            result.push_back(MM18::transform());
        }

        if (size < 100000) {
            MM18::preprocess<3>();
            for (int i = 0; i < versions.size()-1; ++i) {
                MM18::from = versions[i];
                MM18::hash_from<3>();
                string r2 = MM18::transform();

                int old_cost = count(ALL(result[i]), 'I') + count(ALL(result[i]), 'D') +
                                B * count(ALL(result[i]), ' ');
                int new_cost = count(ALL(r2), 'I') + count(ALL(r2), 'D') +
                                B * count(ALL(r2), ' ');
                if (new_cost < old_cost) {
                    cerr <<"new_cost="<<new_cost<<",old_cost="<<old_cost<<endl;
                    result[i] = r2;
                }
            }
        }

        return result;
    }
};