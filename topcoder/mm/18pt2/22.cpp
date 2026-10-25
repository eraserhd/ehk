#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits.h>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <sstream>
#include <string>
#include <sys/time.h>
#include <time.h>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

namespace MM18 BEGIN

int B, S;
unsigned total_bytes;
unsigned average_version_bytes;

struct Block {
    int offset, size, cost;
    string::size_type position;
    Block() : size(-1), cost(B) {}
    Block(int offset, int size, int cost = B) : offset(offset), size(size), cost(cost) {}
};

string from, to;
int from_size, to_size;
list<Block> blocks;
string MID;

#define RKH_BASE 97
//#define RKH_K 5
#define RKH_N (1<<17)
#define RKH_MOD(x) ((x)&(RKH_N-1))
//#define RKH_POWER_K5 27105 // 5, 17 / 5, 15
//#define RKH_POWER_K7 95905 // 7, 17

int rkh_value;
char rkh_history[7];
int rkh_history_index;

void rkh_init() {
    rkh_value = 0;
    rkh_history_index = 0;
    memset(rkh_history, 0, sizeof(rkh_history));
}

template<int K> struct rkh_traits_t {};
template<> struct rkh_traits_t<5> { enum { POWER_K = 27105 }; };
template<> struct rkh_traits_t<7> { enum { POWER_K = 95905 }; };

template<int K>
int rkh_update(int cv) {
    rkh_value = RKH_MOD(
            RKH_MOD(rkh_value*RKH_BASE)
              + RKH_MOD(RKH_N-RKH_MOD(rkh_history[rkh_history_index]*rkh_traits_t<K>::POWER_K))
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

const int DP_MAX_SIZE = 256;
void push_block(const Block& block) {
    bool added = false;
    int M_adjust = 0;
    if (!blocks.empty()) {
        int dist = block.offset - (blocks.back().offset+blocks.back().size);
        if (dist > 0) {
            string::size_type istart = MID.find_last_not_of("I");
            if (istart == string::npos)
                istart = MID.size();
            else
                ++istart;
            int I_count = MID.size()-istart;
            if (abs(dist)+S-I_count < B) {
                added = true;
                int I_count = MID.size()-istart;
                int ti_ofs = block.position-I_count, ti_end = block.position;
                int td_ofs = blocks.back().offset+blocks.back().size;
                int td_end = td_ofs+dist;
                int old_size = blocks.back().size;

                blocks.back().size += block.size + dist;
                MID.erase(MID.begin()+istart, MID.end());
                char last = 'M';
                int cost = 0;

                if (td_end-td_ofs >DP_MAX_SIZE-2 || ti_end-ti_ofs >DP_MAX_SIZE-2||
                    td_end-td_ofs <=S || ti_end-ti_ofs <=S) {
                    while (td_ofs < td_end || ti_ofs < ti_end) {
                        if (ti_end-ti_ofs > 2*S && td_end-td_ofs > 2*S) {
                            int cd_ofs = td_ofs, ci_ofs = ti_ofs, size = 0;
                            while (cd_ofs < td_end && ci_ofs < ti_end &&
                                   from[cd_ofs] == to[ci_ofs])
                                ++cd_ofs, ++ci_ofs, ++size;

                            if (size >= 2*S) {
                                MID.append(size, 'M');
                                td_ofs = cd_ofs;
                                ti_ofs = ci_ofs;
                                if (last != 'M')
                                    cost += S;
                                last = 'M';
                                continue;
                            }
                        }
                        if (td_ofs < td_end) {
                            MID += 'D';
                            ++td_ofs;
                            if (last != 'D')
                                cost += S;
                            ++cost;
                            last = 'D';
                            continue;
                        }
                        MID += 'I';
                        ++ti_ofs;
                        if (last != 'I')
                            cost += S;
                        ++cost;
                        last = 'I';
                    }
                } else {
                    int ie = ti_end-ti_ofs, de = td_end-td_ofs, tmp;
                    static int dp[3][DP_MAX_SIZE][DP_MAX_SIZE];
                    dp[0][ie][de] = 0; // dp[previous character: 0=M, 1=I, 2=D][offset in 'to'][offset in 'from']
                    dp[1][ie][de] = S;
                    dp[2][ie][de] = S;
                    for (int ii = ie-1; ii >= 0; --ii)
                        for (int c = 0; c < 3; ++c)
                            dp[c][ii][de] = (c==1?0:S) + 1 + dp[1][ii+1][de];
                    for (int di = de-1; di >= 0; --di)
                        for (int c = 0; c < 3; ++c)
                            dp[c][ie][di] = (c==2?0:S) + 1 + dp[2][ie][di+1];
                    for (int ii = ie-1; ii >= 0; --ii)
                        for (int di = de-1; di >= 0; --di) {
                            int nextM = dp[0][ii+1][di+1], nextI = dp[1][ii+1][di]+1, nextD = dp[2][ii][di+1]+1;
                            int work = S + min(nextI, nextD);
                            bool match = (from[td_ofs+di] == to[ti_ofs+ii]);
                            if (match && nextM < work)
                                work = nextM;
                            dp[0][ii][di] = work;
                            work = min(nextI, nextD+S);
                            if (match && (tmp = nextM+S) < work)
                                work = tmp;
                            dp[1][ii][di] = work;
                            work = min(nextI+S, nextD);
                            if (match && tmp < work)
                                work = tmp;
                            dp[2][ii][di] = work;
                        }

                    cost = dp[0][0][0];
                    if (cost-I_count <= B+S) { // Don't construct if we already know we're too expensive
                        int ii = 0, di = 0;
                        int last = 0;
                        while (ii < ie || di < de) {
                            if (di < de && dp[last][ii][di] == (last != 2 ? S : 0) + 1 + dp[2][ii][di+1]) {
                                last = 2;
                                MID += 'D';
                                ++di;
                            } else if (ii < ie && dp[last][ii][di] == (last != 1 ? S : 0) + 1 + dp[1][ii+1][di]) {
                                last = 1;
                                MID += 'I';
                                ++ii;
                            } else {
                                last = 0;
                                MID += 'M';
                                ++di, ++ii;
                            }
                        }
                    }
                }

                if (cost-I_count > B+S) {
                    MID.replace(MID.begin()+istart, MID.end(), I_count, 'I');
                    added = false;
                    blocks.back().size = old_size;
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

bool find_best(string::size_type ti, int lofs, Block& best, int except = -1) {
    int i = from_hash[to_hashes[ti]];
    while (i != -1) {
        if (i != except) {
            int fii = i, tii = ti, size = 0;
            while (fii < from_size && tii < to_size && from[fii] == to[tii])
                ++fii, ++tii, ++size;

            int cost = B;
            if (lofs != -1) {
                int ncost = abs(i-lofs)+S;
                if (ncost < cost)
                    cost = ncost;
            }
            if (size-cost >= best.size-best.cost) {
                best.offset = i;
                best.size = size;
                best.cost = cost;
                best.position = ti;
            }
        }
        i = from_hash_next[i];
    }
    return (best.size > best.cost);
}

const int NEXT_OFS = 4;
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
            bool took_next = false;
            if (ti+NEXT_OFS < (to.size()-best.size) && best.size > NEXT_OFS+S) {
                find_best(ti+NEXT_OFS, lofs, next_best, best.offset+NEXT_OFS);
                if (next_best.size != -1) {
                    next_best.cost += NEXT_OFS+1;
                    while (next_best.position > ti &&
                           from[next_best.offset-1] == to[next_best.position-1]) {
                        --next_best.position;
                        --next_best.offset;
                        ++next_best.size;
                        --next_best.cost;
                    }
                     
                    if (next_best.size-next_best.cost > best.size-best.cost) {
                        result.push_back(next_best);
                        ti = next_best.position+next_best.size;
                        took_next = true;
                    }
                }
            }
            // End check for longer block
            if (!took_next) {
                result.push_back(best);
                ti += best.size;
            }
        } else
            ++ti;
    }
}

string transform() {
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
    MID.append(to.size()-ti, 'I');

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

// Standard CRC.  This was originally copied from Wikipedia, as I would
// normally do.  Then I realized this might be a Bad Thing(tm) for a TopCoder
// contest and rewrote it from the English description.  It doesn't look
// significantly different.  This worries me, but how much different can
// you make a CRC look?
unsigned crc(const string& s) {
    unsigned sum1 = 0xffff, sum2 = 0xffff, mod_cycle = 360;
    for (int i = 0; i < s.size(); i += 2) {
        sum1 += *reinterpret_cast<const unsigned short*>(&s[i]);
        sum2 += sum1;
        if (--mod_cycle == 0) {
            mod_cycle = 360;
            sum1 = (sum1 & 0xffff) + (sum1 >> 16);
            sum2 = (sum2 & 0xffff) + (sum2 >> 16);
        }
    }

    sum1 = (sum1 & 0xffff) + (sum1 >> 16);
    sum2 = (sum2 & 0xffff) + (sum2 >> 16);
    return sum2 << 16 | sum1;
}

END

struct timeval start;
int mark() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    int usecs = ((int)tv.tv_sec - start.tv_sec)*1000000 + ((int)tv.tv_usec - start.tv_usec);
    memcpy(&start, &tv, sizeof (struct timeval));
    return usecs;
}

struct BlockEditDistance2 {
    vector<string> transform(const vector<string>& versions, int B, int S) {
        gettimeofday(&start, NULL);
        MM18::B = B;
        MM18::S = S;
        vector<string> result;

        MM18::total_bytes = 0;
        TR(versions,vi)
            MM18::total_bytes += vi->size();
        MM18::average_version_bytes = MM18::total_bytes/versions.size();

        MM18::to = versions.back();
        MM18::to_size = MM18::to.size();

        mark();
        MM18::preprocess<5>();
        cout<<"preprocess="<<mark()<<endl;
        map<unsigned, list<int> > vmap;
        vector<unsigned> crcs;
        crcs.resize(versions.size()-1);
        for (int i = 0; i < versions.size()-1; ++i)
            crcs[i] = MM18::crc(versions[i]);
        cout<<"crc="<<mark()<<endl;
        for (int i = 0; i < versions.size()-1; ++i) {
            bool dup = false;
            unsigned key = crcs[i];
            list<int>& l = vmap[key];
            TR(l, vi) {
                if (versions[i] == versions[(*vi)]) {
                    result.push_back(result[(*vi)]);
                    dup = true;
                    break;
                }
            }
            if (!dup) {
                l.push_back(i);
                MM18::from = versions[i];
                MM18::from_size = MM18::from.size();
                mark();
                MM18::hash_from<5>();
                cout <<'v'<<i<<'='<<MM18::from_size<<"b,h="<<mark()<<",";
                mark();
                result.push_back(MM18::transform());
                cout <<"t="<<mark()<<endl;
            }
        }
        return result;
    }
};

