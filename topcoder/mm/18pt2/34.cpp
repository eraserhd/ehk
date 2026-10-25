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

#define DEBUG 1

#ifndef DEBUG
#ifdef assert
#undef assert
#endif
#define assert(x)
#endif

#define INF 1000000000
#define Alph 96

namespace MM18 BEGIN

struct node;

//Suffix tree edge
struct edge {
  int beg, len, exmp;
  node *to;
};

//Suffix tree node
struct node {
  node *suffix;
  edge *uplink;
  edge *links [Alph];
};

node *nodes;
edge *edges;

int next_node = 0, next_edge = 0;
int top_of_to_node_mark, top_of_to_edge_mark;
int cl;

int B, S;

inline edge *new_edge () {
  edge *ret = &edges[next_edge++];
  //printf ("%d\n", next_edge);
  return ret;
};

inline node *new_node () {
  node *ret = &nodes[next_node++];
  assert (next_node <= cl);
  ret -> suffix = NULL;
  memset (ret -> links, 0, sizeof (ret -> links));
  return ret;
};

struct SuffixTree {
  int pos, bpos;          //position in buf, last added position in buf
  node *root;             //root of the tree
  edge *root_edge;        //root edge (hang)
  edge *last_edge;        //last edge of operation
  node *last_node;        //last node of operation
  node *last_split;       //last split
  int last_pos;           //last position
  char *buf;              //string buffer

  void init_tree () {
    root = new_node ();
    root_edge = new_edge ();
    root_edge -> to = root;
    root_edge -> beg = 0;
    root_edge -> len = 0;
    root_edge -> exmp = -1;
    root -> uplink = root_edge;
    pos = 0;
    bpos = 0;
    last_node = NULL;
    last_split = NULL;
    last_pos = 0;
    last_edge = root_edge;
  };

  void append_char (int x) {
    //printf ("adding: %c\n", x + 96);
    assert ((x >= 0) && (x < Alph));
    do {
      //printf ("  bpos = %d: last_pos = %d, next_edge = %d, last_edge = %p\n", bpos, last_pos, next_edge, last_edge);
      node *temp;
      edge *etemp;
      int skipcount;
      if (last_pos == last_edge -> len) {
        //printf ("  bpos = %d: case vertex\n", bpos);
        temp = last_edge -> to;
        if (last_split != NULL) last_split -> suffix = temp;
        last_split = NULL;
        if (temp -> links [x] != NULL) {
          last_edge = temp -> links [x];
          last_node = temp;
          last_pos = 1;
          break;
        };
        etemp = new_edge ();
        temp -> links [x] = etemp;
        skipcount = 0;
      } else {
        //printf ("  bpos = %d: case edge\n", bpos);
        int wchar = buf [last_edge -> beg + last_pos] - 31;
        if (x == wchar) {
          ++last_pos;
          break;
        };
        temp = new_node ();
        temp -> uplink = last_edge;
        etemp = new_edge ();
        temp -> links [wchar] = etemp;
        etemp -> beg = last_edge -> beg + last_pos;
        etemp -> len = (last_edge -> len == INF) ? INF : last_edge -> len - last_pos;
        etemp -> exmp = last_edge -> exmp;
        etemp -> to = last_edge -> to;
        if (etemp -> to != NULL) etemp -> to -> uplink = etemp;
        last_edge -> len = last_pos;
        last_edge -> to = temp;
        if (last_split != NULL) last_split -> suffix = temp;
        last_split = temp;
        etemp = new_edge ();
        temp -> links [x] = etemp;
        temp = last_node;
        skipcount = last_pos;
      };
      etemp -> beg = pos;
      etemp -> len = INF;
      etemp -> exmp = bpos;
      etemp -> to = NULL;
      //printf ("  returned to case general\n");

      temp = temp -> suffix;
      if (temp == NULL) {
        last_node = NULL;
        last_pos = 0;
        last_edge = root_edge;
        if (skipcount == 0) {
          ++bpos;
          break; 
        } else {
          --skipcount;
          temp = root;
        };
      } else
      if (skipcount == 0) {
        last_edge = temp -> uplink;
        last_pos = last_edge -> len;
      };

      //do skip/count
      while (skipcount) {
        char wchar = buf [pos - skipcount] - 31;
        etemp = temp -> links[(int)wchar];
        assert (etemp != NULL);
        if (skipcount <= etemp -> len) {
          last_edge = etemp;
          last_node = temp;
          last_pos = skipcount;
          break;
        };
        skipcount -= etemp -> len;
        temp = etemp -> to;
      };
      ++bpos;
    } while (1);
    assert (last_split == NULL);
    assert (root -> links [x] != NULL);
    //printf ("returned\n");
  };

public:
  void build_tree (char *str) {
    init_tree ();
    buf = str;
    for (;*str;++str) {
      append_char (*str - 31);
      ++pos;
    };
    append_char (0);
  };

};

SuffixTree from_tree;

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

const int DP_MAX_SIZE = 384;
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
                    td_end-td_ofs <=S || ti_end-ti_ofs <=S ||
                    (td_end-td_ofs)*(ti_end-ti_ofs) > 60000) {
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
    string::size_type si = ti;
    edge *etemp = from_tree.root_edge;
    node *temp = from_tree.root;
    const char *ebuf = NULL;
    int epos = 0, depth = 0;
    while (si < to.size()) {
      if (epos == etemp->len) {
        temp = etemp->to;
        edge *netemp = temp->links[to[si]-31];
        epos = 0;
        if (netemp == NULL) break;
        etemp = netemp;
        ebuf = from.c_str() + etemp->beg + 1;
      } else {
        if (to[si] != *ebuf) break;
        ++ebuf;
      };
      ++si;
      ++epos;
      ++depth;
    };

    if (depth > 0 && etemp->exmp != except) {
      int cost = B;
      if (lofs != -1) {
        int ncost = abs(etemp->exmp - lofs)+S;
        if (ncost < cost)
          cost = ncost;
      }
      best.offset = etemp->exmp;
      best.size = depth;
      best.cost = cost;
      best.position = ti;
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
            
            int bound = 0;
            if (!result.empty())
                bound = result.back().position+result.back().size;
            while (ti > bound &&
                   from[best.offset-1] == to[best.position-1]) {
                --best.position;
                --best.offset;
                ++best.size;
                --best.cost;
                --ti;
            }

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
            ti += 3;
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

struct BlockEditDistance2 {
    vector<string> transform(const vector<string>& versions, int B, int S) {
        MM18::B = B;
        MM18::S = S;
        vector<string> result;

        MM18::to = versions.back();
        MM18::to_size = MM18::to.size();

        MM18::cl = 0;
        for (int i = 0; i < versions.size(); i++) {
          int l = versions[i].size ();
          if (i < versions.size() - 1)
            MM18::cl = max(MM18::cl,l);
        };

        MM18::cl += 10;

        MM18::nodes = (MM18::node *)malloc ((MM18::cl + 1000) * 1 * sizeof (MM18::node));
        MM18::edges = (MM18::edge *)malloc ((MM18::cl + 1000) * 2 * sizeof (MM18::edge));

        map<unsigned, list<int> > vmap;
        vector<unsigned> crcs;
        crcs.resize(versions.size()-1);
        for (int i = 0; i < versions.size()-1; ++i)
            crcs[i] = MM18::crc(versions[i]);
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
                MM18::next_node = MM18::next_edge = 0;
                MM18::from_tree.build_tree((char*)MM18::from.c_str());
                result.push_back(MM18::transform());
            }
        }
        return result;
    }
};

