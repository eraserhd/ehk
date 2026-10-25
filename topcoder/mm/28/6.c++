#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <ext/algorithm>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits.h>
#include <map>
#include <queue>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
using namespace std;
using namespace __gnu_cxx;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)
const int INF = 99999999;

namespace MM28 BEGIN

int N;
int pebbles[1000];
int distance[1000];

vector<vector<short> > paths;
int point_origins[1000] = {};

vector<short> adjacent[1000];
vector<string> result;
map<string,int> point_reasons;

void add_path(const vector<short>& p) {
    int np = pebbles[p.front()];
    assert(np>0);
    assert(p.size()==np+1);
    pebbles[p.front()]=0;
    for (int i = 1; i < p.size(); ++i)
        ++pebbles[p[i]];
    paths.push_back(p);
}

void find_distances() {
    char seen[1000]={};
    seen[0]=1;
    queue<int> q;
    q.push(0); q.push(0);
    while (!q.empty()) {
        int v = q.front(); q.pop();
        int d = q.front(); q.pop();
        distance[v]=d;
        TR(adjacent[v],it) {
            if (!seen[*it]) {
                seen[*it]=1;
                q.push(*it);
                q.push(d+1);
            }
        }
    }
}

char dfs_seen[1000];
int  dfs_to[1000];
bool dfs_find_path(int s, int t, int l) {
    if ((s==t || t==-1) && l==0) return true;
    if (l==0) return false;
    dfs_seen[s]=1;
    vector<short> r = adjacent[s];
    random_shuffle(ALL(r));
    int lim=4;
    TR(r,it) {
        if (dfs_seen[*it])
            continue;
        if (t==0 && distance[*it]>l)
            continue;
        if (lim--==0)
            break;
        dfs_to[s]=*it;
        if (dfs_find_path(*it,t,l-1))
            return true;
    }
    dfs_seen[s]=0;
    return false;
}

bool attempt_extend_path(vector<short>& p) {
    int np = pebbles[p.front()]+1;
    while (p.size()<np) {
        int best_c = INF, best_a = -1, best_v = -1;
        for (int a = (int)p.size()-2; a >= 0; --a) {
            TR(adjacent[p[a]],it) {
                if (count(ALL(p),*it)!=0 || count(ALL(adjacent[p[a+1]]),*it)==0)
                    continue;
                int c;
                if (pebbles[*it]>=distance[*it])
                    c=1000;
                else if (pebbles[*it]==0 && distance[*it]>1)
                    c=50;
                else
                    c=1;
                if (c<best_c) {
                    best_c=c;
                    best_a=a;
                    best_v=*it;
                }
            }
        }
        if (best_v==-1)
            break;
        p.insert(p.begin()+best_a+1,best_v);
    }

    if (p.size()<np && np<14) {
        cerr<<"DFS1"<<endl;
        memset(dfs_seen,0,sizeof(dfs_seen));
        if (dfs_find_path(p.front(),p.back(),np)) {
            int v = p.front();
            p.clear();
            for (int l = 0; l < np; ++l) {
                p.push_back(v);
                v = dfs_to[v];
            }
        }
    }

    // Greedy extend
    int lv = p.back(), w, i;
    while (p.size() < np) {
        vector<short> t = adjacent[lv];
        random_shuffle(ALL(t));
        for (i = 0; i < t.size(); ++i) {
            if (count(ALL(p),t[i])==0) {
                p.push_back(lv=t[i]);
                break;
            }
        }
        if (i==t.size())
            break;
    }

    if (p.size()<np) {
        ++point_reasons["scatter"];
        cerr<<"failed to find path of size "<<np<<" from "<<p.front()<<" to "<<p.back()<<endl;
        cerr<<"  (distance of "<<p.front()<<" is "<<distance[p.front()]<<")"<<endl;
        cerr<<"  (found "<<p.size()<<")"<<endl;

        // Find _any_ path of this length to scatter the stones
        cerr<<"Scattering"<<endl;
        memset(dfs_seen,0,sizeof(dfs_seen));
        if (dfs_find_path(p.front(),-1,np)) {
            int v = p.front();
            p.clear();
            for (int l = 0; l < np; ++l) {
                p.push_back(v);
                v = dfs_to[v];
            }
        } else
            return false;
    }

    return true;
}

bool waterfall_finished[1000];
uint32_t waterfall[1000][32];

void compute_waterfall(int v) {
    if (waterfall_finished[v])
        return;
    memset(waterfall[v], 0, sizeof(waterfall[v]));
    TR(adjacent[v],it) {
        if (distance[*it]>distance[v]) {
            compute_waterfall(*it);
            for (int i = 0; i < 32; ++i)
                waterfall[v][i] |= waterfall[*it][i];
        }
    }
    if (pebbles[v]>0)
        waterfall[v][v/32] |= 1u<<(v%32);
    waterfall_finished[v]=true;
}

int waterfall_count(int v) {
    compute_waterfall(v);
    int result = 0;
    for (int i = 0; i < 32; ++i)
        result += __builtin_popcount(waterfall[v][i]);
    return result;
}

void find_best_cost_shortest_target_0_path(vector<short>& p, int v) {
    int costs[1000], l;
    fill(costs,costs+N,INF);
    costs[v]=0;
    set<int> layers[2];
    layers[0].insert(v);
    memset(waterfall_finished,false,sizeof(waterfall_finished));
    for (l = 0; l <= min(distance[v],pebbles[v]); ++l) {
        layers[(l+1)%2].clear();
        TR(layers[l%2],it) {
            TR(adjacent[*it],it2) {
                if (distance[*it2]<distance[*it]) {
                    int s = costs[*it];
                    if (pebbles[*it2]>=distance[*it2])
                        s+=2000+(pebbles[*it2]-distance[*it2]);
                    else if (pebbles[*it2]==0 && distance[*it2]>1)
                        s+=1500-waterfall_count(*it2);
                    else
                        s+=distance[*it2]-pebbles[*it2];
                    costs[*it2] <?= s;
                    layers[(l+1)%2].insert(*it2);
                }
            }
        }
    }
    int vv = 0, best_c=INF;
    if (layers[(l-1)%2].count(0)==0) {
        TR(layers[(l-1)%2],it) {
            if (costs[*it]<best_c) {
                best_c=costs[*it];
                vv=*it;
            }
        }
    }
    while (vv!=v) {
        p.push_back(vv);
        int best_c=INF, best_v=-1;
        TR(adjacent[vv],it) {
            if (distance[*it]>distance[vv] && costs[*it]<best_c) {
                best_c=costs[*it];
                best_v=*it;
            }
        }
        assert(best_v!=-1);
        vv=best_v;
    }
    p.push_back(v);
    reverse(ALL(p));
}

vector<short> find_path(int v) {
    vector<short> p;
    find_best_cost_shortest_target_0_path(p,v);
    if (p.size()==1+pebbles[v] && p.back() != 0)
        ++point_reasons["Path too short."];
    if (!attempt_extend_path(p))
        assert(false);
    return p;
}

int find_next_move() {
    int best_v=-1, best_s=INT_MAX;
    for (int v = 1; v < N; ++v) {
        if (pebbles[v]>0) {
            int s=0;
            if (pebbles[v]>distance[v])
                s = distance[v];
            else if (pebbles[v]==distance[v])
                s = 1000*distance[v];
            /*
            if (pebbles[v]==distance[v])
                s = distance[v];
            else if (pebbles[v]>distance[v])
                s = 1000*distance[v];
            */ else
                s = 1000000*(1000-distance[v])-pebbles[v];

            if (s<best_s) best_s=s, best_v=v;
        }
    }
    return best_v;
}

void move(int _N, const vector<int>& _u, const vector<int>& _v, const vector<int>& _pebbles) {
    N=_N;
    int n = _u.size();
    for (int i = 0; i < n; ++i) {
        if (count(ALL(adjacent[_u[i]]),_v[i])==0)
            adjacent[_u[i]].push_back(_v[i]);
        if (count(ALL(adjacent[_v[i]]),_u[i])==0)
            adjacent[_v[i]].push_back(_u[i]);
    }
    copy(ALL(_pebbles),pebbles);

    find_distances();

    for (;;) {
        int v = find_next_move();
        if (v == -1)
            break;

        //cerr<<"next move is "<<v<<", count on 3 is "<<pebbles[3]<<endl;

        if (pebbles[v]<distance[v]) {
            deque<pair<int,int> > singles;
            TR(adjacent[v],it)
                if (pebbles[*it]==1)
                    singles.push_back(make_pair(distance[*it],*it));
            if (pebbles[v]+singles.size()>=distance[v]) {
                sort(ALL(singles),greater<pair<int, int> >());
                while (pebbles[v]<distance[v]) {
                    vector<short> p2;
                    p2.push_back(singles.front().second);
                    p2.push_back(v);
                    add_path(p2);
                    singles.pop_front();
                }
            }
        }

        vector<short> p = find_path(v);
        add_path(p);
    }

    static char path[1000*5];
    TR(paths,it) {
        char *scan=path;
        TR(*it,itp)
            scan += sprintf(scan, "%d ", (int)*itp);
        *--scan=0;
        result.push_back(path);
    }
    TR(point_reasons,it)
        cerr<<"REASON:"<<it->first<<":"<<it->second<<endl;
}

END

struct GraphMancala {
    inline vector<string>& move(int N, const vector<int>& u, const vector<int>& v, const vector<int>& pebbles) {
        MM28::move(N,u,v,pebbles);
        return MM28::result;
    }
};
