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

vector<int> adjacent[1000];
vector<string> result;

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
    vector<int> r = adjacent[s];
    random_shuffle(ALL(r));
    TR(r,it) {
        if (dfs_seen[*it])
            continue;
        if (t==0 && distance[*it]>l)
            continue;
        dfs_to[s]=*it;
        if (dfs_find_path(*it,t,l-1))
            return true;
    }
    dfs_seen[s]=0;
    return false;
}

bool attempt_extend_path(vector<int>& p) {
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

    if (p.size()<np && np<15) {
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
        vector<int> t = adjacent[lv];
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
        cerr<<"failed to find path of size "<<np<<" from "<<p.front()<<" to "<<p.back()<<endl;
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

void find_best_cost_shortest_target_0_path(vector<int>& p, int v) {
    int costs[1000], l;
    fill(costs,costs+N,INF);
    costs[v]=0;
    set<int> layers[2];
    layers[0].insert(v);
    for (l = 0; l <= min(distance[v],pebbles[v]); ++l) {
        layers[(l+1)%2].clear();
        TR(layers[l%2],it) {
            TR(adjacent[*it],it2) {
                if (distance[*it2]<distance[*it]) {
                    int s = costs[*it];
                    if (pebbles[*it2]>=distance[*it2])
                        s+=1000+(pebbles[*it2]-distance[*it2]);
                    else if (pebbles[*it2]==0 && distance[*it2]>1)
                        s+=50;
                    else
                        s+=1;
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

vector<int> find_path(int v) {
    vector<int> p;
    find_best_cost_shortest_target_0_path(p,v);
    if (!attempt_extend_path(p))
        assert(false);
    return p;
}

int find_next_move() {
    int best_v=-1, best_s=INT_MAX;
    for (int v = 1; v < N; ++v) {
        if (pebbles[v]>0) {
            int s=0;
            if (pebbles[v]==distance[v])
                s = distance[v];
            else if (pebbles[v]>distance[v])
                s = 1000*distance[v];
            else
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
        adjacent[_u[i]].push_back(_v[i]);
        adjacent[_v[i]].push_back(_u[i]);
    }
    copy(ALL(_pebbles),pebbles);

    find_distances();

    static char path[1000*5];
    for (;;) {
        int v = find_next_move();
        if (v == -1)
            break;

        if (pebbles[v]<distance[v]) {
            int a=0;
            TR(adjacent[v],it)
                if (pebbles[*it]==1)
                    ++a;
            if (pebbles[v]+a>=distance[v]) {
                TR(adjacent[v],it) {
                    if (pebbles[*it]==1) {
                        --pebbles[*it];
                        ++pebbles[v];
                        sprintf(path, "%d %d", *it, v);
                        result.push_back(path);
                        if (pebbles[v]>=distance[v])
                            break;
                    }
                }
            }
        }

        vector<int> p = find_path(v);

        int np = pebbles[v];

        char *scan=path;
        pebbles[p.front()]=-1;
        TR(p,it)
            scan += sprintf(scan, "%d ", *it), ++pebbles[*it];
        *--scan=0;
        assert(pebbles[v]==0);
        assert(v==p.front());
        assert(p.size()==np+1);

        result.push_back(path);
    }
}

END

struct GraphMancala {
    inline vector<string>& move(int N, const vector<int>& u, const vector<int>& v, const vector<int>& pebbles) {
        MM28::move(N,u,v,pebbles);
        return MM28::result;
    }
};
