#include <algorithm>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <ext/algorithm>
#include <iostream>
#include <iterator>
#include <limits.h>
#include <queue>
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

int sp_from[1000];

vector<int> adjacent[1000];
vector<string> result;

void find_distances() {
    char seen[1000]={};
    seen[0]=1;
    memset(sp_from,-1,sizeof(sp_from));
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
                sp_from[*it]=v;
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
        for (int a = (int)p.size()-2; a >= 0; --a) {
            TR(adjacent[p[a]],it) {
                if (count(ALL(p),*it)==0 && count(ALL(adjacent[p[a+1]]),*it)!=0) {
                    p.insert(p.begin()+a+1,*it);
                    goto next;
                }
            }
        }
        break;
next:;
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

    if (p.size()<np && np<20) {
        memset(dfs_seen,0,sizeof(dfs_seen));
        if (dfs_find_path(p.front(),p.back(),np)) {
            cerr<<"DFS found!"<<endl;
            int v = p.front();
            p.clear();
            for (int l = 0; l < np; ++l) {
                p.push_back(v);
                v = dfs_to[v];
            }
        }
    }

    if (p.size()<np) {
        cerr<<"failed to find path of size "<<np<<" from "<<p.front()<<" to "<<p.back()<<endl;
        cerr<<"  (found "<<p.size()<<")"<<endl;

        // Find _any_ path of this length to scatter the stones
        memset(dfs_seen,0,sizeof(dfs_seen));
        if (dfs_find_path(p.front(),-1,np)) {
            cerr<<"DFS found!"<<endl;
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

vector<int> get_sp(int v) {
    int np = pebbles[v]+1;
    assert(np>0);
    vector<int> p;
    while (v!=-1 && np--) {
        p.push_back(v);
        v=sp_from[v];
    }
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
                s = 1000000*(1000-distance[v]);

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

    for (;;) {
        int v = find_next_move();
        if (v == -1)
            break;

        vector<int> p = get_sp(v);
        if (!attempt_extend_path(p))
            assert(false);

        int np = pebbles[v];

        static char path[1000*5];
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
