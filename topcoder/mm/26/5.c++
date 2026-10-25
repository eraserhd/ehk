#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <iterator>
#include <limits.h>
#include <sys/time.h>
#include <time.h>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }

namespace MM26 BEGIN

const int SECONDS = 29;
const int UPPER_ESCHELON = 1;
const double EPSILON = 1e-9;

int N;
struct timeval now, end;
double W[1000*1000];
int p[1000];
int result[1000];

bool best_accurate=false;
double best;

bool have_time() {
    gettimeofday(&now,NULL);
    return timercmp(&now,&end,<);
}

double compute_score(int p[]) {
    double sum = 0.;
    for (int j = 0; j < N; ++j)
        for (int i = 0; i < j; ++i)
            sum += W[p[i]*N+p[j]];
    return sum;
}

int     cacheflag[1000] = {};
int     cachen = 1;
double  cache[1000];
int     cacheiflag[1000*1000] = {};
double  cachei[1000*1000];

inline double compute_partial_delta_leg(int a) {
    double sum = 0.;
    for (int i = 0; i < a; ++i)
        sum += W[p[i]*N+p[a]];
    for (int j = a+1; j < N; ++j)
        sum += W[p[a]*N+p[j]];
    return sum;
}

inline double cache_partial_delta_leg(int a) {
    double& sum = cache[a];
    if (cacheflag[a]!=cachen) {
        sum = compute_partial_delta_leg(a);
        cacheflag[a]=cachen;
    }
    return sum;
}

double compute_delta(int a, int b) {
    if (a>b)
        swap(a,b);
    swap(p[a],p[b]);
    double sum = compute_partial_delta_leg(a) + compute_partial_delta_leg(b);
    swap(p[a],p[b]);
    sum -= cache_partial_delta_leg(a) + cache_partial_delta_leg(b);
    return sum;
}

vector<int> findOrder(const vector<double>& w) {
    gettimeofday(&end,NULL);
    end.tv_sec += SECONDS;

    best = -100000000.;
    N = (int)(sqrt(w.size())+0.1);
    copy(ALL(w),W);

    for (int i = 0; i < N; ++i)
        result[i] = p[i] = i;
    while (have_time()) {
        int a = rand()%N, b = rand()%(N-1);
        if (b>=a)
            ++b;
        double delta = compute_delta(a,b);
        if (delta>0.0) {
            copy(p,p+N,result);
            swap(p[a],p[b]);
            ++cachen;
        }
    }
    return vector<int>(result,result+N);
}

END

struct Permute {
    inline vector<int> findOrder(const vector<double>& w) {
        return MM26::findOrder(w);
    }
};
