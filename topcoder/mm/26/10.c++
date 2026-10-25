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

double Pr_n_samples_ge_x(int n, double x) {
    return 1.-(1.+erf(x/(sqrt(n)*sqrt(2))))/2.;
}

const double REJECT_PERCENT = 0.000005;
double reject_table[2*1000];

inline bool is_reject(int n, double x) {
    return x<reject_table[n];
}

inline void compute_partial_delta_leg(double& sum, int a) {
    for (int i = 0; i < a; ++i)
        sum += W[p[i]*N+p[a]];
    for (int j = a+1; j < N; ++j)
        sum += W[p[a]*N+p[j]];
}

inline double cache_partial_delta_leg(int a) {
    double& sum = cache[a];
    if (cacheflag[a]!=cachen) {
        sum = 0.;
        for (int i = 0; i < a; ++i)
            sum += W[p[i]*N+p[a]];
        for (int j = a+1; j < N; ++j)
            sum += W[p[a]*N+p[j]];
        cacheflag[a]=cachen;
    }
    return sum;
}

bool delta_gt_zero(int a, int b) {
    if (a>b)
        swap(a,b);
    double sum = -cache_partial_delta_leg(a) - cache_partial_delta_leg(b);
    swap(p[a],p[b]);
    int n = 2*(N-1);
    if ((end.tv_sec-now.tv_sec)>20 && is_reject(n,sum))
        return false;
    compute_partial_delta_leg(sum,a);
    compute_partial_delta_leg(sum,b);
    swap(p[a],p[b]);
    return sum>0.;
}

double jmeans[1000];

bool jmean_greater(int a, int b) {
    return jmeans[a] > jmeans[b];
}

void fill_reject_table() {
    for (int n = 0; n < 2*N; ++n) {
        double lo = 0., hi = n, mid;
        for (int i = 0; i < 100; ++i) {
            mid = (lo+hi)/2.;
            double ans = Pr_n_samples_ge_x(n,mid);
            if (ans > REJECT_PERCENT)
                lo = mid;
            else
                hi = mid;
        }
        reject_table[n]=-mid;
    }
}

vector<int> findOrder(const vector<double>& w) {
    gettimeofday(&end,NULL);
    end.tv_sec += SECONDS;
    if ((end.tv_usec += 750000)>1000000) {
        end.tv_usec-=1000000;
        ++end.tv_sec;
    }

    best = -100000000.;
    N = (int)(sqrt(w.size())+0.1);
    copy(ALL(w),W);

    for (int i = 0; i < N; ++i)
        result[i] = p[i] = i;

    fill_reject_table();

    for (int i = 0; i < N; ++i) {
        jmeans[i]=0.;
        for (int j = 0; j < N; ++j) {
            jmeans[i] += W[p[i]*N+p[j]];
        }
    }
    sort(p,p+N,jmean_greater);
    copy(p,p+N,result);

    while (have_time()) {
        int a = rand()%N, b = rand()%(N-1);
        if (b>=a)
            ++b;
        if (delta_gt_zero(a,b)) {
            swap(p[a],p[b]);
            copy(p,p+N,result);
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
