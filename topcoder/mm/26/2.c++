#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <iterator>
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

int N;
struct timeval end;
vector<double> W;
double best;
vector<int> result;

bool have_time() {
    struct timeval now;
    gettimeofday(&now,NULL);
    return timercmp(&now,&end,<);
}

double score(const vector<int>& p) {
    double sum = 0.;
    for (int j = 0; j < N; ++j)
        for (int i = 0; i < j; ++i)
            sum += W[p[i]*N+p[j]];
    if (sum > best) {
        best = sum;
        result = p;
    }
    return sum;
}

vector<int> findOrder(const vector<double>& w) {
    gettimeofday(&end,NULL);
    end.tv_sec += SECONDS;

    best = -100000000.;
    W = w;
    N = (int)(sqrt(w.size())+0.1);
    vector<int> p(N);
    for (int i = 0; i < N; ++i)
        p[i] = i;
    double best_score = score(p);
    while (have_time()) {
        int a = rand()%N, b = rand()%(N-1);
        if (b>=a)
            ++b;
        swap(p[a],p[b]);
        double s = score(p);
        if (s>best_score)
            best_score = s;
        else
            swap(p[a],p[b]);
    }
    return result;
}

END

struct Permute {
    inline vector<int> findOrder(const vector<double>& w) {
        return MM26::findOrder(w);
    }
};
