#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>
#include <sys/time.h>
#include <time.h>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }

namespace MM26 BEGIN

int N;
struct timeval end;

bool have_time() {
    struct timeval now;
    gettimeofday(&now,NULL);
    return timercmp(&now,&end,<);
}

vector<int> findOrder(const vector<double>& w) {
    gettimeofday(&end,NULL);
    end.tv_sec += 28;

    N = (int)(sqrt(w.size())+0.1);
    vector<int> p(N);
    for (int i = 0; i < N; ++i)
        p[i] = i;
    vector<int> result = p;
    double best = -1000000000;
    while (have_time()) {
        double sum = 0.0;
        for (int j = 0; j < N; ++j)
            for (int i = 0; i < j; ++i)
                sum += w[p[i]*N+p[j]];
        if (sum > best) {
            best = sum;
            result = p;
        }
        random_shuffle(ALL(p));
    }
    return result;
}

END

struct Permute {
    inline vector<int> findOrder(const vector<double>& w) {
        return MM26::findOrder(w);
    }
};
