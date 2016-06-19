#include <algorithm>
#include <functional>
#include <string>
#include <vector>
#include <iostream>
#include <queue>
#include <sstream>
#include <utility>
using namespace std;
const long long INF = 1LL<<63;

struct SkiResorts {
    long long minCost(vector <string> road, vector <int> altitude) {
        priority_queue< pair<long long, int>, vector<pair<long long, int> >, greater<pair<long long, int> > > q;
        q.push(make_pair(0,0));
        int N = altitude.size();

        long long costs[55];
        fill(costs,costs+55,INF);
        costs[0] = 0LL;

        while (!q.empty()) {
            pair<long long, int> t = q.top(); q.pop();

            if (t.first > costs[t.second])
                continue;
            for (int i = 0; i < N; ++i)
                if (i != t.second && road[i][t.second] == 'Y') {
                    long long c = costs[t.second];
                    if (altitude[i] > altitude[t.second])
                        c += altitude[i] - altitude[t.second];
                    if (costs[i] > c) {
                        costs[i] = c;
                        q.push(make_pair(c,i));
                    }
                }
        }

        if (costs[N-1] == INF)
            return -1;
        return costs[N-1];
    }

    
// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const long long &Expected, const long long &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arr0[] = {"NYN",
 "YNY",
 "NYN"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {30, 20, 10}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); long long Arg2 = 0LL; verify_case(0, Arg2, minCost(Arg0, Arg1)); }
	void test_case_1() { string Arr0[] = {"NY",
 "YN"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {10, 20}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); long long Arg2 = 10LL; verify_case(1, Arg2, minCost(Arg0, Arg1)); }
	void test_case_2() { string Arr0[] = {"NYN",
 "YNN",
 "NNN"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {573, 573, 573}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); long long Arg2 = -1LL; verify_case(2, Arg2, minCost(Arg0, Arg1)); }
	void test_case_3() { string Arr0[] = {"NNYNNYYYNN",
 "NNNNYNYNNN",
 "YNNNNYYNNN",
 "NNNNNNYNYY",
 "NYNNNNNNYY",
 "YNYNNNNYNN",
 "YYYYNNNYNN",
 "YNNNNYYNNN",
 "NNNYYNNNNN",
 "NNNYYNNNNN"}; vector <string> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {7, 4, 13, 2, 8, 1, 8, 15, 5, 15}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); long long Arg2 = 12LL; verify_case(3, Arg2, minCost(Arg0, Arg1)); }

// END CUT HERE

};

// BEGIN CUT HERE
int main() {
    SkiResorts ___test;
    ___test.run_test(-1);
}
// END CUT HERE
