#include <algorithm>
#include <string>
#include <vector>
#include <iostream>
#include <iterator>
#include <list>
#include <sstream>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

struct TeamContest {
    int worstRank(vector <int> strength) {
        int ours = max(max(strength[0],strength[1]),strength[2]) +
                   min(min(strength[0],strength[1]),strength[2]);

        int rank = 1;

        strength.erase(strength.begin(),strength.begin()+3);
        sort(ALL(strength));
        bool used[55] = {};

        int left = strength.size();
        while (left > 0) {

            int strongest;
            for (int i = int(strength.size())-1; i >= 3; --i)
                if (!used[i]) {
                    used[i] = true;
                    strongest = strength[i];
                    break;
                }

            bool found1 = false;
            for (int i = 0; i < strength.size(); ++i)
                for (int j = i+1; j < strength.size(); ++j)
                    if (!used[i] && !used[j] && strongest + strength[i] > ours) {
                        used[i] = true;
                        used[j] = true;
                        ++rank;
                        found1 = true;
                        //cerr << strength[i] << ","<<strength[j]<<","<<strongest<<endl;
                        goto done;
                    }
done:
            
            if (!found1) {
                for (int i = 0; i < strength.size(); ++i)
                    if (!used[i]) {
                        used[i] = true;
                        break;
                    }
                for (int i = 0; i < strength.size(); ++i)
                    if (!used[i]) {
                        used[i] = true;
                        break;
                    }
            }

            left -= 3;
        }

        return rank;
        
    }

    
// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arr0[] = {5, 7, 3, 5, 7, 3, 5, 7, 3}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 2; verify_case(0, Arg1, worstRank(Arg0)); }
	void test_case_1() { int Arr0[] = {5, 7, 3}
; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(1, Arg1, worstRank(Arg0)); }
	void test_case_2() { int Arr0[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 1; verify_case(2, Arg1, worstRank(Arg0)); }
	void test_case_3() { int Arr0[] = {3,9,4,6,2,6,1,6,9,1,4,1,3,8,5}
; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 3; verify_case(3, Arg1, worstRank(Arg0)); }
	void test_case_4() { int Arr0[] = {53,47,88,79,99,75,28,54,65,14,22,13,11,31,43}
; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arg1 = 3; verify_case(4, Arg1, worstRank(Arg0)); }

// END CUT HERE

};

// BEGIN CUT HERE
int main() {
    TeamContest ___test;
    ___test.run_test(-1);
}
// END CUT HERE
