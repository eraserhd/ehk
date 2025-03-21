#include <algorithm>
#include <string>
#include <vector>
#include <iostream>
#include <sstream>
using namespace std;

struct WaterTank {
    double minOutputRate(vector <int> t, vector <int> x, int C) {
        int N = t.size();
        double low = 0.0, high = 1000000.0 * 1000000.0 * 50.0 * 5.0;
        for (int j = 0; j < 1000; ++j) {
            double mid = low/2 + high/2;

            double a = 0.0;
            bool ok = true;
            for (int i = 0; i < N; ++i) {
                a += double(t[i]) * (double(x[i]) - mid);
                if (a > C) {
                    ok = false;
                    break;
                }
                a = max(0.0, a);
            }

            if (ok)
                high = mid;
            else
                low = mid;
        }
        return low;
    }

    
// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); if ((Case == -1) || (Case == 5)) test_case_5(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const double &Expected, const double &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arr0[] = {3,3}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {1,2}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 3; double Arg3 = 0.9999999999999999; verify_case(0, Arg3, minOutputRate(Arg0, Arg1, Arg2)); }
	void test_case_1() { int Arr0[] = {1,2,3,4,5}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {5,4,3,2,1}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 10; double Arg3 = 1.9999999999999996; verify_case(1, Arg3, minOutputRate(Arg0, Arg1, Arg2)); }
	void test_case_2() { int Arr0[] = {5949,3198,376,3592,4019,3481,5609,3840,6092,4059}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {29,38,96,84,10,2,39,27,76,94}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 1000000000; double Arg3 = 0.0; verify_case(2, Arg3, minOutputRate(Arg0, Arg1, Arg2)); }
	void test_case_3() { int Arr0[] = {9,3,4,8,1,2,5,7,6}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {123,456,789,1011,1213,1415,1617,1819,2021}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 11; double Arg3 = 2019.1666666666665; verify_case(3, Arg3, minOutputRate(Arg0, Arg1, Arg2)); }
	void test_case_4() { int Arr0[] = {100}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {1000}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 12345; double Arg3 = 876.55; verify_case(4, Arg3, minOutputRate(Arg0, Arg1, Arg2)); }
	void test_case_5() {
            int Arr0[] = {1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000};
        vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0])));
        int Arr1[] = {1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,
        }; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 1; double Arg3 = 876.55; verify_case(4, Arg3, minOutputRate(Arg0, Arg1, Arg2)); }

        // Fail: no max(0.0,a)  (test case #3)
        // Fail: precision
        // Fail: high is too low

// END CUT HERE

};

// BEGIN CUT HERE
int main() {
    WaterTank ___test;
    ___test.run_test(-1);
}
// END CUT HERE
