#include <algorithm>
#include <iostream>
#include <string>
using namespace std;

int T;
string A, B;
short dp[2005][2005];

int main() {
    cin >> T;
    for (int t = 0; t < T; ++t) {
        cin >> A >> B;
        dp[0][0] = 0;
        for (int i = 1; i < 2003; ++i)
            dp[i][0] = dp[0][i] = i;

        for (int i = 1; i < A.size()+1; ++i)
            for (int j = 1; j < B.size()+1; ++j)
                dp[i][j] = min(min(1 + dp[i-1][j], 1 + dp[i][j-1]),
                               dp[i-1][j-1] + (A[i-1] == B[j-1] ? 0 : 1));

        cout << dp[A.size()][B.size()] << endl;
    }
    return 0;
}

