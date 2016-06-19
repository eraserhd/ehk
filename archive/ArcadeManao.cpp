#include <cstdlib>
#include <string>
#include <vector>
#include <iostream>
#include <queue>
#include <set>
#include <sstream>
#include <utility>
using namespace std;

struct ArcadeManao {
    int shortestLadder(vector <string> level, int coinRow, int coinColumn) {
        --coinRow;
        --coinColumn;
        int N = level.size();
        for (int L = 0; L < 55; ++L) {
            queue<pair<int, int> > q;
            set<pair<int,int> > seen;

            q.push(make_pair(level.size()-1, 0));
            seen.insert(make_pair(level.size()-1, 0));

            while (!q.empty()) {
                pair<int, int> p = q.front(); q.pop();
                if (p.first == coinRow && p.second == coinColumn)
                    return L;

                const int DJ[] = {-1,1};
                for (int d = 0; d < 2; ++d) {
                    pair<int, int> next_p(p.first, p.second + DJ[d]);
                    if (next_p.second < 0 || next_p.second >= level[0].size())
                        continue;

                    if (level[next_p.first][next_p.second] != 'X')
                        continue;

                    if (seen.count(next_p))
                        continue;

                    seen.insert(next_p);
                    q.push(next_p);
                }

                for (int li = 0; li < N; ++li) {
                    if (li == p.first)
                        continue;
                    if (level[li][p.second] != 'X')
                        continue;
                    if (abs(li-p.first) > L)
                        continue;

                    pair<int, int> next_p(li, p.second);
                    if (seen.count(next_p))
                        continue;

                    seen.insert(next_p);
                    q.push(next_p);
                }
            }
        }
        return -1;
    }

    

};



// Powered by FileEdit
// Powered by TZTester 1.01 [25-Feb-2003]
// Powered by CodeProcessor
