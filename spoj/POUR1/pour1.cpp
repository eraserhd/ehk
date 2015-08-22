#include <algorithm>
#include <iostream>
#include <queue>
#include <set>
#include <utility>
using namespace std;

int main() {
    int N;
    cin >> N;
    for (int i = 0; i < N; ++i) {
        int A, B, C;
        cin >> A >> B >> C;

        set<pair<int, int> > seen;
        queue<pair<int, pair<int, int> > > q;
        seen.insert(make_pair(0, 0));
        q.push(make_pair(0, make_pair(0, 0)));
        int answer = -1;
        while (!q.empty()) {
            pair<int, pair<int, int> > t = q.front(); q.pop();
            if (t.second.first == C || t.second.second == C) {
                answer = t.first;
                break;
            }

            pair<int, pair<int, int> > next = make_pair(t.first + 1, make_pair(0, t.second.second));
            if (!seen.count(next.second)) {
                seen.insert(next.second);
                q.push(next);
            }
            next.second = make_pair(t.second.first, 0);
            if (!seen.count(next.second)) {
                seen.insert(next.second);
                q.push(next);
            }
            next.second = make_pair(A, t.second.second);
            if (!seen.count(next.second)) {
                seen.insert(next.second);
                q.push(next);
            }
            next.second = make_pair(t.second.first, B);
            if (!seen.count(next.second)) {
                seen.insert(next.second);
                q.push(next);
            }

            int X = min(t.second.first, B - t.second.second);
            next.second = make_pair(t.second.first - X, t.second.second + X);
            if (!seen.count(next.second)) {
                seen.insert(next.second);
                q.push(next);
            }

            X = min(t.second.second, A - t.second.first);
            next.second = make_pair(t.second.first + X, t.second.second - X);
            if (!seen.count(next.second)) {
                seen.insert(next.second);
                q.push(next);
            }
        }
        cout << answer << endl;
    }
    return 0;
}

