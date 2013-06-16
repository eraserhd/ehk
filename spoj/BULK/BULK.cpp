#include <algorithm>
#include <cstring>
#include <iostream>
#include <queue>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

vector<pair<int, vector<pair<int, int> > > > faces;

bool crosses(vector<pair<int, int> > const& points, pair<int, int> a, pair<int, int> b) {
    for (int i = 0; i < points.size(); ++i) {
        pair<int, int> const& c = points[i];
        pair<int, int> const& d = points[(i+1)%points.size()];

        if (c.first == d.first) {
            if (a.first == b.first) continue;
            if (a.first >= c.first && b.first >= c.first) continue;
            if (a.first < c.first && b.first < c.first) continue;
            if (a.second < min(c.second, d.second)) continue;
            if (a.second >= max(c.second, d.second)) continue;
            return true;
        } else {
            if (a.second == b.second) continue;
            if (a.second >= c.second && b.second >= c.second) continue;
            if (a.second < c.second && b.second < c.second) continue;
            if (a.first < min(c.first, d.first)) continue;
            if (a.first >= max(c.first, d.first)) continue;
            return true;
        }
    }
    return false;
}

char A[1024][1024];
int area(vector<pair<int, int> > const& points) {
    memset(A, 0, sizeof(A));

    queue<pair<int, int> > q;
    A[0][0] = 1;
    q.push(make_pair(0,0));
    int filled = 1;

    while (!q.empty()) {
        pair<int, int> t = q.front(); q.pop();

        const int DX[] = {-1,1,0,0};
        const int DY[] = {0,0,-1,1};

        for (int d = 0; d < 4; ++d) {
            pair<int, int> nt(t.first + DX[d], t.second + DY[d]);
            if (nt.first < 0 || nt.second < 0 || nt.first >= 1024 || nt.second >= 1024)
                continue;
            if (A[nt.first][nt.second])
                continue;
            if (crosses(points, t, nt))
                continue;

            A[nt.first][nt.second] = 1;
            q.push(nt);
            ++filled;
        }
    }

    return 1024*1024 - filled;
}

int solve() {
    sort(ALL(faces));

    TR(faces, it) {
        cout << "A:" << area(it->second) <<"/";
        cout << "Z:" << it->first << " ";
        TR(it->second, p_it) {
            cout << "(" << p_it->first << "," << p_it->second << ") ";
        }
        cout << endl;
    }
    return 0;
}

int main() {
    int T;
    cin >> T;
    for (int t = 0; t < T; ++t) {
        int F;
        cin >> F;
        faces.clear();

        for (int f = 0; f < F; ++f) {
            int P;
            cin >> P;

            int first_z = -1;
            bool z_face = true;
            vector<pair<int, int> > points;

            for (int p = 0; p < P; ++p) {
                int x,y,z;
                cin >> x >> y >> z;
                points.push_back(make_pair(x,y));

                if (first_z == -1)
                    first_z = z;
                else if (first_z != z)
                    z_face = false;
            }

            if (z_face)
                faces.push_back(make_pair(first_z, points));
        }

        cout << "The bulk is composed of " << solve() << " units." << endl;
    }
    return 0;
}

