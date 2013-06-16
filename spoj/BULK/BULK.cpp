#include <algorithm>
#include <cstring>
#include <iostream>
#include <queue>
#include <set>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

vector<pair<int, vector<pair<int, int> > > > faces;

int area(vector<pair<int, int> > const& points) {

    vector<pair<int, int> > events; // at y, toggle x

    for (int i = 0; i < points.size(); ++i) {
        pair<int, int> const& a = points[i];
        pair<int, int> const& b = points[(i+1)%points.size()];

        if (a.second == b.second) continue; // horizontal edge

        events.push_back(make_pair(a.second, a.first));
        events.push_back(make_pair(b.second, b.first));
    }

    sort(ALL(events));

    int area = 0;
    int last_y = -1;
    set<int> x_scan;

    typeof(events.begin()) y_sweep = events.begin();
    while (y_sweep != events.end()) {
        int y = y_sweep->first;

        int y_dist = y - last_y;
        int x_area = 0;
        typeof(x_scan.begin()) x_sweep = x_scan.begin();
        while (x_sweep != x_scan.end()) {
            int x1 = *x_sweep++;
            int x2 = *x_sweep++;
            
            x_area += (x2 - x1);
        }

        //cerr << "Adding " << x_area << " * " << y_dist << endl;
        area += x_area * y_dist;

        while (y == y_sweep->first && y_sweep != events.end()) {
            if (x_scan.count(y_sweep->second) > 0)
                x_scan.erase(y_sweep->second);
            else
                x_scan.insert(y_sweep->second);
            ++y_sweep;
        }

        last_y = y;
    }

    return area;
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

