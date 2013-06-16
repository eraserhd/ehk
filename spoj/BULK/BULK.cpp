#include <algorithm>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <queue>
#include <set>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

struct Face {
    int z;
    vector<pair<int, int> > points;

    inline bool operator < (Face const& rhs) const {
        if (z < rhs.z) return true;
        if (z > rhs.z) return false;
        return points < rhs.points;
    }
};

vector<Face> faces;

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

bool inside(pair<int,int> const& point, vector<pair<int, int> > const& points) {
    set<int> x_scan;

    for (int i = 0; i < points.size(); ++i) {
        pair<int, int> a = points[i];
        pair<int, int> b = points[(i+1)%points.size()];

        if (a.second == b.second) continue; // skip because horizontal

        if (a.second > b.second) swap(a,b); // make ys ascending

        if (a.second <= point.second && b.second > point.second)
            x_scan.insert(a.first);
    }

    int edges_to_left = 0;
    typeof(x_scan.begin()) x_sweep = x_scan.begin();
    while (x_sweep != x_scan.end() && *x_sweep <= point.first) {
        ++edges_to_left;
        ++x_sweep;
    }

    return edges_to_left&1;
}

int solve() {
    sort(ALL(faces));

    typeof(faces.begin()) z_sweep = faces.begin();
    int last_z = -1;
    int volume = 0;
    int current_area = 0;

    while (z_sweep != faces.end()) {
        int z = z_sweep->z;
        int z_dist = z - last_z;

        volume += z_dist * current_area;

        while (z == z_sweep->z) {
            int a = area(z_sweep->points);

            pair<int, int> inside_point = *min_element(ALL(z_sweep->points));

            bool start = true;
            typeof(faces.begin()) z_sweep_2 = faces.begin();
            while (z_sweep_2 != z_sweep) {
                if (inside(inside_point, z_sweep_2->points))
                    start = !start;
                ++z_sweep_2;
            }

            current_area += a * (start ? 1 : -1);
            ++z_sweep;
        }

        last_z = z;
    }

    return volume;
}

int main() {
    int T;
    scanf("%d", &T);
    for (int t = 0; t < T; ++t) {
        int F;
        scanf("%d", &F);
        faces.clear();

        for (int f = 0; f < F; ++f) {
            int P;
            scanf("%d", &P);

            bool z_face = true;

            Face face;
            face.z = -1;

            for (int p = 0; p < P; ++p) {
                int x,y,z;
                scanf("%d%d%d",&x,&y,&z);
                face.points.push_back(make_pair(x,y));

                if (face.z == -1)
                    face.z = z;
                else if (face.z != z)
                    z_face = false;
            }

            if (z_face)
                faces.push_back(face);
        }

        printf("The bulk is composed of %d units.\n", solve());
    }
    return 0;
}

