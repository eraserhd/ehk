#include <cassert>
#include <cmath>
#include <cstring>
#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <vector>
using namespace std;
#define BEGIN {
#define END }

namespace MM19 BEGIN

int noOfRovers;
vector<int> mineralA, mineralB;
vector<string> result;

int low_map[2][100][100];
int really_low_map[2][10][10];

void add_waypoint(int rover, double x, double y) {
    static int waypoints = 0;
    ++waypoints;
    assert(waypoints <= 1000);

    ostringstream out;
    out << rover << " " << (int)x << " " << (int)y;
    result.push_back(out.str());
}

void run() {
    result.reserve(1000*noOfRovers);
    memset(low_map, 0, sizeof(low_map));
    for (int i = 0; i < 1000; ++i)
        for (int j = 0; j < 1000; ++j) {
            low_map[0][i/10][j/10] += mineralA[i*1000+j];
            low_map[1][i/10][j/10] += mineralB[i*1000+j];
            really_low_map[0][i/100][j/100] += mineralA[i*1000+j];
            really_low_map[1][i/100][j/100] += mineralB[i*1000+j];
        }

    int collected[2] = {0,0};
    for (int n = 0; n < noOfRovers; ++n) {
        if (result.size() > 998)
            continue; // Don't use this robot, no waypoints left
        double fuel = 1999.;
        int x = 500, y = 500;
        int m = collected[0] < collected[1] ? 0 : 1;
        
        // Find the highest density of the mineral we need.
        int highest = 0, highest_i = -1, highest_j = -1;
        for (int i = 0; i < 10; ++i)
            for (int j = 0; j < 10; ++j)
                if (really_low_map[m][i][j] > highest) {
                    highest = really_low_map[m][i][j];
                    highest_i = i;
                    highest_j = j;
                }

        really_low_map[m][highest_i][highest_j] = 0;

        // Go to inner corner of 100x100 cell with the highest density of our mineral
        if (highest_i > 4)
            y = 100*highest_i;
        else
            y = 100*highest_i + 99;
        if (highest_j > 4)
            x = 100*highest_j;
        else
            x = 100*highest_j + 99;
        add_waypoint(n,x,y);
        fuel -= sqrt((500-x)*(500-x)+(500-y)*(500-y));

        for (;;) {
            int best_x = -1, best_y = -1;
            double best_s = 0.0, best_f = 0.0;
            if (result.size() > 998)
                break;
            for (int i = 0; i < 100; ++i)
                for (int j = 0; j < 100; ++j) {
                    double f = sqrt((x-j*10)*(x-j*10) + (y-i*10)*(y-i*10));
                    if (f > fuel)
                        continue;
                    double s = (double)low_map[m][i][j]/f;
                    double fb = sqrt((j*10-500)*(j*10-500) + (i*10-500)*(i*10-500));
                    if (f+fb < fuel && s > best_s) {
                        best_s = s;
                        best_x = j*10;
                        best_y = i*10;
                        best_f = f;
                    }
                }
            if (best_x == -1)
                break;
            add_waypoint(n, best_x, best_y);
            x = best_x, y = best_y;
            for (int c = 0; c < 2; ++c) {
                collected[c] += low_map[c][y/10][x/10];
                low_map[c][y/10][x/10] = 0;
            }
            fuel -= best_f;
        }

        add_waypoint(n, 500, 500);
    }
}

END

struct MarsRover {
    inline vector<string> generateWaypoints(int noOfRovers, const vector<int>& mineralA,
                                            const vector<int>& mineralB) {
        MM19::noOfRovers = noOfRovers;
        MM19::mineralA = mineralA;
        MM19::mineralB = mineralB;
        MM19::run();
        return MM19::result;
    }
};
