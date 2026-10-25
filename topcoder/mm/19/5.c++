#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>
#include <iostream>
#include <map>
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
int totals[2] = {0, 0};

int low_res_map[2][10][10];

void add_waypoint(int rover, double x, double y) {
    ostringstream out;
    out << rover << " " << (int)x << " " << (int)y;
    result.push_back(out.str());
}

const char COLLECTABLE[21*21] = {
0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
};

char already_collected[1000][1000];
void collect(int& total, vector<int>& m, int bit, int x, int y, bool zero = true) {
    for (int yy = 0; yy < 21; ++yy)
        for (int xx = 0; xx < 21; ++xx) {
            if (!COLLECTABLE[yy*21+xx])
                continue;
            int cx = x+xx-11, cy = y+yy-11;
            if (cx < 0 || cx >= 1000)
                continue;
            if (cy < 0 || cy >= 1000)
                continue;
            if (!zero && (already_collected[cx][cy] & bit) != 0)
                continue;
            already_collected[cx][cy] |= bit;
            int amount = m[cy*1000+cx];
            total += amount;
            if (zero) {
                m[cy*1000+cx] = 0;
                low_res_map[bit-1][cy/100][cx/100] -= amount;
                totals[bit-1] -= amount;
            }
        }
}

// Bresenham's line algorithm
void collect(int& totalA, int& totalB, int x1, int y1, int x2, int y2, bool zero = true) {
    if (!zero)
        memset(already_collected, 0, sizeof(already_collected));
    bool steep = abs(y1 - y2) > abs(x1 - x2);
    if (steep) {
        swap(x1,y1);
        swap(x2,y2);
    }
    if (x1 > x2) {
        swap(x1,x2);
        swap(y1,y2);
    }
    int deltax = x2-x1;
    int deltay = abs(y2-y1);
    int error = -deltax / 2;
    int ystep = (y1 < y2) ? 1 : -1, y = y1;
    for (int x = x1; x <= x2; ++x) {
        if (steep) {
            collect(totalA, mineralA, 1, y, x, zero);
            collect(totalB, mineralB, 2, y, x, zero);
        } else {
            collect(totalA, mineralA, 1, x, y, zero);
            collect(totalB, mineralB, 2, x, y, zero);
        }
        error += deltay;
        if (error>0) {
            y += ystep;
            error -= deltax;
        }
    }
}

void run() {
    result.reserve(1000*noOfRovers);
    for (int i = 0; i < 1000; ++i)
        for (int j = 0; j < 1000; ++j) {
            low_res_map[0][i/100][j/100] += mineralA[i*1000+j];
            low_res_map[1][i/100][j/100] += mineralB[i*1000+j];
            totals[0] += mineralA[i*1000+j];
            totals[1] += mineralB[i*1000+j];
        }

    double value_ratio = (double)totals[0]/(totals[0]+totals[1]);
    cerr <<"Original value_ratio = "<<value_ratio<<endl;

    int collected[2] = {0,0};
    double total_fuel = 1999.*noOfRovers;
    double total_fuel_used = 0.;
    for (int n = 0; n < noOfRovers; ++n) {
        if (result.size() > 998)
            continue; // Don't use this robot, no waypoints left
        double fuel = 1999.;
        int x = 500, y = 500;

        // FIXME:, collect rarer for first rover
        int m = (collected[0] < collected[1]) ? 0 : 1;
        
        // Find the highest density of the mineral we need.
        int highest = 0, highest_i = -1, highest_j = -1;
        for (int i = 0; i < 10; ++i)
            for (int j = 0; j < 10; ++j)
                if (low_res_map[m][i][j] > highest) {
                    highest = low_res_map[m][i][j];
                    highest_i = i;
                    highest_j = j;
                }

        // Go to inner corner of 100x100 cell with the highest density of our mineral
        if (highest_i > 4)
            y = 100*highest_i;
        else
            y = 100*highest_i + 99;
        if (highest_j > 4)
            x = 100*highest_j;
        else
            x = 100*highest_j + 99;
        collect(collected[0], collected[1], 500, 500, x, y);
        add_waypoint(n,x,y);
        double fuel_used = sqrt((500-x)*(500-x)+(500-y)*(500-y));
        fuel -= fuel_used;
        total_fuel -= fuel_used;
        total_fuel_used += fuel_used;

        for (;;) {
            int best_x = -1, best_y = -1;
            double best_s = 0., best_f = 0.;
            if (result.size() > 998)
                break;

            double predict = (double)(collected[0]+collected[1])/total_fuel_used*total_fuel;
            double need_ratio = ((double)collected[0]-collected[1])/predict+1.;
            value_ratio = need_ratio*((double)totals[0]/(totals[0]+totals[1]));
            value_ratio = (value_ratio-.5)*1.1+.5;
            value_ratio = max(0.,min(1.,value_ratio));
            cerr << "value_ratio="<< value_ratio<<endl;

            for (int i = max(0,y/10-13); i < min(100,y/10+13); i += 4)
                for (int j = max(0,x/10-13); j < min(100,x/10+13); j += 4) {
                    double f = sqrt((x-j*10)*(x-j*10) + (y-i*10)*(y-i*10));
                    if (f > fuel)
                        continue;
                    double fb = sqrt((j*10-500)*(j*10-500) + (i*10-500)*(i*10-500));
                    if (f+fb > fuel)
                        continue;
                    int sA = 0, sB = 0;
                    collect(sA, sB, x, y, j*10, i*10, false);
                    double s = ((1.-value_ratio)*sA+value_ratio*sB)/f;
                    if (s > best_s) {
                        best_s = s;
                        best_x = j*10;
                        best_y = i*10;
                        best_f = f;
                    }
                }
            if (best_x == -1)
                break;
            add_waypoint(n, best_x, best_y);
            collect(collected[0], collected[1], x, y, best_x, best_y);
            x = best_x, y = best_y;
            fuel -= best_f;
            total_fuel -= best_f;
            total_fuel_used += best_f;
        }

        collect(collected[0], collected[1], x, y, 500, 500);
        add_waypoint(n, 500, 500);
        fuel_used = sqrt((500-x)*(500-x) + (500-y)*(500-y));
        total_fuel -= fuel_used;
        total_fuel_used += fuel_used;
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
