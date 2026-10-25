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

const int DIRECTIONS[8][2] = {
    {-1, -1},
    {-1,  0},
    {-1, +1},
    { 0, -1},
    { 0, +1},
    {+1, -1},
    {+1,  0},
    {+1, +1}
};

const int COLLECT_MASKS[26][21] = {
{1024,32704,131056,262136,524284,524284,1048574,1048574,1048574,1048574,2097151,1048574,1048574,1048574,1048574,524284,524284,262136,131056,32704,1024,},
{0,0,0,0,262144,0,524288,524288,524288,0,1572864,524288,524288,524288,786432,262144,393216,196608,114704,32192,1024,},
{0,0,0,0,0,0,0,0,0,0,1048576,0,0,0,524288,0,262148,131080,98352,31680,1024,},
{0,0,0,0,0,0,524288,524288,0,0,1572864,524288,524288,524288,786432,262144,393216,196608,114704,32192,1024,},
{0,0,0,0,0,0,0,0,0,0,1048577,0,0,0,524290,0,262148,131080,98352,31680,1024,},
{0,0,0,0,0,0,0,0,0,0,1048576,0,0,0,524290,0,262148,131080,98352,31680,1024,},
{0,0,0,0,0,0,524288,0,0,0,1572864,524288,524288,524288,786432,262144,393216,196608,114704,32192,1024,},
{0,0,0,0,0,0,0,0,0,0,1572864,524288,524288,524288,786432,262144,393216,196608,114704,32192,1024,},
{0,0,0,0,0,0,0,0,0,0,3,2,2,2,6,4,12,24,65648,30656,1024,},
{0,0,0,0,0,0,0,0,0,0,1,0,0,0,2,0,262148,131080,98352,31680,1024,},
{0,0,0,0,0,0,0,0,0,0,1,0,0,0,524290,0,262148,131080,98352,31680,1024,},
{0,0,0,0,0,0,2,0,0,0,3,2,2,2,6,4,12,24,65648,30656,1024,},
{0,0,0,0,0,0,2,2,0,0,3,2,2,2,6,4,12,24,65648,30656,1024,},
{0,0,0,0,4,0,2,2,2,0,3,2,2,2,6,4,12,24,65648,30656,1024,},
{1024,32192,114704,196608,393216,262144,786432,524288,524288,524288,1572864,0,524288,524288,524288,0,262144,0,0,0,0,},
{0,0,65536,131072,262144,262144,524288,524288,524288,524288,1048576,524288,524288,524288,524288,262144,262144,131072,65536,16384,1024,},
{0,0,0,0,262144,0,524288,524288,524288,0,1572864,524288,524288,524288,786432,262144,393216,196608,114688,31936,1024,},
{1024,16384,65536,131072,262144,262144,524288,524288,524288,524288,1048576,524288,524288,524288,524288,262144,262144,131072,65536,0,0,},
{1024,31936,114688,196608,393216,262144,786432,524288,524288,524288,1572864,0,524288,524288,524288,0,262144,0,0,0,0,},
{1024,16384,65536,131072,262144,262144,524288,524288,524288,524288,1048576,524288,524288,524288,524288,262144,262144,131072,65536,16384,1024,},
{0,16384,65536,131072,262144,262144,524288,524288,524288,524288,1048576,524288,524288,524288,524288,262144,262144,131072,65536,16384,1024,},
{0,0,0,0,262144,0,524288,524288,524288,0,1572864,524288,524288,524288,786432,262144,393216,196608,114688,31808,1024,},
{1024,16384,65536,131072,262144,262144,524288,524288,524288,524288,1048576,524288,524288,524288,524288,262144,262144,131072,65536,16384,0,},
{1024,31808,114688,196608,393216,262144,786432,524288,524288,524288,1572864,0,524288,524288,524288,0,262144,0,0,0,0,},
{0,0,0,0,262144,0,524288,524288,524288,0,1572864,524288,524288,524288,786432,262144,393216,196608,114688,31744,1024,},
{1024,31744,114688,196608,393216,262144,786432,524288,524288,524288,1572864,0,524288,524288,524288,0,262144,0,0,0,0,},
};

const int COLLECT_STATE_TRANSITIONS[62][8] = {
{-1,-1,1,-1,4,29,32,57,},
{-1,-1,1,-1,2,-1,-1,-1,},
{-1,-1,3,-1,-1,-1,-1,-1,},
{-1,-1,1,-1,2,-1,-1,-1,},
{-1,-1,5,-1,11,-1,-1,23,},
{-1,-1,-1,-1,6,-1,-1,-1,},
{-1,-1,5,-1,7,-1,-1,-1,},
{-1,-1,8,-1,9,-1,-1,-1,},
{-1,-1,-1,-1,6,-1,-1,-1,},
{-1,-1,10,-1,-1,-1,-1,-1,},
{-1,-1,-1,-1,6,-1,-1,-1,},
{-1,-1,12,-1,17,-1,-1,22,},
{-1,-1,-1,-1,13,-1,-1,-1,},
{-1,-1,-1,-1,14,-1,-1,-1,},
{-1,-1,-1,-1,15,-1,-1,-1,},
{-1,-1,16,-1,17,-1,-1,-1,},
{-1,-1,-1,-1,13,-1,-1,-1,},
{-1,-1,16,-1,17,-1,-1,18,},
{-1,-1,-1,-1,19,-1,-1,-1,},
{-1,-1,-1,-1,20,-1,-1,-1,},
{-1,-1,-1,-1,21,-1,-1,-1,},
{-1,-1,-1,-1,17,-1,-1,18,},
{-1,-1,-1,-1,19,-1,-1,-1,},
{-1,-1,-1,-1,24,-1,-1,-1,},
{-1,-1,-1,-1,25,-1,-1,23,},
{-1,-1,-1,-1,26,-1,-1,28,},
{-1,-1,-1,-1,-1,-1,-1,27,},
{-1,-1,-1,-1,24,-1,-1,-1,},
{-1,-1,-1,-1,24,-1,-1,-1,},
{-1,-1,-1,-1,-1,29,30,-1,},
{-1,-1,-1,-1,-1,31,-1,-1,},
{-1,-1,-1,-1,-1,29,30,-1,},
{-1,-1,-1,-1,-1,33,39,51,},
{-1,-1,-1,-1,-1,-1,34,-1,},
{-1,-1,-1,-1,-1,33,35,-1,},
{-1,-1,-1,-1,-1,36,37,-1,},
{-1,-1,-1,-1,-1,-1,34,-1,},
{-1,-1,-1,-1,-1,38,-1,-1,},
{-1,-1,-1,-1,-1,-1,34,-1,},
{-1,-1,-1,-1,-1,40,45,50,},
{-1,-1,-1,-1,-1,-1,41,-1,},
{-1,-1,-1,-1,-1,-1,42,-1,},
{-1,-1,-1,-1,-1,-1,43,-1,},
{-1,-1,-1,-1,-1,44,45,-1,},
{-1,-1,-1,-1,-1,-1,41,-1,},
{-1,-1,-1,-1,-1,44,45,46,},
{-1,-1,-1,-1,-1,-1,47,-1,},
{-1,-1,-1,-1,-1,-1,48,-1,},
{-1,-1,-1,-1,-1,-1,49,-1,},
{-1,-1,-1,-1,-1,-1,45,46,},
{-1,-1,-1,-1,-1,-1,47,-1,},
{-1,-1,-1,-1,-1,-1,52,-1,},
{-1,-1,-1,-1,-1,-1,53,51,},
{-1,-1,-1,-1,-1,-1,54,56,},
{-1,-1,-1,-1,-1,-1,-1,55,},
{-1,-1,-1,-1,-1,-1,52,-1,},
{-1,-1,-1,-1,-1,-1,52,-1,},
{-1,-1,-1,-1,58,-1,60,57,},
{-1,-1,-1,-1,-1,-1,-1,59,},
{-1,-1,-1,-1,58,-1,-1,57,},
{-1,-1,-1,-1,-1,-1,-1,61,},
{-1,-1,-1,-1,-1,-1,60,57,},
};

const int COLLECT_STATE_MASKS[62] = {
0,13,9,12,4,12,9,10,11,10,8,4,11,9,10,10,8,4,7,2,5,5,6,3,2,5,5,7,6,14,17,18,19,18,17,22,23,22,25,19,23,17,22,22,25,19,24,15,20,20,21,16,15,20,20,24,21,1,2,3,15,16,};

void add_waypoint(int rover, double x, double y) {
    ostringstream out;
    out << rover << " " << (int)x << " " << (int)y;
    result.push_back(out.str());
}

void scollect(int& sum, vector<int>& m, int c, int s, int x, int y, bool zero) {
    for (int yy = 0; yy < 21; ++yy)
        for (int mask = COLLECT_MASKS[COLLECT_STATE_MASKS[s]][yy]; mask; mask ^= (mask & -mask)) {
            int xx = __builtin_ctz(mask);
            int cx = x+xx-11, cy = y+yy-11;
            if (cx < 0 || cy < 0 || cx >= 1000 || cy >= 1000)
                continue;
            int amount = m[cy*1000+cx];
            sum += amount;
            if (zero) {
                m[cy*1000+cx] = 0;
                low_res_map[c][cy/100][cx/100] -= amount;
                totals[c] -= amount;
            }
        }
}

// Bresenham's line algorithm
void collect(int& sumA, int& sumB, int x1, int y1, int x2, int y2, bool zero = true) {
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
    int s = 0, d, lx = -1, ly = -1;
    for (int x = x1; x <= x2; ++x) {
        if (steep) {
            if (lx != -1) {
                int xofs = y-ly, yofs = x-lx;
                for (d = 0; DIRECTIONS[d][0] != xofs || DIRECTIONS[d][1] != yofs; ++d)
                    ;
            } else
                d = -1;
            if (d != -1) {
                s = COLLECT_STATE_TRANSITIONS[s][d];
                assert(s != -1);
            }
            scollect(sumA, mineralA, 0, s, y, x, zero);
            scollect(sumB, mineralB, 1, s, y, x, zero);
        } else {
            if (lx != -1) {
                int xofs = x-lx, yofs = y-ly;
                for (d = 0; DIRECTIONS[d][0] != xofs || DIRECTIONS[d][1] != yofs; ++d)
                    ;
            } else
                d = -1;
            if (d != -1) {
                s = COLLECT_STATE_TRANSITIONS[s][d];
                assert(s != -1);
            }
            scollect(sumA, mineralA, 0, s, x, y, zero);
            scollect(sumB, mineralB, 1, s, x, y, zero);
        }
        lx = x, ly = y;
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
    //cerr <<"Original value_ratio = "<<value_ratio<<endl;

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
            //cerr << "value_ratio="<< value_ratio<<endl;

            for (int i = max(0,y-130); i < min(1000,y+130); i += 10)
                for (int j = max(0,x-130); j < min(1000,x+130); j += 10) {
                    double f = sqrt((x-j)*(x-j) + (y-i)*(y-i));
                    if (f > fuel)
                        continue;
                    double fb = sqrt((j-500)*(j-500) + (i-500)*(i-500));
                    if (f+fb > fuel)
                        continue;
                    int sA = 0, sB = 0;
                    collect(sA, sB, x, y, j, i, false);
                    double s = ((1.-value_ratio)*sA+value_ratio*sB)/f;
                    if (s > best_s) {
                        best_s = s;
                        best_x = j;
                        best_y = i;
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

