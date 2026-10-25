#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <sys/time.h>
#include <time.h>
#include <utility>
#include <vector>
using namespace std;
#define BEGIN {
#define END }
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

namespace MM19 BEGIN

int noOfRovers;
vector<int> mineralA, mineralB;
int totals[2] = {0, 0};

int low_res_map[2][10][10];
int collect_map[1000][1000];

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

inline void nozero(int& sumA, int& sumB, int i, int cx, int cy) {
    if (!collect_map[cy][cx])
        sumA += mineralA[i], sumB += mineralB[i];
}
inline void zero(int& sumA, int& sumB, int i, int cx, int cy) {
    if (collect_map[cy][cx]++ == 0) {
        int amtA = mineralA[i], amtB = mineralB[i];
        sumA += amtA, sumB += amtB;
        low_res_map[0][cy/100][cx/100] -= amtA;
        low_res_map[1][cy/100][cx/100] -= amtB;
        totals[0] -= amtA;
        totals[1] -= amtB;
    }
}
inline void unzero(int& sumA, int& sumB, int i, int cx, int cy) {
    if (--collect_map[cy][cx] == 0) {
        int amtA = mineralA[i], amtB = mineralB[i];
        sumA += amtA, sumB += amtB;
        low_res_map[0][cy/100][cx/100] += amtA;
        low_res_map[1][cy/100][cx/100] += amtB;
        totals[0] += amtA;
        totals[1] += amtB;
    }
}

template<void CollectFunctor(int&,int&,int,int,int)>
inline void scollect(int& sumA, int& sumB, int s, int x, int y) {
    int cx, cy, i;
    for (int yy = 0; yy < 21; ++yy) {
        cy = y+yy-11;
        if (cy < 0 || cy >= 1000)
            continue;
        for (int mask = COLLECT_MASKS[COLLECT_STATE_MASKS[s]][yy]; mask; mask ^= (mask & -mask)) {
            cx = x+__builtin_ctz(mask)-11;
            if (cx < 0 || cx >= 1000) 
                continue;
            i = cy*1000+cx;
            CollectFunctor(sumA,sumB,i,cx,cy);
        }
    }
}

const int DIRECTIONS[8][2] = {
    {-1, -1}, {-1,  0}, {-1, +1}, { 0, -1}, { 0, +1}, {+1, -1}, {+1,  0}, {+1, +1}
};
const int DIRECTION_LOOKUP[9] = {0,1,2,3,-1,4,5,6,7};
inline int direction(int xofs, int yofs) {
    return DIRECTION_LOOKUP[(xofs+1)*3+(yofs+1)];
}

// Bresenham's line algorithm
template<void CollectFunctor(int&,int&,int,int,int)>
void collect(int& sumA, int& sumB, int x1, int y1, int x2, int y2) {
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
    int s = 0, lx = -1, ly = -1;
    for (int x = x1; x <= x2; ++x) {
        if (steep) {
            if (lx != -1)
                s = COLLECT_STATE_TRANSITIONS[s][direction(y-ly,x-lx)];
            scollect<CollectFunctor>(sumA, sumB, s, y, x);
        } else {
            if (lx != -1)
                s = COLLECT_STATE_TRANSITIONS[s][direction(x-lx,y-ly)];
            scollect<CollectFunctor>(sumA, sumB, s, x, y);
        }
        lx = x, ly = y;
        error += deltay;
        if (error>0) {
            y += ystep;
            error -= deltax;
        }
    }
}

int collected[2] = {0,0};
double total_fuel;
double total_fuel_used = 0.;
int waypoints_used = 0;
double value_ratio;

pair<int,int> find_best_sector() {
    double best = 0.;
    int best_i = -1, best_j = -1;
    for (int i = 0; i < 10; ++i)
        for (int j = 0; j < 10; ++j) {
            double s = (1.-value_ratio)*low_res_map[0][i][j] + value_ratio*low_res_map[1][i][j];
            if (s > best) {
                best = s;
                best_i = i;
                best_j = j;
            }
        }

    // Go to inner corner of 100x100 cell with the best density of our mineral
    int x,y;
    if (best_i > 4)
        y = 100*best_i;
    else
        y = 100*best_i + 99;
    if (best_j > 4)
        x = 100*best_j;
    else
        x = 100*best_j + 99;
    return make_pair(x,y);
}

void update_value_ratio() {
    double predict = (double)(collected[0]+collected[1])/total_fuel_used*total_fuel;
    double need_ratio = ((double)collected[0]-collected[1])/predict+1.;
    value_ratio = need_ratio*((double)totals[0]/(totals[0]+totals[1]));
    value_ratio = (value_ratio-.5)*1.6+.5;
    value_ratio = max(0.,min(1.,value_ratio));
}

struct timeval end;

bool have_time() {
    struct timeval now;
    gettimeofday(&now,NULL);
    return timercmp(&now,&end,<);
}

struct Trip {
    vector<pair<int,int> > waypoints;

    void plan(int dist,int increm) {
        double fuel = 1999.;

        update_value_ratio();
        pair<int,int> coord = find_best_sector();
        int x = coord.first, y = coord.second;

        collect<zero>(collected[0], collected[1], 500, 500, x, y);
        waypoints.push_back(make_pair(x,y));
        double fuel_used = sqrt((500-x)*(500-x)+(500-y)*(500-y));
        fuel -= fuel_used;
        total_fuel -= fuel_used;
        total_fuel_used += fuel_used;

        while (have_time()) {
            int best_x = -1, best_y = -1;
            double best_s = 0., best_f = 0.;
            if (waypoints_used+waypoints.size() > 998)
                break;

            update_value_ratio();

            for (int i = max(0,y-dist); i < min(1000,y+dist); i += increm) {
                for (int j = max(0,x-dist); j < min(1000,x+dist); j += increm) {
                    double f = sqrt((x-j)*(x-j) + (y-i)*(y-i));
                    if (f > fuel)
                        continue;
                    double fb = sqrt((j-500)*(j-500) + (i-500)*(i-500));
                    if (f+fb > fuel)
                        continue;
                    int sA = 0, sB = 0;
                    collect<nozero>(sA, sB, x, y, j, i);
                    double s = ((1.-value_ratio)*sA+value_ratio*sB)/f;
                    if (s > best_s) {
                        best_s = s;
                        best_x = j;
                        best_y = i;
                        best_f = f;
                    }
                }
                if (!have_time())
                    break;
            }
            if (best_x == -1)
                break;
            waypoints.push_back(make_pair(best_x,best_y));
            collect<zero>(collected[0], collected[1], x, y, best_x, best_y);
            x = best_x, y = best_y;
            fuel -= best_f;
            total_fuel -= best_f;
            total_fuel_used += best_f;
        }

        collect<zero>(collected[0], collected[1], x, y, 500, 500);
        waypoints.push_back(make_pair(500,500));
        fuel_used = sqrt((500-x)*(500-x) + (500-y)*(500-y));
        total_fuel -= fuel_used;
        total_fuel_used += fuel_used;
    }

    void optimize() {
        double fuel = 1999.;
        int x = 500, y = 500;
        for (int i = 0; i < waypoints.size(); ++i) {
            fuel -= hypot(x-waypoints[i].first, y-waypoints[i].second);
            x = waypoints[i].first, y = waypoints[i].second;
        }
        fuel -= hypot(x-500,y-500);
        for (int i = 0; i < waypoints.size()-1; ++i) {
            if (!have_time())
                break;
            int best_score = min(collected[0],collected[1]);
            int best_x = waypoints[i].first;
            int best_y = waypoints[i].second;
            int amtA = 0, amtB = 0;
            int sx = (i==0) ? 500 : waypoints[i-1].first;
            int sy = (i==0) ? 500 : waypoints[i-1].second;
            int nx = waypoints[i+1].first;
            int ny = waypoints[i+1].second;
            double best_f = hypot(sx-best_x, sy-best_y) +
                            hypot(best_x-nx, best_y-ny);
            fuel += best_f;

            collect<unzero>(amtA,amtB,sx,sy,waypoints[i].first,waypoints[i].second);
            collect<unzero>(amtA,amtB,waypoints[i].first,waypoints[i].second,nx,ny);
            collected[0] -= amtA;
            collected[1] -= amtB;
            for (int t = 0; t < 4; ++t) {
                int tx = max(0,min(999,waypoints[i].first + (rand()%8)-4));
                int ty = max(0,min(999,waypoints[i].second + (rand()%8)-4));
                double fused = hypot(sx-tx,sy-ty) + hypot(tx-nx,ty-ny);
                if (fuel < fused)
                    continue;
                collect<zero>(collected[0],collected[1],sx,sy,tx,ty);
                collect<zero>(collected[0],collected[1],tx,ty,nx,ny);
                if (min(collected[0],collected[1]) > best_score ||
                    (min(collected[0],collected[1]) == best_score &&
                     fused < best_f)) {
                    best_score = min(collected[0],collected[1]);
                    best_x = tx;
                    best_y = ty;
                    best_f = fused;
                }
                int amtA = 0, amtB = 0;
                collect<unzero>(amtA,amtB,sx,sy,tx,ty);
                collect<unzero>(amtA,amtB,tx,ty,nx,ny);
                collected[0] -= amtA, collected[1] -= amtB;
            }

            // Can we delete this waypoint?
            int mx = (sx+nx)/2, my = (sy+ny)/2;
            collect<zero>(collected[0],collected[1],sx,sy,mx,my);
            collect<zero>(collected[0],collected[1],mx,my,nx,ny);
            double dfused = hypot(sx-nx,sy-ny);
            if (min(collected[0],collected[1]) >= best_score) {
                fuel -= dfused;
                waypoints[i].first = mx;// (sx+nx)/2;
                waypoints[i].second = my;//(sy+ny)/2;
                //--i;
            } else {
                int amtA = 0, amtB = 0;
                collect<unzero>(amtA,amtB,sx,sy,mx,my);
                collect<unzero>(amtA,amtB,mx,my,nx,ny);
                collected[0] -= amtA, collected[1] -= amtB;

                waypoints[i].first = best_x;
                waypoints[i].second = best_y;
                collect<zero>(collected[0],collected[1],sx,sy,waypoints[i].first,waypoints[i].second);
                collect<zero>(collected[0],collected[1],waypoints[i].first,waypoints[i].second,nx,ny);
                fuel -= best_f;
            }
        }
    }

    void uncollect() {
        int x = 500, y = 500;
        int amtA = 0, amtB = 0;
        TR(waypoints,wi) {
            collect<unzero>(amtA,amtB,x,y,wi->first,wi->second);
            x = wi->first, y = wi->second;
        }
        collected[0] -= amtA, collected[1] -= amtB;
    }
    void recollect() {
        int x = 500, y = 500;
        TR(waypoints,wi) {
            collect<zero>(collected[0],collected[1],x,y,wi->first,wi->second);
            x = wi->first, y = wi->second;
        }
    }

};
vector<Trip> trips;

vector<string> result;
void construct_waypoints() {
    int n = 0;
    TR(trips,ti) {
        TR(ti->waypoints,wi) {
            ostringstream out;
            out << n <<' '<<wi->first<<' '<<wi->second;
            result.push_back(out.str());
        }
        ++n;
    }
}

void run() {
    gettimeofday(&end,NULL);
    end.tv_sec += 29;
    memset(collect_map,0,sizeof(collect_map));
    for (int i = 0; i < 1000; ++i)
        for (int j = 0; j < 1000; ++j) {
            low_res_map[0][i/100][j/100] += mineralA[i*1000+j];
            low_res_map[1][i/100][j/100] += mineralB[i*1000+j];
            totals[0] += mineralA[i*1000+j];
            totals[1] += mineralB[i*1000+j];
        }

    //cerr <<"Original value_ratio = "<<value_ratio<<endl;
    
    total_fuel = 1999.*noOfRovers;

    int n;
    for (n = 0; n < noOfRovers; ++n) {
        if (waypoints_used > 998)
            continue; // Don't use this robot, no waypoints left

        Trip trip;
        trip.plan(130,10);
        trips.push_back(trip);
        waypoints_used += trip.waypoints.size();
    }

    int p = 0;
    while (have_time()) {
        for (int i = 0; i < 7; ++i) {
            cerr <<"Score before optimization: "<<min(collected[0],collected[1])<<endl;
            TR(trips,ti) {
                if (!have_time())
                    break;
                ti->optimize();
            }
            cerr <<"After:"<<min(collected[0],collected[1])<<endl;
        }

        const int PARAMS[][2] = {{220,17},{200,13},{300,23},{300,57}};
        for (int n = 0; n < noOfRovers; ++n) {
            int old_score = min(collected[0],collected[1]);
            trips[n].uncollect();
            waypoints_used -= trips[n].waypoints.size();
            Trip new_trip;
            new_trip.plan(PARAMS[p][0],PARAMS[p][1]);
            for (int i = 0; i < 5; ++i)
                new_trip.optimize();
            int new_score = min(collected[0],collected[1]);
            if (new_score > old_score && new_trip.waypoints.back() == make_pair(500,500)) {
                cerr << "taking new trip."<<endl;
                waypoints_used += new_trip.waypoints.size(); 
                trips[n] = new_trip;
            } else {
                new_trip.uncollect();
                waypoints_used += trips[n].waypoints.size();
                trips[n].recollect();
                assert(old_score == min(collected[0],collected[1]));
            }
        }
        if (++p == sizeof(PARAMS)/sizeof(PARAMS[0]))
            p = 0;
    }

    construct_waypoints();
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

