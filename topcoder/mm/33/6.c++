#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)
const int INF = 999999999;

namespace MM33 BEGIN

enum direction_type {
    UP = 1,
    STANDING = 0,
    DOWN = -1
};

struct lift {
    char state;
    int home_floor, position, direction;
    set<int> destinations;

    void reset() {
        position = 0;
        direction = UP;
        destinations.clear();
    }

    int clicks_to(int fl, int dir) {
        if (destinations.empty()) {
            if (fl == position)
                return 0;
            if (fl >= position && dir == UP)
                return 1 + abs(fl-position);
            if (fl <= position && dir == DOWN)
                return 1 + abs(fl-position);
            return INF;
        }
        if (fl <= position && dir == DOWN && direction == DOWN) {
            return abs(fl-position);
        }
        if (fl >= position && dir == UP && direction == UP) {
            return abs(fl-position);
        }
        return INF;
    }

    void add_destination(int fl, int dir) {
        destinations.insert(fl);
        if (fl > position || (fl == position && dir == UP))
            direction = UP;
        if (fl < position || (fl == position && dir == DOWN))
            direction = DOWN;
    }
};

int nfloors,nlifts,capacity;
lift lifts[50];
set<int> upRequests, downRequests;

int initialise(int N, int M, int C) {
    nfloors=N, nlifts=M, capacity=C;
    cerr<<"nfloors="<<nfloors<<",nlifts="<<nlifts<<",capacity="<<capacity<<endl;
    for (int i = 0; i < nlifts; ++i)
        lifts[i].reset();

    // Distribute home floors evenly
    if (nfloors > nlifts) {
        double sp = double(nfloors)/nlifts;
        double at = 0.;
        for (int i = 0; i < nfloors; ++i) {
            lifts[i].home_floor = min((int)at, nfloors-1);
            at += sp;
        }
    } else {
        double estep = double(nfloors)/nlifts;
        double nt = 0.;
        int i = 0;
        while (i < nfloors) {
            lifts[i].home_floor = min(int(nt), nfloors-1);
            ++i;
            nt += estep;
        }
    }

    return -1;
}

void update_states_and_positions(const string& _state) {
    for (int i = 0; i < nlifts; ++i) {
        lift& l = lifts[i];
        l.state = _state[i];
        switch (l.state) {
        case 'U':
            ++l.position;
            break;
        case 'D':
            --l.position;
            break;
        }
    }
}

void handle_requests(const vector<int>& _requests, int dir, set<int>& requests) {
    TR(_requests,it) {
        int fl = *it, closest = -1, closest_dist = INF;
        for (int i = 0; i < nlifts; ++i) {
            lift& l = lifts[i];
            int dist = INF;
            if (l.destinations.empty() || l.destinations.count(fl)>0) {
                dist = l.clicks_to(fl, dir);
            }
            if (dist < closest_dist) {
                closest_dist = dist;
                closest = i;
            }
            if (dist == INF)
                requests.insert(fl);
            else {
                lift& l = lifts[closest];
                l.add_destination(fl, dir);
            }
        }
    }

    set<int> toRemove;
    char want_state = (dir == UP) ? 'U' : 'D';
    TR(requests, it) {
        for (int i = 0; i < nlifts; ++i) {
            lift& l = lifts[i];
            if (l.position == *it && l.direction==dir && l.state == want_state && l.destinations.count(*it)>0) {
                toRemove.insert(*it);
                l.destinations.insert(*it);
                break;
            }
        }
    }
    TR(toRemove, it)
        requests.erase(*it);

}

string step(const vector<int>& _upRequests, const vector<int>& _downRequests,
            const vector<int>& liftRequests, const vector<int>& destinationRequests,
            string _state)
{
    update_states_and_positions(_state);

    for (int i = 0; i < liftRequests.size(); ++i) {
        lift& l = lifts[liftRequests[i]];
        int fl = destinationRequests[i];
        l.destinations.insert(fl);
    }

    handle_requests(_upRequests, UP, upRequests);
    handle_requests(_downRequests, DOWN, downRequests);
    
    for (int i = 0; i < nlifts; ++i) {
        lift& l = lifts[i];
        if (l.position+1 == nfloors) {
            l.direction=DOWN;
        }
        if (l.position == 0) {
            l.direction=UP;
        }

        bool stop = l.destinations.count(l.position)>0;
        l.destinations.erase(l.position);

        if (l.state=='U' && l.position+1 == nfloors)
            l.state='M';
        else if (l.state=='D' && l.position == 0)
            l.state='L';

        else if ((l.state=='U' && l.direction==DOWN) || (l.state=='D' && l.direction==UP)) {
            if (!stop)
                l.state = 'S';
            else if (l.direction == UP) {
                l.state = 'L';
                upRequests.erase(l.position);
            } else {
                l.state = 'M';
                downRequests.erase(l.position);
            }

        } else if (l.state=='U' && (stop || upRequests.count(l.position))) {
            l.state='L';
            upRequests.erase(l.position);
        } else if (l.state=='D' && (stop || downRequests.count(l.position))) {
            l.state='M';
            downRequests.erase(l.position);

        } else if (l.state=='S') {
            switch (l.direction) {
            case UP:
                l.state='U';
                break;
            case DOWN:
                l.state='D';
                break;
            }
        }
    }

    string new_state;
    for (int i = 0; i < nlifts; ++i)
        new_state += lifts[i].state;
    return new_state;
}

END 

struct LiftScheduler {
    inline int initialise(int N, int M, int C) {
        return MM33::initialise(N, M, C);
    }
    inline string step(const vector<int>& upRequests, const vector<int>& downRequests,
                       const vector<int>& liftRequests, const vector<int>& destinationRequests,
                       string state)
    {
        return MM33::step(upRequests, downRequests, liftRequests, destinationRequests, state);
    }
};
