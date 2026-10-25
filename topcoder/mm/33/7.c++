#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <fstream>
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
    DOWN = -1,
};

struct destination {
    int floor;
    int direction;
    destination() : floor(0), direction(UP) {}
    destination(int floor, int direction) : floor(floor), direction(direction) {}
    inline bool operator < (const destination& rhs) const {
        if (floor < rhs.floor) return true;
        if (floor > rhs.floor) return false;
        return direction < rhs.direction;
    }
    inline bool operator == (const destination& rhs) const {
        return floor == rhs.floor && direction == rhs.direction;
    }
    inline bool operator != (const destination& rhs) const {
        return ! operator == (rhs);
    }
};

struct lift {
    char state;
    int home_floor, position, direction;
    set<destination> destinations;

    void reset() {
        position = 0;
        direction = UP;
        destinations.clear();
    }

    bool want_stop(const destination& dest) {
        assert(dest.direction!=STANDING);
        if (destinations.count(dest)>0) return true;
        if (destinations.count(destination(dest.floor, STANDING))>0) return true;
        return false;
    }
    void erase_stop(const destination& dest) {
        assert(dest.direction!=STANDING);
        destinations.erase(dest);
        destinations.erase(destination(dest.floor, STANDING));
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

    void add(const destination& new_dest) {

        if (new_dest.direction == UP || new_dest.direction == DOWN) {
            destinations.insert(new_dest);
            destination stand(new_dest.floor, STANDING);
            if (destinations.count(stand)>0)
                destinations.erase(stand);
        } else if (new_dest.direction == STANDING) {
            if (destinations.count(destination(new_dest.floor, UP)) == 0 &&
                destinations.count(destination(new_dest.floor, DOWN)) == 0)
                destinations.insert(new_dest);
        } else
            assert(false);

        if (new_dest.floor > position || (new_dest.floor == position && new_dest.direction != DOWN))
            direction = UP;
        if (new_dest.floor < position || (new_dest.floor == position && new_dest.direction != UP))
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
        destination new_dest(*it, dir);
        int closest = -1, closest_dist = INF;
        for (int i = 0; i < nlifts; ++i) {
            lift& l = lifts[i];
            int dist = INF;
            if (l.destinations.empty() || l.want_stop(new_dest)) {
                dist = l.clicks_to(new_dest.floor, new_dest.direction);
            }
            if (dist < closest_dist) {
                closest_dist = dist;
                closest = i;
            }
        }
        if (closest_dist == INF)
            requests.insert(*it);
        else {
            lift& l = lifts[closest];
            l.add(new_dest);
        }
    }

    set<int> toRemove;
    char want_state = (dir == UP) ? 'U' : 'D';
    TR(requests, it) {
        vector<pair<int, int> > candidates;
        for (int i = 0; i < nlifts; ++i) {
            lift& l = lifts[i];
            if (l.position == *it && l.direction==dir && l.state == want_state) {
                candidates.push_back(make_pair(
                            (l.want_stop(destination(*it, dir)) ? 0 : 1000)+l.destinations.size(),
                            i
                            ));
            }
        }
        if (!candidates.empty()) {
            int i = min_element(ALL(candidates))->second;
            lift& l = lifts[i];
            //FIXME: use l.add()?
            l.destinations.insert(destination(*it, dir));
            toRemove.insert(*it);
        }
    }
    TR(toRemove, it)
        requests.erase(*it);

}

bool write_test_case = false;
string step(const vector<int>& _upRequests, const vector<int>& _downRequests,
            const vector<int>& liftRequests, const vector<int>& destinationRequests,
            string _state)
{
    if (write_test_case) {
        write_test_case = false;
        ofstream out("test_case.cpp",ios::out);
        out<<"    MM33::nfloors = "<<nfloors<<", MM33::nlifts = "<<nlifts<<", MM33::capacity = "<<capacity<<";\n";
        for (int i = 0; i < nlifts; ++i) {
            lift& l = lifts[i];
            out<<"    MM33::lifts["<<i<<"].state = '"<<l.state<<"';\n";
            out<<"    MM33::lifts["<<i<<"].home_floor = "<<l.home_floor<<";\n";
            out<<"    MM33::lifts["<<i<<"].position = "<<l.position<<";\n";
            out<<"    MM33::lifts["<<i<<"].direction = "<<l.direction<<";\n";
            out<<"    MM33::lifts["<<i<<"].destinations.clear();\n";
            TR(l.destinations, it) {
                out<<"    MM33::lifts["<<i<<"].destinations.insert(destination("<<it->floor<<","<<it->direction<<"));\n";
            }
        }
        out<<"    MM33::upRequests.clear();\n";
        TR(upRequests,it)
            out<<"    MM33::upRequests.insert("<<*it<<");\n";
        out<<"    MM33::downRequests.clear();\n";
        TR(downRequests,it)
            out<<"    MM33::downRequests.insert("<<*it<<");\n";
        out<<"\n";
        out<<"    vector<int> upRequests, downRequests, liftRequests, destinationRequests;\n";
        TR(_upRequests,it)
            out<<"    upRequests.push_back("<<*it<<");\n";
        TR(_downRequests,it)
            out<<"    downRequests.push_back("<<*it<<");\n";
        TR(liftRequests,it)
            out<<"    liftRequests.push_back("<<*it<<");\n";
        TR(destinationRequests,it)
            out<<"    destinationRequests.push_back("<<*it<<");\n";
        out<<"    string state = \""<<_state<<"\";\n";
        out<<"    string new_state = MM33::step(upRequests, downRequests, liftRequests, destinationRequests, state);\n";

        out<<flush;
        out.close();
    }

    update_states_and_positions(_state);

    for (int i = 0; i < liftRequests.size(); ++i) {
        lift& l = lifts[liftRequests[i]];
        int fl = destinationRequests[i];
        l.destinations.insert(destination(fl, STANDING));
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

        destination target(l.position, l.direction);
        bool stop = l.want_stop(target);
        if (!((l.state=='L' && l.direction == DOWN) || (l.state=='M' && l.direction==UP)))
            l.erase_stop(target);

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
                l.state=stop ? 'L' : 'U';
                break;
            case DOWN:
                l.state=stop ? 'M' : 'D';
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
