#include <algorithm>
#include <iterator>
#include <set>
#include <string>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }

namespace MM33 BEGIN

enum direction_type {
    UP = 1,
    STANDING = 0,
    DOWN = -1
};

int floors,lifts,capacity;
int position[50], direction[50];
string state;
set<int> upRequests, downRequests;
set<int> destinations[50];

int initialise(int N, int M, int C) {
    floors=N, lifts=M, capacity=C;
    memset(position,0,sizeof(position));
    fill(direction,direction+50,UP);
    return -1;
}

string step(const vector<int>& _upRequests, const vector<int>& _downRequests,
            const vector<int>& liftRequests, const vector<int>& destinationRequests,
            string _state)
{
    state=_state;
    for (int i = 0; i < lifts; ++i) {
        switch (state[i]) {
        case 'U':
            ++position[i];
            break;
        case 'D':
            --position[i];
            break;
        }
    }
    copy(ALL(_upRequests),insert_iterator<set<int> >(upRequests, upRequests.begin()));
    copy(ALL(_downRequests),insert_iterator<set<int> >(downRequests, downRequests.begin()));
    for (int i = 0; i < liftRequests.size(); ++i)
        destinations[liftRequests[i]].insert(destinationRequests[i]);
    
    for (int i = 0; i < lifts; ++i) {
        if (state[i]=='U' && position[i]+1 == floors) {
            state[i]='S';
            direction[i]=DOWN;
        } else if (state[i]=='D' && position[i] == 0) {
            state[i]='S';
            direction[i]=UP;
        } else if (state[i]=='U' && upRequests.count(position[i])) {
            state[i]='L';
            upRequests.erase(upRequests.find(position[i]));
        } else if (state[i]=='D' && downRequests.count(position[i])) {
            state[i]='M';
            downRequests.erase(downRequests.find(position[i]));
        } else if (state[i]=='S') {
            switch (direction[i]) {
            case UP:
                state[i]='U';
                break;
            case DOWN:
                state[i]='D';
                break;
            }
        }
    }

    return state;
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
