#include <string>
#include <vector>
#include <functional>
#include <algorithm>
#include <cmath>
#include <set>
#include <cassert>
#ifdef ARGO
#include <cstdio>
#endif
using namespace std;

#ifndef MP
#define MP(extvar,value) value
#endif

struct Sequence {
    int n;
    Sequence() {n = 0;}
    int operator() () { return n++; }
};


struct State;
struct Ad {
    struct SlotStatistics {
	double adjusted_views, adjusted_hits;
	SlotStatistics();
    };
    int index, M;
    vector<SlotStatistics> stats;
    double P, D;
    static const double default_info_decay;
    double info_decay;
    State *state;
    Ad(State *_state, int _index, int _M, int _K);
    double probability(int x);
    void update_statistics(int minutes);
    void decay_info();
    double emperic_probability(int slot);
    double standard_error(int slot);
    double predicted_probability();
    double adjusted_probability(int slot);
};

struct CompareStatsByError {
    Ad& ad;
    CompareStatsByError(Ad& _ad) : ad(_ad) {}
    bool operator () (int i, int j) const {
	return ad.standard_error(i) < ad.standard_error(j);
    }
};

struct CompareByM {
    const vector<Ad>* ads;
    CompareByM(vector<Ad>* _ads) : ads(_ads) {}
    bool operator () (int i, int j) {
	return ads->at(i).M > ads->at(j).M;
    }
};

struct State {
    vector<Ad> ads;
    vector<int> placement;
    vector<vector<int> > placements;
    string clicks;
    int minutes;
    void setup(vector<int> _M, int _K) {
	placement.resize(_K);
	for (int i = 0; i < _M.size(); i++) {
	    ads.push_back(Ad(this, i, _M.at(i), _K));
	}
	place_by_m();
	minutes = 0;
    }

    inline int K() {
	return placement.size();
    }
    
    double evaluate(const vector<int>& p) {
        return evaluate(p, 0);
    }
    double evaluate(const vector<int>& p, int ofs) {
        double elt_P = ads.at(placement.at(ofs)).probability(ofs);

	// Add in error to give higher chance to ads we don't know about
	// yet.
	double adjusted_P = elt_P +
	    ads.at(placement.at(ofs)).standard_error(ofs);

        int M = ads.at(placement.at(ofs)).M;
        double result = adjusted_P * M;
        if (ofs != p.size() - 1) {
            result += (1.0 - elt_P) * evaluate(p, ofs+1);
        }
        return result;
    }
    void do_swap(int x, int y) {
	for (vector<int>::iterator i = placement.begin(); i != placement.end();
	     i++) {
	    if (*i == x) *i = y;
	    else if (*i == y) *i = x;
	}
    }
    void try_swap(int x, int y) {
	double v = evaluate(placement);
	do_swap(x, y);
	if (evaluate(placement) < v)
	    do_swap(x, y);
    }
    // Done in order to get new stats after updating Ad::info_decay
    void replay_clicks()
    {
	for (int a = 0; a < ads.size(); a++) {
	    for (int s = 0; s < K(); s++) {
		ads.at(a).stats.at(s).adjusted_hits = 0;
		ads.at(a).stats.at(s).adjusted_views = 0;
	    }
	}

	for (int m = 0; m < minutes; m++) {
	    if (m % 20 == 0)
		decay_info();
	    process_click(placements.at(m / 20), clicks.at(m));
	}
    }
    // Calculate views and hits (paired with slot numbers) for a click
    struct Thing {
	int ad, slot;
        Thing() : ad(-1), slot(-1) {}
	Thing(int _ad, int _slot) : ad(_ad), slot(_slot) {}
    };
    struct ClickInfo {
	vector<Thing> view_info;
	bool hit;
	Thing hit_info;
	ClickInfo() : hit(false) {}
    };
    ClickInfo click(const vector<int>& placement, char ch) {
	ClickInfo result;
	int n = -1;
	if (ch != 'X') {
	    n = ch - '0';
	    n = placement.at(n);
	}
	for (int x = 0; x < K(); x++) {
	    int a = placement.at(x);
	    result.view_info.push_back(Thing(a, x));
	    if (a == n) {
		result.hit = true;
		result.hit_info = Thing(a, x);
		break;
	    }
	}
	return result;
    }
    void process_click(const vector<int>& placement, char ch) {
	ClickInfo info = click(placement, ch);
        for (vector<Thing>::const_iterator thi = info.view_info.begin();
             thi != info.view_info.end(); thi++)
            ads.at(thi->ad).stats.at(thi->slot).adjusted_views++;
        if (!info.hit)
            return;
        ads.at(info.hit_info.ad).stats.at(info.hit_info.slot).adjusted_hits++;
    }
    void decay_info() {
	for (int a = 0; a < ads.size(); a++)
	    ads.at(a).decay_info();
    }
    void update_views_and_hits(const string& prevClicks) {
	clicks += prevClicks;
	minutes += prevClicks.size();

	decay_info();
	for (string::const_iterator i = prevClicks.begin();
	     i != prevClicks.end(); i++){
	    process_click(placement, *i);
	}
    }
    void find_and_update_info_decay() {
	const int SAMPLE_MINUTES = 2000;
	const int SAMPLES = 10000 / SAMPLE_MINUTES;
	vector<vector<double> > psamples(ads.size(), vector<double>(SAMPLES));
	vector<int> hits(ads.size(), 0);
	vector<int> views(ads.size(), 0);

	for (int a = 0; a < ads.size(); a++) {
	    for (int s = 0; s < SAMPLES; s++) {
		psamples.at(a).at(s) = 0;
	    }
	}
	for (int m = 0; m < 10000; m++) {
	    for (int a = 0; a < ads.size(); a++) {
		hits.at(a) = 0;
		views.at(a) = 0;
	    }

//XXX:
	}
    }
    template<class fT>
    void do_placement(fT f) {
	vector<int> new_order(ads.size(), 0);
	generate(new_order.begin(), new_order.end(), Sequence());
	sort(new_order.begin(), new_order.end(), f);
	copy(new_order.begin(), new_order.begin() + K(), placement.begin());
    }
    void place_by_m() {
	do_placement(CompareByM(&ads));
    }
};

Ad::SlotStatistics::SlotStatistics()
    : adjusted_views(0), adjusted_hits(0)
{
}

const double Ad::default_info_decay = (1.0 - 1.0 * pow(2.0, 2.5) / 10000.0);

double Ad::emperic_probability(int slot)
{
    if (stats.at(slot).adjusted_views == 0) return 0.0;
    return (double)stats.at(slot).adjusted_hits /
	   (double)stats.at(slot).adjusted_views;
}

double Ad::standard_error(int slot)
{
    if (stats.at(slot).adjusted_views == 0) return 1.0;
    double p = emperic_probability(slot);
    /* 95% certainty */
    return 1.96 * sqrt((p * (1.0 - p)) / (double)stats.at(slot).adjusted_views);
}
double Ad::predicted_probability()
{
    // Approximation of the average P at `minutes'
    return (0.189966 - 0.0845645) *
	   pow((double)state->minutes / 50000.0,0.62) + 0.0845645;
}
double Ad::adjusted_probability(int slot)
{
    double eP = emperic_probability(slot);
    double pP = predicted_probability();
    double e = standard_error(slot);
    return (1.0 - e) * eP + e * pP;
}

Ad::Ad(State *_state, int _index, int _M, int _K)
    : state(_state), index(_index), M(_M), stats(_K, SlotStatistics()),
      P(0.25), D(0.85), info_decay(default_info_decay)
{
}

double Ad::probability(int slot)
{
    if (standard_error(slot) < 0.2)
	return emperic_probability(slot);
    return P * pow(D, slot);
}

void Ad::decay_info()
{
    for (vector<SlotStatistics>::iterator i = stats.begin(); i != stats.end();
	 i++) {
	i->adjusted_views *= info_decay;
	i->adjusted_hits *= info_decay;
    }
}

void Ad::update_statistics(int minutes)
{
    // Find P and D
    vector<int> by_view(stats.size(), 0);
    generate(by_view.begin(), by_view.end(), Sequence());
    sort(by_view.begin(), by_view.end(), CompareStatsByError(*this));
    int first = by_view.at(0), second = by_view.at(1);

    double start;

    if (standard_error(second) >= 0.4) {
	// One good data point.  Here's where our dummy values for
	// probability and error will foot us in the shoot.
	D = 0.85;
        start = adjusted_probability(first);
    } else {
	if (first > second)
	    swap(first, second);
	int distance = second - first;
        start = adjusted_probability(first);
	double target = adjusted_probability(second);

	D = pow(target / start, 1.0 / (double)distance);
	D = max(0.7, min(1.0, D));
    }
    P = max(0.0, min(1.0, start * pow(1.0 / D, (double)first)));

#if defined(ARGO) && 0
printf("M = %d\n", M);
printf("#     CalcP     EmpP        e   hits views  M*CalcP   M*EmpP\n");
printf("-  -------- -------- -------- ------ ----- -------- --------\n");
for (int si = 0; si < stats.size(); si++) {
printf("%d  %0.6f %0.6f %0.6f %6d %6d %2.4f %2.4f\n", si, probability(si),
    stats.at(si).probability(), stats.at(si).standard_error(),
    stats.at(si).hits, stats.at(si).views,
    (double)M * probability(si), (double)M * stats.at(si).probability());
}
printf("\n");
#endif
}

struct Strategy {
    // Returns false if the strategy can't do anything
    virtual bool operator () (State& state) = 0;
};

struct RandSwapAndEvaluateStrategy: public Strategy {
    virtual bool operator () (State& state) {
	if (state.minutes == 0 || (state.minutes % 100) != 0)
	    return true;
	for_each(state.ads.begin(), state.ads.end(),
		 bind2nd(mem_fun_ref(&Ad::update_statistics), state.minutes));
// Converges quickly and my laptop sucks.
#if defined(ARGO)
# define RAND_SWAP_COUNT 40
#else
# define RAND_SWAP_COUNT 200
#endif
	for (int i = 0; i < RAND_SWAP_COUNT; i++) {
	    int x = rand() % state.ads.size(), y = rand() % (state.ads.size() - 1);
	    if (y >= x) y++;
	    state.try_swap(x, y);
	}
	return true;
    }
};

// What we want to accomplish:  Make sure the contenders have enough views
// that we can get an accurate reading on them.
struct EnsureExposureForTopRankers: public Strategy {
    set<int> remaining;
    EnsureExposureForTopRankers(State& state) {
	vector<int> rankers(state.ads.size(), 0);
	generate(rankers.begin(), rankers.end(), Sequence());
	sort(rankers.begin(), rankers.end(), CompareByM(&state.ads));
	int n = min(state.K() * 3, (int)state.ads.size());
	for (int i = 0; i < n; i++)
	    remaining.insert(rankers.at(i));
    }
    virtual bool operator () (State& state) {
	for (int i = 0; i < state.K(); i++) {
	    int x = state.placement.at(i);
	    if (remaining.find(x) != remaining.end()) {
		if (!complete(state,x))
		    continue;
		remaining.erase(x);
		if (remaining.empty()) {
		    return false;
		}
	    }
	    // Swap in another ad from the set.
	    for (set<int>::iterator next = remaining.begin();
		 next != remaining.end(); next++) {
		if (!in_slot(state, *next)) {
		    state.do_swap(x, *next);
		}
	    }
	}
	return true;
    }
    bool complete(State& state, int i) {
	// FIXME: determine by value and error: if we're pretty certain that
	// others will place better, forget this one.
	double total_views = 0.0;
	for (int j = 0; j < state.ads.at(i).stats.size(); j++) {
	    total_views += state.ads.at(i).stats.at(j).adjusted_views;
	}
	return (total_views >= 70.0);
    }
    bool in_slot(State& state, int i) {
	for (int j = 0; j < state.K(); j++) {
	    if (state.placement.at(j) == i)
		return true;
	}
	return false;
    }
};

class AdPlacement {
    State state;
    Strategy *strategy;
public:
    AdPlacement() : strategy(NULL) {}
    ~AdPlacement() {
	if (strategy != NULL)
	    delete strategy;
    }
    int init(vector<int> M, int K) {
	state.setup(M, K);
	strategy = new EnsureExposureForTopRankers(state);
	return 0;
    }
    vector<int> placeAds(int timeLeft, string prevClicks) {
	if (timeLeft > 50) {
	    state.update_views_and_hits(prevClicks);

	    if (state.minutes == 10000)
		state.find_and_update_info_decay();

	    if (!(*strategy) (state)) {
		delete strategy;
		strategy = new RandSwapAndEvaluateStrategy();
	    }
	    state.placements.push_back(state.placement);
	}
	return state.placement;
    }
};

// vi:set ts=8:
