#include <string>
#include <vector>
#include <functional>
#include <algorithm>
#include <cmath>
using namespace std;

struct Sequence {
    int n;
    Sequence() {n = 0;}
    int operator() () { return n++; }
};

struct Ad {
    struct SlotStatistics {
	int views, hits;
	SlotStatistics() : views(0), hits(0) {}
        double probability() {
            return (double)hits / (double)views;
        }
        double error() {
            double p = probability();
	    // 95% confidence interval
            return 1.96 * sqrt((p * (1.0 - p)) / (double)views);
        }
    };
    int M;
    vector<SlotStatistics> stats;
    Ad(int _M, int _K) : M(_M), stats(_K, SlotStatistics()),
			 view_counter(0) {}
    int view_counter;
    double P, D, E;
    double probability(int x) {
	return P * pow(D, x);
    }
    void update_score() {
	if (view_counter < 150) return;
	view_counter = 0;
	find_D_and_P();
    }
    int view(int x) { view_counter++; return ++stats[x].views; }
    int hit(int x)  { return ++stats[x].hits; }
    int sum_of_statistic(int SlotStatistics::*varptr) const {
	int result = 0;
	for (vector<SlotStatistics>::const_iterator i = stats.begin();
	     i != stats.end(); i++)
	    result += (*i).*varptr;
	return result;
    }
    int hits() const  { return sum_of_statistic(&SlotStatistics::hits); }
    int views() const { return sum_of_statistic(&SlotStatistics::views); }
    struct CompareStatsByError {
	vector<SlotStatistics> *stats;
	CompareStatsByError(vector<SlotStatistics> *_stats)
	    : stats(_stats) {}
	bool operator () (int i, int j) const {
	    return (*stats)[i].error() < (*stats)[j].error();
	}
    };
    void find_D_and_P() {
	vector<int> by_view(stats.size(), 0);
	generate(by_view.begin(), by_view.end(), Sequence());
	sort(by_view.begin(), by_view.end(), CompareStatsByError(&stats));
	int first = by_view[0], second = by_view[1];
	D = 0.85;
	P = 0.25 * 0.25;

	E = stats[first].error() + stats[second].error();
        if (E < 0.2) {
	    if (first > second)
		swap(first, second);
	    int distance = second - first;
	    double start = stats[first].probability();
	    double target = stats[second].probability();

            D = exp(log(target / start) / distance);
            D = max(0.7, min(1.0, D));

	    if (E > stats[0].error()) {
		P = stats[0].probability();
		E = stats[0].error();
	    } else {
		P = start;
		while (first > 0) {
		    P /= D;
		    first--;
		}
	    }
	}
    }
};

class AdPlacement {
public: //XXX: DELETE ME.
    int K;
    vector<Ad> ads;
    vector<int> placement;
    int total_views;
    bool ordered;
public:
    int init(vector<int> M, int K) {
	this->K = K;
	ordered = false;
	for (vector<int>::const_iterator i = M.begin(); i != M.end(); i++) {
	    ads.push_back(Ad(*i, K));
	}
	placement.resize(K);
	for (int i = 0; i < K; i++) placement[i] = i;
	total_views = 0;
	return 0;
    }
    vector<int> placeAds(int timeLeft, string prevClicks) {
	if (timeLeft > 50) {
	    update_views_and_hits(prevClicks);
	    if (total_views < /* 8500 */ 8)
		shift_placement_up();
	    else {
		if (!ordered) {
		    place_by_p_times_m();
		    ordered = true;
		} else
		    update_placement();
	    }
	}
	return placement;
    }
private:
    void update_views_and_hits(const string& clicks) {
	for (string::const_iterator i = clicks.begin(); i != clicks.end(); i++){
	    int n = -1;
	    if (*i != 'X') {
		n = *i - '0';
                n = placement[n];
	    }
	    for (int x = 0; x < K; x++) {
		int a = placement[x];
		ads[a].view(x);
		total_views++;
		if (a == n) {
		    ads[a].hit(x);
		    break;
		}
	    }
	}
    }
    struct CompareByPTimesM{
	const vector<Ad>* ads;
	CompareByPTimesM(vector<Ad>* _ads) : ads(_ads) {}
	bool operator () (int i, int j) {
	    return ((double)(*ads)[i].M * (*ads)[i].P) >
		   ((double)(*ads)[j].M * (*ads)[i].P);
	}
    };
    struct CompareByViews {
	const vector<Ad>* ads;
	CompareByViews(vector<Ad>* _ads) : ads(_ads) {}
	bool operator () (int i, int j) {
	    return (*ads)[i].views() < (*ads)[j].views();
	}
    };
    template<class fT>
    void do_placement(fT f) {
	vector<int> new_order(ads.size(), 0);
	generate(new_order.begin(), new_order.end(), Sequence());
	sort(new_order.begin(), new_order.end(), f);
	copy(new_order.begin(), new_order.begin() + K, placement.begin());
    }
    void place_by_p_times_m() {
	do_placement(CompareByPTimesM(&ads));
    }
    void shift_placement_up() {
	for (int i = 0; i < K; i++) {
	    if (++ placement[i] == ads.size())
		placement[i] = 0;
	}
    }
    void update_placement() {
	for_each(ads.begin(), ads.end(), mem_fun_ref(&Ad::update_score));
	for (int i = 0; i < 50; i++) {
	    int x = rand() % ads.size(), y = rand() % ads.size();
	    while (x == y) y = rand() % ads.size();
	    try_swap(x, y);
	}
    }
    double evaluate(const vector<int>& p) {
        return evaluate(p, 0);
    }
    double evaluate(const vector<int>& p, int ofs) {
        double elt_P = ads[placement[ofs]].probability(ofs);

	// Add the margin of error so that we try ads that have high margins
	// and _could_ be winners until we know better.
	elt_P += ads[placement[ofs]].E;

        int M = ads[placement[ofs]].M;
        if (ofs == p.size() - 1) {
            return elt_P * M;
        } else {
            return elt_P * M + (1.0 - elt_P) * evaluate(p, ofs+1);
        }
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
};
