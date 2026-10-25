#include <string>
#include <vector>
#include <functional>
#include <algorithm>
#include <cmath>
#include <queue>
#if defined(ARGO)
#include <cstdio>
#include "Tester.hpp"
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

struct Ad {
    struct SlotStatistics {
	int views, hits;
	SlotStatistics() : views(0), hits(0) {}
        double probability() {
	    if (views == 0) return MP(DefaultProbability,0.5);
            return (double)hits / (double)views;
        }
        double standard_error() {
            if (views == 0) return 1.0;
            double p = probability();
            /* 95% certainty */
            return 1.96 * sqrt((p * (1.0 - p)) / (double)views);
        }
    };
#if defined(ARGO)
    int real_index;
    Tester *cheat_ptr;
#endif
    int M;
    vector<SlotStatistics> stats;
    double P, D;
    Ad(int _M, int _K) : M(_M), stats(_K, SlotStatistics()),
			 P(0.25*0.25), D(0.85) {}
    double probability(int x) {
        if (stats.at(x).standard_error() < 0.2)
            return stats.at(x).probability();
	return P * pow(D, x);
    }
    void update_score() {
	vector<int> by_view(stats.size(), 0);
	generate(by_view.begin(), by_view.end(), Sequence());
	sort(by_view.begin(), by_view.end(), CompareStatsByError(&stats));
	int first = by_view.at(0), second = by_view.at(1);
	double start;
	if (stats.at(second).standard_error() >= 0.4) {
	    // One good data point.  Here's where our dummy values for
	    // probability and error will foot us in the shoot.
	    D = 0.85;
	    start = stats.at(first).probability();
	} else {
	    if (first > second)
		swap(first, second);
	    int distance = second - first;
	    start = stats.at(first).probability();
	    double target = stats.at(second).probability();

	    D = pow(target / start, 1.0 / (double)distance);
	    D = max(0.7, min(1.0, D));
	}
	P = max(0.0, min(1.0, start * pow(1.0 / D, (double)first)));

#if defined(ARGO) && 0
printf("      P = %f, D = %f, V = %f, M = %d\n",
        cheat_ptr->ads.at(real_index).P, cheat_ptr->ads.at(real_index).D,
        cheat_ptr->ads.at(real_index).V, M);
printf("#  CalcP    EmpP    e      hits   views M*CalcP M*EmpP\n");
printf("- ------- ------- ------- ------ ------ ------- -------\n");
for (int si = 0; si < stats.size(); si++) {
printf("%d %0.5f %0.5f %0.5f %6d %6d %02.4f %02.4f\n", si,
        probability(si),
	stats.at(si).probability(), stats.at(si).standard_error(),
	stats.at(si).hits, stats.at(si).views,
        (double)M * probability(si), (double)M * stats.at(si).probability());
}
printf("\n");
#endif
    }
    int view(int x) { return ++stats.at(x).views; }
    int hit(int x)  { return ++stats.at(x).hits; }
    struct CompareStatsByError {
	vector<SlotStatistics> *stats;
	CompareStatsByError(vector<SlotStatistics> *_stats)
	    : stats(_stats) {}
	bool operator () (int i, int j) const {
	    return stats->at(i).standard_error() <
		   stats->at(j).standard_error();
	}
    };
    bool exposed() {
        int total_views = 0;
        for (int i = 0; i < stats.size(); i++) {
            total_views += stats.at(i).views;
        }
        return (total_views >= 60);
    }
};

class AdPlacement {
    int K;
    vector<Ad> ads;
    vector<int> placement;
    int total_views;
    int minutes;
    bool ordered;
    queue<int> to_expose;
public:
#if defined(ARGO)
    Tester *cheat_ptr;
#endif
    int init(vector<int> M, int K) {
	this->K = K;
	ordered = false;
        for (int i = 0; i < M.size(); i++) {
	    ads.push_back(Ad(M.at(i), K));
#if defined(ARGO)
            ads.back().real_index = i;
            ads.back().cheat_ptr = cheat_ptr;
#endif
	}
	placement.resize(K);
	place_by_m();
	total_views = 0;
	minutes = 0;
        init_to_expose();
	return 0;
    }
    vector<int> placeAds(int timeLeft, string prevClicks) {
	if (timeLeft > 50) {
	    minutes += prevClicks.size();
	    update_views_and_hits(prevClicks);

            if (!to_expose.empty())
                exposure_round();
            else {
                if (minutes > 0 && (minutes % 100) == 0) {
                    update_placement();
                }
            }
	}
	return placement;
    }
private:
    bool in_a_slot(int ad) {
        for (int i = 0; i < placement.size(); i++) {
            if (placement.at(i) == ad) return true;
        }
        return false;
    }
    void exposure_round() {
        for (int i = 0; i < K; i++) {
            int x = placement.at(i);
            if (ads.at(x).exposed()) {
                do_swap(i, to_expose.front());
                to_expose.pop();
                if (to_expose.size() == 0) break;
            }
        }
    }
    void update_views_and_hits(const string& clicks) {
	for (string::const_iterator i = clicks.begin(); i != clicks.end(); i++){
	    int n = -1;
	    if (*i != 'X') {
		n = *i - '0';
                n = placement.at(n);
	    }
	    for (int x = 0; x < K; x++) {
		int a = placement.at(x);
		ads.at(a).view(x);
		total_views++;
		if (a == n) {
		    ads.at(a).hit(x);
		    break;
		}
	    }
	}
    }
    struct CompareByM{
	const vector<Ad>* ads;
	CompareByM(vector<Ad>* _ads) : ads(_ads) {}
	bool operator () (int i, int j) {
	    return ads->at(i).M > ads->at(j).M;
	}
    };
    template<class fT>
    void do_placement(fT f) {
	vector<int> new_order(ads.size(), 0);
	generate(new_order.begin(), new_order.end(), Sequence());
	sort(new_order.begin(), new_order.end(), f);
	copy(new_order.begin(), new_order.begin() + K, placement.begin());
    }
    void init_to_expose() {
	vector<int> new_order(ads.size(), 0);
	generate(new_order.begin(), new_order.end(), Sequence());
	sort(new_order.begin(), new_order.end(), CompareByM(&ads));
        for (int i = 0; i < min((size_t)(K * 3), ads.size()); i++) {
            to_expose.push(new_order.at(i));
        }
    }
    void place_by_m() {
	do_placement(CompareByM(&ads));
    }
    void shift_placement_up() {
	for (int i = 0; i < K; i++) {
	    if (++ placement.at(i) == ads.size())
		placement.at(i) = 0;
	}
    }
    void update_placement() {
#if defined(ARGO) && 0
printf("update_placement().  Starting value = %4.2f\n", evaluate(placement));
#endif
	for_each(ads.begin(), ads.end(), mem_fun_ref(&Ad::update_score));
	for (int i = 0; i < 40 * 50; i++) {
	    int x = rand() % ads.size(), y = rand() % (ads.size() - 1);
	    if (y >= x) y++;
	    try_swap(x, y);
	}
#if defined(ARGO) && 0
printf("Placement:");
for (int j = 0; j < placement.size(); j++) {
    printf("    %d -> %d\n", j, placement.at(j));
}
printf("\n  New value = %4.2f\n", evaluate(placement));
#endif
    }
    double evaluate(const vector<int>& p) {
        return evaluate(p, 0);
    }
    double percent_of_run() {
        return (double)minutes / 50000.0;
    }
    // At the beginning of the run, we select ads with higher _potential_
    // value.   At the end of the run, we select ads with higher _emperic_
    // value.
    double evaluate(const vector<int>& p, int ofs) {
        double elt_P = ads.at(placement.at(ofs)).probability(ofs);

	// Add in error to give higher chance to ads we don't know about
	// yet.
	double adjusted_P = elt_P +
	    ads.at(placement.at(ofs)).stats.at(ofs).standard_error()
	    * (1.0 - percent_of_run());

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
};

// vi:set ts=8:
