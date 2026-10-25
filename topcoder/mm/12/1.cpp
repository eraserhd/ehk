#include <string>
#include <vector>
#include <functional>
#include <algorithm>
using namespace std;

class AdPlacement {
    struct Ad {
	int M, views, hits;
	Ad(int _M) : M(_M), views(0), hits(0) {
	}
	double score;
	void update_score() {
	    if (views != 0)
		score = (double)hits / (double)views * (double)M;
	    else
		score = (double)M;
	}
    };
    int K;
    vector<Ad> ads;
    vector<int> placement;
public:
    int init(vector<int> M, int K) {
	this->K = K;
	for (vector<int>::const_iterator i = M.begin(); i != M.end(); i++) {
	    ads.push_back(Ad(*i));
	}
	placement.resize(K);
	for (int i = 0; i < K; i++) placement[i] = i;
	return 0;
    }
    vector<int> placeAds(int timeLeft, string prevClicks) {
	if (timeLeft > 50) {
	    update_views_and_hits(prevClicks);
	    update_placement();
	}
	return placement;
    }
private:
    void update_views_and_hits(const string& clicks) {
	for (string::const_iterator i = clicks.begin(); i != clicks.end(); i++){
	    int n = -1;
	    if (*i != 'X') {
		n = *i - '0';
		ads[n].hits++;
	    }
	    for (int j = 0; j < K; j++) {
		ads[j].views++;
		if (placement[j] == n)
		    break;
	    }
	}
    }
    struct Sequence {
	int n;
	Sequence() {n = 0;}
	int operator() () { return n++; }
    };
    struct CompareByScore {
	const vector<Ad>* ads;
	CompareByScore(vector<Ad>* _ads) : ads(_ads) {
	}
	bool operator () (int i, int j) {
	    return (*ads)[i].score > (*ads)[j].score;
	}
    };
    void update_placement() {
	for_each(ads.begin(), ads.end(), mem_fun_ref(&Ad::update_score));
	vector<int> new_order(ads.size(), 0);
	generate(new_order.begin(), new_order.end(), Sequence());
	sort(new_order.begin(), new_order.end(), CompareByScore(&ads));
	copy(new_order.begin(), new_order.begin() + K, placement.begin());
    }
};
