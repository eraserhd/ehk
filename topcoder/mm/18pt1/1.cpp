#include <algorithm>
#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

namespace MM18 BEGIN

int B;
string from, to;
vector<int> suffix;

bool suffix_less(int l, int r) {
    if (l==r)
	return false;
    while (l < from.size() && r < from.size() && from[l] == from[r])
	++l, ++r;
    if (l == from.size() && r == from.size())
	return false;
    if (l == from.size())
	return true;
    if (r == from.size())
	return false;
    return from[l] < from[r];
}

int bsearch_cmp(int start_fi, int ti, pair<int, int>& best) {
    int size = 0;
    int fi = start_fi;
    while (fi < from.size() && ti < to.size() && from[fi] == to[ti])
	++fi, ++ti, ++size;
    if (size > best.second) {
	best.first = start_fi;
	best.second = size;
    }
    if (ti == to.size())
	return 0;
    if (fi == from.size())
	return -1;
    return to[ti] - from[fi];
}

pair<int, int> find_best_suffix(int ti) {
    //cerr << "Looking for:" << to.substr(ti,30) <<endl;
    int low = 0, high = suffix.size()-1, mid;
    pair<int, int> best(0,0);
    while (low < high) {
	mid = (low + high)/2;
	int cmp = bsearch_cmp(suffix[mid], ti, best);
	//cerr << cmp << "@" << mid << " " << from.substr(suffix[mid],30) << endl;
	if (cmp < 0)
	    high = mid - 1;
	else if (cmp > 0)
	    low = mid + 1;
	else
	    return best;
    }
    return best;
}

string transform() {
    suffix.resize(from.size());
    for (int i = 0; i < from.size(); ++i)
	suffix[i] = i;
    sort(ALL(suffix), suffix_less);

    vector<pair<int,int> > ranges;
    string MID;

    string::size_type ti = 0;
    pair<int, int> best_suffix;
    while (ti < to.size()) {
	best_suffix = find_best_suffix(ti);
	if (best_suffix.second > B) {
	    //cerr << "Selecting (" << best_suffix.first <<"," <<best_suffix.second <<") "
	    // << from.substr(best_suffix.first, 30) <<endl;
	    //cerr << "Matches: " << to.substr(ti, 30) <<endl;
	    ranges.push_back(best_suffix);
	    MID += string(best_suffix.second, 'M');
	    ti += best_suffix.second;
	} else {
	    MID += 'I';
	    ++ti;
	}
    }

    ostringstream out;
    TR(ranges, ri)
	out << ri->first << "-" << ri->first+ri->second-1 << " ";
    out << MID;
    return out.str();
}

END

struct BlockEditDistance {
    vector<string> transform(const vector<string>& versions, int B) {
	MM18::B = B;
	MM18::to = versions.back();
	vector<string> result;
	for (int i = 0; i < versions.size()-1; ++i) {
	    MM18::from = versions[i];
	    result.push_back(MM18::transform());
	}
	return result;
    }
};