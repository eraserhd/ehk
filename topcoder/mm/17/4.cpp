#include <vector>
#include <string>
#include <queue>
#include <algorithm>
#include <iostream>
using namespace std;

#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)
#define ALL(c) (c).begin(),(c).end()

#define BEGIN {
#define END }
namespace MM17 BEGIN

vector<string> board;
int disjoint_set[100*100];
int set_sizes[100*100];
int colors = 0;

int disjoint_find(int n) {
    if (disjoint_set[n]%65536 == 65535)
	return n;
    return disjoint_set[n] = disjoint_find(disjoint_set[n]%65536);
}

void disjoint_merge(int l, int r) {
    int lv = disjoint_find(l), rv = disjoint_find(r);
    if (lv == rv)
        return;
    int ld = disjoint_set[lv]/65536, rd = disjoint_set[rv]/65536;
    if (ld > rd)
        disjoint_set[rv] = lv;
    else if (ld < rd)
        disjoint_set[lv] = rv;
    else {
        disjoint_set[rv] = lv;
        disjoint_set[lv] += 65536;
    }
}

int find_moves() {
    fill(disjoint_set, disjoint_set+100*100, 65536 + 65535);
    for (int i = 0; i < board.size(); ++i) {
        for (int j = 0; j < board[0].size(); ++j) {
            char col = board[i][j];
            if (col == '.')
                continue;
	    colors = max(col-'A'+1, colors);
            if (i > 0 && board[i-1][j] == col)
                disjoint_merge(i*100+j,(i-1)*100+j);
            if (j > 0 && board[i][j-1] == col)
                disjoint_merge(i*100+j,i*100+(j-1));
        }
    }

    memset(set_sizes, 0, sizeof(set_sizes));
    vector<int> djs;
    for (int j = 0; j < board[0].size(); ++j) {
	for (int i = 0; i < board.size(); ++i) {
            int n = disjoint_find(i*100+j);
	    if (++set_sizes[n] == 2)
		djs.push_back(n);
        }
    }

    int best_score = -1, best_n = -1;
    TR(djs,di) {
	if (set_sizes[*di] <= 1)
	    continue;
	int colnum = board[(*di)/100%100][(*di)%100]-'A';
	int score = colnum*65536;//+set_sizes[*di];
	if (score > best_score) {
	    best_score = score;
	    best_n = *di;
	}
    }
    
    return best_n;
}

vector<int> move() {
    int n = find_moves();
    vector<int> result(2,0);
    if (n >= 0) {
        result[0] = n/100%100;
        result[1] = n%100;
    }
    return result;
}

END;

struct ContinuousSameGame {
    vector<int> move(const vector<string>& board) {
        MM17::board = board;
        return MM17::move();
    }
};
