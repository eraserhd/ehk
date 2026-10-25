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

int width, height;
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

int kill_easy_singles() {
    queue<int> try_harder;
    for (int i = height-1; i >= height-6; --i) {
	for (int j = 0; j < width; ++j) {
	    if (board[i][j] == '.')
		continue;
	    if (set_sizes[i*100+j] == 1 && board[i][j] != 'A') {
		// Possibilities: 1) The move above us will connect us with another.
		// 2) A move exists to our left or right which will connect us.  This happens
		// if there is a colored block n spaces above us and there is a move which will
		// drop us and it to the same level.
		if (i > 0 && board[i-1][j] != '.' && set_sizes[(i-1)*100+j] >= 2) {
		    int k = i-1;
		    char col = board[k][j];
		    while (board[k][j] == col && k > 0)
			--k;
		    if (board[k][j] == board[i][j])
			return (k+1)*100+j;
		    try_harder.push(i*100+j);
		}
	    }
	}
    }

    for (int i = 0; i < 5; ++i) {
	for (int j = 0; j < width; ++j) {
	    if (board[i][j] == '.')
		continue;
	    if (set_sizes[i*100+j] == 1 && board[i][j] != 'A') {
		// Possibilities: 1) The move above us will connect us with another.
		// 2) A move exists to our left or right which will connect us.  This happens
		// if there is a colored block n spaces above us and there is a move which will
		// drop us and it to the same level.
		if (set_sizes[(i+1)*100+j] >= 2) {
		    int k = i+1;
		    char col = board[k][j];
		    while (board[k][j] == col && k+1 < height)
			++k;
		    if (board[k][j] == board[i][j])
			return (k-1)*100+j;
		}
	    }
	}
    }

    //DEBUG:
    return -1;
    while (!try_harder.empty()) {
	int j = try_harder.front();
	try_harder.pop();
	int i = j/100;
	j %= 100;

	assert(i > 0);
	char col = board[i][j];
	int seeking[2][100] = {0};
	for (int k = i-1; k >= 0; --k) {
	    if (j > 0 && board[k][j-1] == col)
		seeking[0][k] = 1;
	    if (j+1 < width && board[k][j+1] == col)
		seeking[1][k] = 1;
	}
    }
    return -1;
}

int find_moves() {
    fill(disjoint_set, disjoint_set+100*100, 65536 + 65535);
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
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
    for (int j = 0; j < width; ++j) {
	for (int i = 0; i < height; ++i) {
            int n = disjoint_find(i*100+j);
	    if (++set_sizes[n] == 2)
		djs.push_back(n);
        }
    }

    int n = kill_easy_singles();
    if (n != -1)
	return n;

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
	MM17::height = board.size();
	MM17::width = board[0].size();
        MM17::board = board;
        return MM17::move();
    }
};
