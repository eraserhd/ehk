#include <vector>
#include <string>
#include <queue>
#include <algorithm>
#include <iostream>
#include <iterator>
using namespace std;

#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)
#define ALL(c) (c).begin(),(c).end()

#define BEGIN {
#define END }
namespace MM17 BEGIN

int width, height;
vector<string> board;
int colors = 0;
int color0 = 0;
time_t end = -1;

struct DisjointSet {
    int data[100*100];
    DisjointSet() {
	clear();
    }
    DisjointSet(const vector<string>& new_board) {
	parse_board(new_board);
    }
    void clear() {
	for (int n = 0; n < 100*100; ++n)
	    data[n] = 65536+65535;
    }
    void parse_board(const vector<string>& new_board) {
	int height = new_board.size();
	int width = new_board[0].size();
	clear();
	for (int i = 0; i < height; ++i) {
	    for (int j = 0; j < width; ++j) {
		char col = new_board[i][j];
		if (col == '.')
		    continue;
		if (i > 0 && col == new_board[i-1][j])
		    merge(i*100+j, (i-1)*100+j);
		if (j > 0 && col == new_board[i][j-1])
		    merge(i*100+j, i*100+(j-1));
	    }
	}
    }
    int find(int n) {
	if (data[n]%65536 == 65535)
	    return n;
	return data[n] = find(data[n]%65536);
    }
    void merge(int l, int r) {
	int lv = find(l), rv = find(r);
	if (lv == rv)
	    return;
	int ld = data[lv]/65536, rd = data[rv]/65536;
	if (ld > rd)
	    data[rv] = lv;
	else if (ld < rd)
	    data[lv] = rv;
	else {
	    data[rv] = lv;
	    data[lv] += 65536;
	}
    }
};

DisjointSet djs;

// Stake in the mud: Prefer 1) unstuck 2) non0 color above bumper 3) 0-score maximizer in bumper
// 4) Anything else

int calculate_score0(DisjointSet& djs, const vector<string>& board, int set_sizes[]) {
    int result = 0;
    char got[100*100] = {0};
    for (int i = 0; i < height; ++i) {
	for (int j = 0; j < width; ++j) {
	    if (board[i][j] == '.' || board[i][j] != 'A'+color0)
		continue;
	    int n = djs.find(i*100+j);
	    if (set_sizes[n] < 2)
		continue;
	    if (got[n])
		continue;
	    ++got[n];
	    result += set_sizes[n]*(set_sizes[n]-1);
	}
    }
    return result;
}

void do_erase(vector<string>& board, int i, int j, int& left, int &right) {
    char col = board[i][j];
    board[i][j] = ' ';
    if (j < left)
	left = j;
    if (j > right)
	right = j;
    if (i > 0 && col == board[i-1][j])
	do_erase(board, i-1, j, left, right);
    if (j > 0 && col == board[i][j-1])
	do_erase(board, i, j-1, left, right);
    if (i+1 < height && col == board[i+1][j])
	do_erase(board, i+1, j, left, right);
    if (j+1 < width && col == board[i][j+1])
	do_erase(board, i, j+1, left, right);
}

int score0_after_move(const vector<string>& board, int n, int limit = (time(NULL) < end ? 2 : 1),
		      int left = INT_MAX, int right = 0) {
    vector<string> new_board(board);
    int move_left = INT_MAX, move_right = 0;
    do_erase(new_board, n/100, n%100, move_left, move_right);
    int lleft = 0;
    for (int j = 0; j < width; ++j) {
	int top = height-1;
	for (int i = height-1; i >= 0; --i) {
	    if (new_board[i][j] != ' ')
		new_board[top--][lleft] = new_board[i][j];
	}
	while (top >= 0)
	    new_board[top--][lleft] = '.';
	if (new_board[height-1][lleft] != '.')
	    ++lleft;
    }
    while (lleft < width) {
	for (int i = 0; i < height; ++i)
	    new_board[i][lleft] = '.';
	++lleft;
    }

    left = min(left, move_left-1);
    right = max(right, move_right+1);

    DisjointSet new_djs(new_board);
    int set_sizes[100*100] = {0};
    for (int i = 0; i < height; ++i)
	for (int j = 0; j <width; ++j)
	    if (board[i][j] != '.')
		++set_sizes[new_djs.find(i*100+j)];

    int score0 = calculate_score0(new_djs, new_board, set_sizes);
    if (limit == 0)
	return score0;

    char got[100*100] = {0};
    for (int i = 0; i < height; ++i) {
	for (int j = max(0,left); j <= min(right,width-1); ++j) {
	    int n = new_djs.find(i*100+j);
	    if (set_sizes[n] < 2)
		continue;
	    if (got[n])
		continue;
	    ++got[n];
	    score0 = max(score0, score0_after_move(new_board, n, limit-1, left, right));
	}
    }
    return score0;
}

int maximize_0score(int set_sizes[]) {
    int result = -1;
    int best_score0 = calculate_score0(djs, board, set_sizes);

    char got[100*100] = {0};
    for (int i = 0; i < height; ++i) {
	for (int j = 0; j < width; ++j) {
	    if (board[i][j] == '.' && board[i][j] == 'A'+color0)
		continue;
	    int n = djs.find(i*100+j);
	    if (got[n])
		continue;
	    ++got[n];
	    if (set_sizes[n] < 2)
		continue;
	    int try_score0 = score0_after_move(board, n);
	    if (try_score0 > best_score0) {
		result = n;
		best_score0 = try_score0;
	    }
	}
    }

    return result;
}

int find_moves() {
    djs.parse_board(board);
    TR(board,bi)
	colors = max(colors,'A'-*max_element(ALL(*bi))+1);

    int set_sizes[100*100] = {0};
    for (int i = 0; i < height; ++i) {
	for (int j = 0; j < width; ++j) {
	    if (board[i][j] == '.')
		continue;
	    int n = djs.find(i*100+j);
	    ++set_sizes[n];
	}
    }

    int rightmost_stuck_column = -1; 
    int stuck_move = -1;
    for (int j = 0; j < width; ++j) {
	bool stuck = true;
	for (int i = 0; i < height; ++i) {
	    char col = board[i][j];
	    if (col == '.')
		continue;
	    int n = djs.find(i*100+j);
	    if (set_sizes[n] >= 2) {
		if (stuck_move == -1)
		    stuck_move = n;
		stuck = false;
		break;
	    }
	}
	if (stuck && stuck_move != -1)
	    return stuck_move;
    }

    const int BUFFER_SIZE = 2;
    for (int j = 0; j < width; ++j) {
	for (int i = height-BUFFER_SIZE-1; i >= 0; --i) {
	    char col = board[i][j];
	    if (col == '.' || col == 'A'+color0)
		continue;
	    int n = djs.find(i*100+j);
	    if (set_sizes[n] < 2)
		continue;
	    return n;
	}
    }

    int n = maximize_0score(set_sizes);
    if (n != -1)
	return n;

    for (int i = 0; i < height; ++i) {
	for (int j = 0; j < width; ++j) {
	    int n = djs.find(i*100+j);
	    if (set_sizes[n] < 2)
		continue;
	    return n;
	}
    }
    return -1;
}

vector<int> move() {
    if (end == -1)
#if defined(ARGO)
	end = time(NULL)+45;
#else
	end = time(NULL)+20;
#endif

    int n = find_moves();

    vector<int> result(2,0);
    if (n >= 0) {
        result[0] = n/100%100;
        result[1] = n%100;
    } else {
	cerr << "no move!" <<endl;
    }
    return result;
}

END;

struct ContinuousSameGame {
    vector<int> move(const vector<string>& board) {
	// This only runs the first time so we have the color count for our first
	// decision
	if (MM17::colors == 0) {
	    static int colcounts[50] = {0};
	    for (int i = 0; i < board.size(); ++i)
		for (int j = 0; j < board[0].size(); ++j)
		    if (board[i][j] != '.') {
			MM17::colors = max(board[i][j]-'A'+1, MM17::colors);
			++colcounts[board[i][j]-'A'];
		    }
	    int best = colcounts[0];
	    for (int k = 1; k < MM17::colors; ++k)
		if (colcounts[k] > best) {
		    MM17::color0 = k;
		    best = colcounts[k];
		}
	}

	MM17::height = board.size();
	MM17::width = board[0].size();
        MM17::board = board;
        return MM17::move();
    }
};
