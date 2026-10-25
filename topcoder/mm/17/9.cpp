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
int per_col[100*100][100];
int first_col[100*100];
int colors = 0;
int color0 = 0;

struct Field {
    vector<string> board;
    int disjoint_set[100*100];
    int set_sizes[100*100];
    long long score;
    int width, height;

    Field(const vector<string>& board) : board(board), height(board.size()), width(board[0].size()) {}

    void find_sets() {
	fill(disjoint_set, disjoint_set+100*100, 65536 + 65535);
	for (int i = 0; i < board.size(); ++i) {
	    for (int j = 0; j < board[0].size(); ++j) {
		char col = board[i][j];
		if (col == '.')
		    continue;
		if (i > 0 && board[i-1][j] == col)
		    disjoint_merge(i*100+j,(i-1)*100+j);
		if (j > 0 && board[i][j-1] == col)
		    disjoint_merge(i*100+j,i*100+(j-1));
	    }
	}

	memset(set_sizes, 0, sizeof(set_sizes));
	for (int i = 0; i < board.size(); ++i) {
	    for (int j = 0; j < board[0].size(); ++j) {
		int n = disjoint_find(i*100+j);
		++set_sizes[n];
	    }
	}

	score = 0LL;
	for (int i = 0; i < 100*100; ++i)
	    if (set_sizes[i] > 1)
		score += set_sizes[i]*(set_sizes[i]-1);
    }

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

    int best_move() {
	int result = -1;
	long long best_score = 0;
	for (int n = 0; n < 100*100; ++n) {
	    if (set_sizes[n] < 2) 
		continue;
	    if (board[n/100][n%100] == color0+'A')
		continue;
	    vector<string> new_board(board);
	    for (int j = 0; j < width; ++j) {
		int top = height-1;
		for (int i = height-1; i >= 0; --i) {
		    if (disjoint_find(i*100+j) != n)
			new_board[top--][j] = new_board[i][j];
		}
		while (top >= 0)
		    new_board[top--][j] = '.';
	    }

	    Field subfield(new_board);
	    subfield.find_sets();
	    long long move_score = set_sizes[n]*(set_sizes[n]-1) + subfield.score;
	    if (move_score > best_score) {
		best_score = move_score;
		result = n;
	    }
	}
	return result;
    }
};

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
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
            char col = board[i][j];
            if (col == '.')
                continue;
	    if (colors < col-'A')
		colors = col-'A';
            if (i > 0 && board[i-1][j] == col)
                disjoint_merge(i*100+j,(i-1)*100+j);
            if (j > 0 && board[i][j-1] == col)
                disjoint_merge(i*100+j,i*100+(j-1));
        }
    }

    memset(set_sizes, 0, sizeof(set_sizes));
    static int column_sets[100][100];
    int column_set_sizes[100] = {0};
    int start_j = INT_MAX, color0_move = -1;
    int rightmost_stuck_column = -1;
    for (int j = 0; j < width; ++j) {
	int is_stuck = 1;
	for (int i = 0; i < height; ++i) {
            int n = disjoint_find(i*100+j);
	    switch (++set_sizes[n]) {
	    case 1:
		first_col[n] = j;
		memset(per_col[n], 0, sizeof(per_col[0]));
		break;
	    case 2:
		is_stuck = 0;
		if (board[i][j] == color0+'A') {
		    if (color0_move == -1)
			color0_move = n;
		} else
		    start_j = min(start_j, first_col[n]);
		break;
	    default:
		is_stuck = 0;
		break;
	    }
	    if (++per_col[n][j] == 1)
		if (board[i][j] != color0+'A')
		    column_sets[j][column_set_sizes[j]++] = n;
        }
	if (is_stuck)
	    rightmost_stuck_column = j;
    }

    if (color0_move != -1 && color0_move%100 < rightmost_stuck_column)
	return color0_move;

    int noncolor0_move = -1;
    int smallest_col_count = INT_MAX;
    if (start_j != INT_MAX) {
	for (int kk = 0; kk < column_set_sizes[start_j]; ++kk) {
	    int n = column_sets[start_j][kk];
	    if (set_sizes[n] < 2)
		continue;
	    if (per_col[n][start_j] < smallest_col_count) {
		smallest_col_count = per_col[n][start_j];
		noncolor0_move = n;
	    }
	}
	if (noncolor0_move != -1)
	    return noncolor0_move;
    }
    return color0_move;
}

time_t end = -1;
vector<int> move() {
    if (end == -1)
#if defined(ARGO)
	end = time(NULL)+45;
#else
	end = time(NULL)+20;
#endif

    int n = -1;
    if (time(NULL) < end && colors >= 6) {
	Field field(board);
	field.find_sets();
	n = field.best_move();
    }
    if (n == -1)
	n = find_moves();

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
