#include <algorithm>
#include <cassert>
#include <cctype>
#include <iostream>
#include <iterator>
#include <queue>
#include <string>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }

namespace MM30 BEGIN

const int DX[] = {-1,0,1,-1,1,-1,0,1};
const int DY[] = {-1,-1,-1,0,0,1,1,1};

int N;

struct Board {
    bool valid;
    int turn;
    char board[12][12];

    Board(const vector<string>& b) : turn(0), valid(true) {
	for (int i = 0; i < N; ++i)
	    for (int j = 0; j < N; ++j)
		board[i][j]=b[i][j];
    }

    inline double score() {
	int m=0,o=0;
	for (int i = 0; i < N; ++i)
	    for (int j = 0; j < N; ++j)
		if (isdigit(board[i][j]))
		    ++ (board[i][j]=='1' ? m : o);
	return double(m)/(o);
    }

    inline Board with_move(int i, int j) {
	Board result(*this);
	result.valid=false;
	if (board[i][j]!='.')
	    return result;
	for (int d = 0; d < 8; ++d) {
	    int dx = DX[d], dy = DY[d];
	    int ii=i+dy, jj=j+dx;
	    queue<pair<int,int> > q;
	    for (;;) {
		if (ii<0 || ii>=N || jj<0 || jj>=N) break;
		if (board[ii][jj]=='.') break;
		if (board[ii][jj]=='1'+turn) {
		    while (!q.empty()) {
			result.valid=true;
			result.board[q.front().first][q.front().second]='1'+turn;
			q.pop();
		    }
		    break;
		}
		q.push(make_pair(ii,jj));
		ii+=dy, jj+=dx;
	    }
	}
	result.turn = (result.turn+1)%((N&1) ? 3 : 4);
	return result;
    }

    double score_to_depth(int depth=2) {
	if (depth==0)
	    return score();
	int n=0,tm=0;
	double acc=0.;
	for (int i = 0; i < N; ++i)
	    for (int j = 0; j < N; ++j) {
		if (board[i][j]=='.')
		    ++tm;
		Board nb = with_move(i,j);
		if (nb.valid) {
		    acc += nb.score_to_depth(depth-1);
		    ++n;
		}
	    }
	if (n==0) {
	    // No moves left, return board's score
	    if (tm==0)
		return score();
	    
	    // Other players have moves, make them.
	    turn = (turn+1)%((N&1) ? 3 : 4);
	    return score_to_depth(depth-1);
	}
	return acc/n;
    }

};

vector<int> make_move(int i, int j) {
    vector<int> result(2);
    cerr<<"MOVE:"<<i<<","<<j<<endl;
    result[0]=j, result[1]=i;
    return result;
}

vector<int> getNextMove(const vector<string>& board) {
    N = board.size();
    Board bs(board);
    double best_score=-1e200;
    int best_i=-1, best_j=-1;
    for (int i = 0; i < N; ++i)
	for (int j = 0; j < N; ++j) {
	    Board bb = bs.with_move(i,j);
	    if (!bb.valid)
		continue;
	    if ((i==0 || i==N-1) && (j==0 || j==N-1))
		return make_move(i,j);
	    double sc = bb.score_to_depth();
	    if (sc>best_score)
		best_score=sc, best_i=i, best_j=j;
	}
	    
    return make_move(best_i,best_j);
}

END 

struct MultiplayerReversi {
    inline vector<int> getNextMove(const vector<string>& board) {
	return MM30::getNextMove(board);
    }
};
