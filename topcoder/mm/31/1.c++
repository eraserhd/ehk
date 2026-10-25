#include <cassert>
#include <iostream>
#include <string>
#include <vector>
using namespace std;
#define BEGIN {
#define END }

namespace MM30 BEGIN

int N;

vector<int> getNextMove(const vector<string>& board) {
    N = board.size();
    int best_score=0, best_i=-1, best_j=-1;
    for (int i = 0; i < N; ++i)
	for (int j = 0; j < N; ++j) {
	    if (board[i][j]!='.')
		continue;
	    const int DX[] = {-1,0,1,-1,1,-1,0,1};
	    const int DY[] = {-1,-1,-1,0,0,1,1,1};
	    bool found=false;
	    int score=0;
	    for (int d = 0; d < 8; ++d) {
		int dx = DX[d], dy = DY[d];
		int ii=i+dy, jj=j+dx, n=0; 
		int lscore=0;
		for (;;) {
		    if (ii<0 || ii>=N || jj<0 || jj>=N) break;
		    if (board[ii][jj]=='.') break;
		    if (board[ii][jj]=='1') {
			found=true;
			score+=lscore;
			break;
		    }
		    ++lscore;
		    ii+=dy, jj+=dx;
		}
	    }
	    if (found && score>0)
		if ((i==0 || i==N-1) && (j==0 || j==N-1))
		    score+=10;
	    if (found && score>best_score)
		best_score=score, best_i=i, best_j=j;
	}
    assert(best_score>0);
    vector<int> result(2);
    cerr<<"MOVE:"<<best_j<<","<<best_i<<endl;
    result[0]=best_j, result[1]=best_i;
    return result;
}

END 

struct MultiplayerReversi {
    inline vector<int> getNextMove(const vector<string>& board) {
	return MM30::getNextMove(board);
    }
};
