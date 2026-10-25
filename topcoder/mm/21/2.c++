#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iterator>
#include <string>
#include <sys/time.h>
#include <time.h>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }

namespace MM21 BEGIN

vector<string> board;
vector<string> rotations[4];

struct Placement {
    int i, j, r;
} placement;

int best_score;
vector<int> best_move;

bool initialized = false;
struct timeval end;

bool have_time() {
    struct timeval now;
    gettimeofday(&now,NULL);
    return timercmp(&now,&end,<);
}

vector<string> rotate90(const vector<string>& input) {
    vector<string> result(input[0].size(), string(input.size(), '.'));
    for (int i = 0; i < input[0].size(); ++i)
        for (int j = 0; j < input.size(); ++j)
            result[i][j] = input[j][input[0].size()-i-1];
    return result;
}

void make_rotations() {
    for (int i = 1; i < 4; ++i)
        rotations[i] = rotate90(rotations[i-1]);
}

bool is_board_x(int i, int j) {
    if (i<0 || j<0 || i>=board.size() || j>=board[0].size())
        return true;
    return (board[i][j] == 'X');
}

inline bool is_piece_x(int i, int j) {
    vector<string>& piece = rotations[placement.r];
    int piece_i = i-placement.i, piece_j = j-placement.j;
    if (piece_i < 0 || piece_i >= piece.size())
        return false;
    if (piece_j < 0 || piece_j >= piece[0].size())
        return false;
    return (piece[piece_i][piece_j] == 'X');
}

bool is_x(int i, int j) {
    return is_board_x(i,j) || is_piece_x(i,j);
}

bool fits() {
    vector<string>& piece = rotations[placement.r];
    for (int ii = 0; ii < piece.size(); ++ii)
        for (int jj = 0; jj < piece[0].size(); ++jj)
            if (is_board_x(placement.i+ii, placement.j+jj) &&
                is_piece_x(placement.i+ii, placement.j+jj))
                return false;
    return true;
}

bool borders_piece_x(int i, int j) {
    return is_piece_x(i+1,j) || is_piece_x(i-1,j) ||
           is_piece_x(i,j+1) || is_piece_x(i,j-1);
}

int esize;
char empty_seen[101][101];
void empty_size(int i, int j) {
    empty_seen[i][j] = 1;
    if (esize < 4 && !is_x(i+1,j)) {
        ++esize;
        empty_size(i+1,j);
    }
    if (esize < 4 && !is_x(i-1,j)) {
        ++esize;
        empty_size(i-1,j);
    }
    if (esize < 4 && !is_x(i,j+1)) {
        ++esize;
        empty_size(i,j+1);
    }
    if (esize < 4 && !is_x(i,j-1)) {
        ++esize;
        empty_size(i,j-1);
    }
}

void score() {
    int score = 0;
    vector<string>& piece = rotations[placement.r]; 
    memset(empty_seen,0,sizeof(empty_seen));
    for (int ii = -1; ii <= (int)piece.size(); ++ii) {
        for (int jj = -1; jj <= (int)piece[0].size(); ++jj) {
            if (is_piece_x(placement.i+ii, placement.j+jj))
                continue;
            if (!borders_piece_x(placement.i+ii, placement.j+jj))
                continue;
            if (is_board_x(placement.i+ii, placement.j+jj)) {
                ++score;
                continue;
            }
            if (empty_seen[placement.i+ii][placement.j+jj])
                continue;
            esize = 1;
            empty_size(placement.i+ii,placement.j+jj);
            if (esize >= 4)
                continue;
            score -= esize;
        }
    }

    if (score > best_score) {
        best_score = score;
        best_move.resize(3);
        best_move[0] = placement.j;
        best_move[1] = placement.i;
        best_move[2] = placement.r;
    }
}

vector<int> placeTile(vector<string> _board, vector<string> _piece) {
    if (!initialized) {
        gettimeofday(&end,NULL);
        end.tv_sec += 28;
        initialized = true;
    }

    best_score = -9999999;
    best_move.clear();

    if (!have_time())
        return best_move;

    board = _board;

    rotations[0] = _piece;
    make_rotations();

    for (int i = 0; i < board.size(); ++i) {
        for (int j = 0; j < board[0].size(); ++j) {
            for (int r = 0; r < 4; ++r) {
                placement.i = i;
                placement.j = j;
                placement.r = r;
                if (!fits())
                    continue;
                score();
            }
        }
    }

    return best_move;
}

END

struct ShapePackaging {
    inline vector<int> placeTile(vector<string> board, vector<string> piece) {
        return MM21::placeTile(board, piece);
    }
};
