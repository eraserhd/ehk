#include <algorithm>
#include <cassert>
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

const int PIECE_MASKS[25][4] = {
    {15,33825,15,33825},
    {31,1082401,31,1082401},
    {39,3105,228,2115},
    {47,99361,488,67651},
    {71,1121,226,2146},
    {79,35873,484,67682},
    {99,99,99,99},
    {102,2145,102,2145},
    {103,3169,230,2147},
    {110,68641,236,67681},
    {135,1059,225,3138},
    {143,33889,482,68674},
    {167,3107,229,3139},
    {195,1122,195,1122},
    {199,1123,227,3170},
    {271,33827,481,100418},
    {391,33890,451,35906},
    {1126,6241,3268,4291},
    {2119,1249,7234,4324},
    {2150,2273,3266,4322},
    {2243,1250,6242,2276},
    {2274,2274,2274,2274},
    {3142,4321,3142,4321},
    {6211,1252,6211,1252},
    {7201,7300,4231,1063}
};
const long long BORDERS_MASKS[25][4] = {
    {495774LL,70072222338LL,495774LL,70072222338LL},
    {1024190LL,8969244459650LL,1024190LL,8969244459650LL},
    {4409486LL,1629569666LL,29641480LL,1094878342LL},
    {4673694LL,208584917634LL,63461136LL,140144444550LL},
    {8554638LL,547504770LL,29639940LL,1094862084LL},
    {8818846LL,70080610946LL,63458056LL,140144428292LL},
    {12731526LL,12731526LL,12731526LL,12731526LL},
    {12732684LL,1094861442LL,12732684LL,1094861442LL},
    {12732558LL,1629635202LL,29640972LL,1094861958LL},
    {12996892LL,140142264962LL,29643544LL,140144427650LL},
    {17139854LL,547439750LL,29640322LL,1629652228LL},
    {17141918LL,70072287874LL,63458564LL,140142347524LL},
    {21317774LL,1629570182LL,29641354LL,1629652102LL},
    {25461894LL,547505412LL,25461894LL,547505412LL},
    {25462926LL,547505286LL,29639814LL,1629635844LL},
    {34312350LL,70072222854LL,63458946LL,208595485956LL},
    {50956430LL,70072288516LL,59280518LL,70080693508LL},
    {547506444LL,3259122306LL,1629784584LL,2189722758LL},
    {1094879374LL,564414082LL,3793913092LL,2193902344LL},
    {1094863116LL,1094993538LL,1629783300LL,2193900804LL},
    {1095009414LL,564413700LL,3259122948LL,1094994696LL},
    {1094993156LL,1094993156LL,1094993156LL,1094993156LL},
    {1629653260LL,2193901186LL,1629653260LL,2193901186LL},
    {3259139206LL,564415240LL,3259139206LL,564415240LL},
    {3793961602LL,3794110984LL,2189789326LL,547571854LL}
};

int piece;
int width, height;
unsigned board[100][4];

struct Placement {
    int i, j, r;
} placement;

int best_score;
vector<int> best_move;

bool initialized = false;
struct timeval end;

int make_mask(const vector<string>& repr) {
    int result = 0;
    for (int i = 0; i < repr.size(); ++i)
        for (int j = 0; j < repr[0].size(); ++j)
            if (i < repr.size() && j < repr[0].size() && repr[i][j] == 'X')
                result |= (1<<(i*5+j));
    return result;
}

int find_piece(const vector<string>& repr) {
    int m = make_mask(repr);
    int low = 0, high = 24;
    while (low <= high) {
        int mid = (low + high)/2;
        if (m < PIECE_MASKS[mid][0])
            high = mid-1;
        else if (m > PIECE_MASKS[mid][0])
            low = mid+1;
        else
            return mid;
    }
    assert(false);
    return -1;
}

bool have_time() {
    struct timeval now;
    gettimeofday(&now,NULL);
    return timercmp(&now,&end,<);
}

bool is_board_x(int i, int j) {
    if (i<0 || j<0 || i>=height || j>=width)
        return true;
    return (board[i][j/32] & (1U<<(j%32))) != 0;
}

inline bool is_piece_x(int i, int j) {
    int ii = i-placement.i, jj = j-placement.j;
    if (ii < 0 || jj < 0 || ii >= 5 || jj >= 5)
        return false;
    return (PIECE_MASKS[piece][placement.r] & (1<<(ii*5+jj))) != 0;
}

bool is_x(int i, int j) {
    return is_board_x(i,j) || is_piece_x(i,j);
}

bool fits() {
    for (int bits = PIECE_MASKS[piece][placement.r]; bits; bits ^= (bits & -bits)) {
        int jj = __builtin_ctz(bits);
        int ii = jj/5;
        jj %= 5;
        if (is_board_x(placement.i+ii, placement.j+jj) &&
            is_piece_x(placement.i+ii, placement.j+jj))
            return false;
    }
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
    if (esize < 4 && !is_x(i+1,j) && !empty_seen[i+1][j]) {
        ++esize;
        empty_size(i+1,j);
    }
    if (esize < 4 && !is_x(i-1,j) && !empty_seen[i-1][j]) {
        ++esize;
        empty_size(i-1,j);
    }
    if (esize < 4 && !is_x(i,j+1) && !empty_seen[i][j+1]) {
        ++esize;
        empty_size(i,j+1);
    }
    if (esize < 4 && !is_x(i,j-1) && !empty_seen[i][j-1]) {
        ++esize;
        empty_size(i,j-1);
    }
}

void score() {
    int score = 0;
    for (long long bits = BORDERS_MASKS[piece][placement.r]; bits; bits ^= (bits & -bits)) {
        int jj = __builtin_ctzll(bits);
        int ii = (jj/7)-1;
        jj = (jj%7)-1;
        if (is_board_x(placement.i+ii, placement.j+jj))
            ++score;
    }

    if (score < best_score)
        return;

    memset(empty_seen,0,sizeof(empty_seen));
    for (long long bits = BORDERS_MASKS[piece][placement.r]; bits; bits ^= (bits & -bits)) {
        int jj = __builtin_ctzll(bits);
        int ii = (jj/7)-1;
        jj = (jj%7)-1;
        if (is_board_x(placement.i+ii, placement.j+jj))
            continue;
        if (empty_seen[placement.i+ii][placement.j+jj])
            continue;
        esize = 1;
        empty_size(placement.i+ii,placement.j+jj);
        if (esize >= 4)
            continue;
        //cerr<<"esize="<<esize<<endl;
        score -= 2*esize;
        if (score < best_score)
            return;
    }
    if (score > best_score) {
        best_score = score;
        best_move.resize(3);
        best_move[0] = placement.j;
        best_move[1] = placement.i;
        best_move[2] = placement.r;
    }
}

vector<int> placeTile(int _piece) {
    best_score = -9999999;
    best_move.clear();

    if (!have_time()) {
        cerr <<"Out of time - bailing."<<endl;
        return best_move;
    }

    piece = _piece;

    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
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

    for (int bits = PIECE_MASKS[piece][best_move[2]]; bits; bits ^= (bits & -bits)) {
        int jj = __builtin_ctz(bits);
        int ii = jj/5;
        jj %= 5;
        board[best_move[1]+ii][(best_move[0]+jj)/32] |= (1U<<((best_move[0]+jj)%32));
    }

    return best_move;
}

END

struct ShapePackaging {
    inline vector<int> placeTile(const vector<string>& _board, const vector<string>& piece) {
        using namespace MM21;
        if (!initialized) {
            gettimeofday(&end,NULL);
            end.tv_sec += 34;
            memset(board, 0, sizeof(board));
            height = _board.size();
            width = _board[0].size();
            initialized = true;
        }
        return MM21::placeTile(MM21::find_piece(piece));
    }
};
