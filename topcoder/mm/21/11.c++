#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iterator>
#include <list>
#include <queue>
#include <set>
#include <string>
#include <sys/time.h>
#include <time.h>
#include <utility>
#include <vector>
using namespace std;
#define ALL(c) (c).begin(),(c).end()
#define BEGIN {
#define END }
#define TR(c,i) for (typeof((c).begin()) i = (c).begin(); i != (c).end(); ++i)

namespace MM21 BEGIN

const int ENDGAME_THRESHOLD = 65;

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
const signed char BORDER_TRACE[25][4][19] = {
    {
        {0,1,2,3,4,5,13,21,20,19,18,17,16,8,-1},
        {0,1,2,10,18,26,34,42,41,40,32,24,16,8,-1},
        {0,1,2,3,4,5,13,21,20,19,18,17,16,8,-1},
        {0,1,2,10,18,26,34,42,41,40,32,24,16,8,-1},
    },
    {
        {0,1,2,3,4,5,6,14,22,21,20,19,18,17,16,8,-1},
        {0,1,2,10,18,26,34,42,50,49,48,40,32,24,16,8,-1},
        {0,1,2,3,4,5,6,14,22,21,20,19,18,17,16,8,-1},
        {0,1,2,10,18,26,34,42,50,49,48,40,32,24,16,8,-1},
    },
    {
        {0,1,2,3,4,12,20,19,18,26,25,24,16,8,-1},
        {0,1,2,10,18,19,27,35,34,33,32,24,16,8,-1},
        {2,3,4,12,20,28,27,26,25,24,16,8,9,10,-1},
        {0,1,2,3,11,19,27,35,34,33,25,17,16,8,-1},
    },
    {
        {0,1,2,3,4,5,13,21,20,19,18,26,25,24,16,8,-1},
        {0,1,2,10,18,26,27,35,43,42,41,40,32,24,16,8,-1},
        {3,4,5,13,21,29,28,27,26,25,24,16,8,9,10,11,-1},
        {0,1,2,3,11,19,27,35,43,42,41,33,25,17,16,8,-1},
    },
    {
        {0,1,2,3,4,12,20,19,27,26,25,17,16,8,-1},
        {0,1,2,10,11,19,27,26,34,33,32,24,16,8,-1},
        {1,2,3,11,12,20,28,27,26,25,24,16,8,9,-1},
        {1,2,3,11,19,27,35,34,33,25,24,16,8,9,-1},
    },
    {
        {0,1,2,3,4,5,13,21,20,19,27,26,25,17,16,8,-1},
        {0,1,2,10,18,19,27,35,34,42,41,40,32,24,16,8,-1},
        {2,3,4,12,13,21,29,28,27,26,25,24,16,8,9,10,-1},
        {1,2,3,11,19,27,35,43,42,41,33,25,24,16,8,9,-1},
    },
    {
        {0,1,2,3,11,19,27,26,25,24,16,8,-1},
        {0,1,2,3,11,19,27,26,25,24,16,8,-1},
        {0,1,2,3,11,19,27,26,25,24,16,8,-1},
        {0,1,2,3,11,19,27,26,25,24,16,8,-1},
    },
    {
        {1,2,3,4,12,20,19,27,26,25,24,16,8,9,-1},
        {0,1,2,10,11,19,27,35,34,33,25,24,16,8,-1},
        {1,2,3,4,12,20,19,27,26,25,24,16,8,9,-1},
        {0,1,2,10,11,19,27,35,34,33,25,24,16,8,-1},
    },
    {
        {0,1,2,3,4,12,20,19,27,26,25,24,16,8,-1},
        {0,1,2,10,11,19,27,35,34,33,32,24,16,8,-1},
        {1,2,3,4,12,20,28,27,26,25,24,16,8,9,-1},
        {0,1,2,3,11,19,27,35,34,33,25,24,16,8,-1},
    },
    {
        {1,2,3,4,5,13,21,20,19,27,26,25,24,16,8,9,-1},
        {0,1,2,10,18,19,27,35,43,42,41,33,32,24,16,8,-1},
        {2,3,4,5,13,21,20,28,27,26,25,24,16,8,9,10,-1},
        {0,1,2,10,11,19,27,35,43,42,41,33,25,24,16,8,-1},
    },
    {
        {0,1,2,3,4,12,20,28,27,26,18,17,16,8,-1},
        {0,1,2,3,11,19,18,26,34,33,32,24,16,8,-1},
        {0,1,2,10,11,12,20,28,27,26,25,24,16,8,-1},
        {1,2,3,11,19,27,35,34,33,32,24,16,17,9,-1},
    },
    {
        {0,1,2,3,4,5,13,21,20,28,27,26,18,17,16,8,-1},
        {0,1,2,10,11,19,27,26,34,42,41,40,32,24,16,8,-1},
        {1,2,3,11,12,13,21,29,28,27,26,25,24,16,8,9,-1},
        {1,2,3,11,19,27,35,43,42,41,33,32,24,16,17,9,-1},
    },
    {
        {0,1,2,3,4,12,20,28,27,26,18,26,25,24,16,8,-1},
        {0,1,2,3,11,19,18,19,27,35,34,33,32,24,16,8,-1},
        {0,1,2,10,2,3,4,12,20,28,27,26,25,24,16,8,-1},
        {0,1,2,3,11,19,27,35,34,33,32,24,16,17,16,8,-1},
    },
    {
        {0,1,2,3,11,12,20,28,27,26,25,17,16,8,-1},
        {1,2,3,11,19,27,26,34,33,32,24,16,8,9,-1},
        {0,1,2,3,11,12,20,28,27,26,25,17,16,8,-1},
        {1,2,3,11,19,27,26,34,33,32,24,16,8,9,-1},
    },
    {
        {0,1,2,3,4,12,20,28,27,26,25,17,16,8,-1},
        {0,1,2,3,11,19,27,26,34,33,32,24,16,8,-1},
        {0,1,2,3,11,12,20,28,27,26,25,24,16,8,-1},
        {1,2,3,11,19,27,35,34,33,32,24,16,8,9,-1},
    },
    {
        {0,1,2,3,4,5,13,21,29,28,27,19,18,17,16,8,-1},
        {0,1,2,3,11,19,18,26,34,42,41,40,32,24,16,8,-1},
        {0,1,2,10,11,12,13,21,29,28,27,26,25,24,16,8,-1},
        {1,2,3,11,19,27,35,43,42,41,40,32,24,25,17,9,-1},
    },
    {
        {0,1,2,3,4,12,13,21,29,28,27,26,18,17,16,8,-1},
        {1,2,3,11,19,27,26,34,42,41,40,32,24,16,8,9,-1},
        {0,1,2,3,11,12,13,21,29,28,27,26,25,17,16,8,-1},
        {1,2,3,11,19,27,35,34,42,41,40,32,24,16,17,9,-1},
    },
    {
        {1,2,3,4,12,20,19,27,26,34,33,32,24,16,8,9,-1},
        {0,1,2,10,11,19,20,28,36,35,34,33,25,24,16,8,-1},
        {2,3,4,12,20,28,27,35,34,33,32,24,16,17,9,10,-1},
        {0,1,2,3,11,12,20,28,36,35,34,26,25,17,16,8,-1},
    },
    {
        {0,1,2,3,4,12,20,19,27,35,34,33,25,17,16,8,-1},
        {0,1,2,10,11,12,20,28,27,26,34,33,32,24,16,8,-1},
        {1,2,3,11,19,20,28,36,35,34,33,32,24,16,17,9,-1},
        {2,3,4,12,20,28,36,35,34,26,25,24,16,8,9,10,-1},
    },
    {
        {1,2,3,4,12,20,19,27,35,34,33,25,24,16,8,9,-1},
        {0,1,2,10,11,12,20,28,27,35,34,33,25,24,16,8,-1},
        {1,2,3,11,12,20,28,27,35,34,33,32,24,16,17,9,-1},
        {1,2,3,11,12,20,28,36,35,34,26,25,24,16,8,9,-1},
    },
    {
        {0,1,2,3,11,12,20,28,27,35,34,33,25,17,16,8,-1},
        {1,2,3,11,12,20,28,27,26,34,33,32,24,16,8,9,-1},
        {1,2,3,11,19,20,28,36,35,34,33,25,24,16,8,9,-1},
        {2,3,4,12,20,28,27,35,34,33,25,24,16,8,9,10,-1},
    },
    {
        {1,2,3,11,12,20,28,27,35,34,33,25,24,16,8,9,-1},
        {1,2,3,11,12,20,28,27,35,34,33,25,24,16,8,9,-1},
        {1,2,3,11,12,20,28,27,35,34,33,25,24,16,8,9,-1},
        {1,2,3,11,12,20,28,27,35,34,33,25,24,16,8,9,-1},
    },
    {
        {1,2,3,4,12,20,19,27,35,34,33,32,24,16,17,9,-1},
        {0,1,2,10,11,12,20,28,36,35,34,26,25,24,16,8,-1},
        {1,2,3,4,12,20,19,27,35,34,33,32,24,16,17,9,-1},
        {0,1,2,10,11,12,20,28,36,35,34,26,25,24,16,8,-1},
    },
    {
        {0,1,2,3,11,19,20,28,36,35,34,33,25,17,16,8,-1},
        {2,3,4,12,20,28,27,26,34,33,32,24,16,8,9,10,-1},
        {0,1,2,3,11,19,20,28,36,35,34,33,25,17,16,8,-1},
        {2,3,4,12,20,28,27,26,34,33,32,24,16,8,9,10,-1},
    },
    {
        {0,1,2,10,18,19,20,28,36,35,34,33,32,24,16,8,-1},
        {2,3,4,12,20,28,36,35,34,33,32,24,16,17,18,10,-1},
        {0,1,2,3,4,12,20,28,36,35,34,26,18,17,16,8,-1},
        {0,1,2,3,4,12,20,19,18,26,34,33,32,24,16,8,-1},
    }
};

int piece;
int width, height;
char board[128][128];
int unused;

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
#if defined(ARGO)
    return true;
#else
    return timercmp(&now,&end,<);
#endif
}

inline bool is_board_x(int i, int j) {
    if (i<0 || j<0 || i>=height || j>=width)
        return true;
    return board[i][j];
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

void set_tile() {
    for (int bits = PIECE_MASKS[piece][placement.r]; bits; bits ^= (bits & -bits)) {
        int jj = __builtin_ctz(bits);
        int ii = jj/5;
        jj %= 5;
        board[placement.i+ii][placement.j+jj] = 1;
        --unused;
    }
}

void unset_tile() {
    for (int bits = PIECE_MASKS[piece][placement.r]; bits; bits ^= (bits & -bits)) {
        int jj = __builtin_ctz(bits);
        int ii = jj/5;
        jj %= 5;
        board[placement.i+ii][placement.j+jj] = 0;
        ++unused;
    }
}

int border_transitions() {
    bool start, last;
    int transitions = 0;

    {
        int jj = BORDER_TRACE[piece][placement.r][0];
        int ii = (jj>>3)-1;
        jj = (jj&7)-1;
        start = last = is_board_x(placement.i+ii, placement.j+jj);
    }

    for (int n = 1; BORDER_TRACE[piece][placement.r][n] >= 0; ++n) {
        int jj = BORDER_TRACE[piece][placement.r][n];
        int ii = (jj>>3)-1;
        jj = (jj&7)-1;

        bool ix = is_board_x(placement.i+ii, placement.j+jj);
        if (last != ix)
            ++transitions;
        last = ix;
    }

    if (start!=last)
        ++transitions;
    if (!start && transitions == 0)
        transitions = 100;
    return transitions;
}

short endgame_move_counts[25][4];
short endgame_moves[25][4][10000];
void make_endgame_moves() {
    memset(endgame_move_counts,0,sizeof(endgame_move_counts));
    for (piece = 0; piece < 25; ++piece) {
        set<int> masks;
        for (placement.r = 0; placement.r < 4; ++placement.r) {
            if (masks.find(PIECE_MASKS[piece][placement.r]) != masks.end())
                continue;
            masks.insert(PIECE_MASKS[piece][placement.r]);
            for (placement.i = 0; placement.i < height; ++placement.i)
                for (placement.j = 0; placement.j < width; ++placement.j)
                    if (fits())
                        endgame_moves[piece][placement.r][endgame_move_counts[piece][placement.r]++]
                            = placement.i*256+placement.j;
        }
    }
}

bool fits_somewhere() {
    for (short n = 0; n < endgame_move_counts[piece][placement.r]; ++n) {
        placement.j = endgame_moves[piece][placement.r][n];
        placement.i = placement.j/256;
        placement.j %= 256;
        if (fits())
            return true;
    }
    return false;
}

void score() {
    int score = 0;
    if (unused > ENDGAME_THRESHOLD)
        score = 2500000;
    else {
        set_tile();
        Placement saved_placement = placement;
        int saved_piece = piece;

        for (piece = 0; piece < 25; ++piece) {
            for (placement.r = 0; placement.r < 4; ++placement.r) {
                if (fits_somewhere()) {
                    score += 100000;
                    break;
                }
            }
        }

        placement = saved_placement;
        piece = saved_piece;
        unset_tile();
    }

    if (score+29999 <= best_score)
        return;

    switch (border_transitions()) {
    case 0:
        score += 20000;
        break;
    case 2:
        score += 10000;
        break;
    }

    if (score+9999 <= best_score)
        return;
    
    for (long long bits = BORDERS_MASKS[piece][placement.r]; bits; bits ^= (bits & -bits)) {
        int jj = __builtin_ctzll(bits);
        int ii = (jj/7)-1;
        jj = (jj%7)-1;
        if (is_board_x(placement.i+ii, placement.j+jj))
            score += 100;
    }

    if (score+99 <= best_score)
        return;

    score += (int)hypot(abs(width/2-placement.j), abs(height/2-placement.i));

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

    if (unused <= ENDGAME_THRESHOLD)
        make_endgame_moves();

    piece = _piece;

    set<int> masks;
    for (placement.r = 0; placement.r < 4; ++placement.r) {
        if (masks.find(PIECE_MASKS[piece][placement.r]) != masks.end())
            continue;
        masks.insert(PIECE_MASKS[piece][placement.r]);
        for (placement.i = 0; placement.i < height; ++placement.i) {
            for (placement.j = 0; placement.j < width; ++placement.j) {
                if (!fits())
                    continue;
                score();
            }
        }
    }

    if (!best_move.empty()) {
        placement.j = best_move[0];
        placement.i = best_move[1];
        placement.r = best_move[2];
        set_tile();
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
            unused = width*height;
            initialized = true;
        }
        return MM21::placeTile(MM21::find_piece(piece));
    }
};
