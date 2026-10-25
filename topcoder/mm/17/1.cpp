#include <vector>
#include <string>
#include <queue>
#include <algorithm>
#include <iostream>
using namespace std;

#define BEGIN {
#define END }
namespace MM17 BEGIN

vector<string> board;
int disjoint_set[100*100];
int set_sizes[100*100];

int disjoint_find(int n) {
    queue<int> q;
    while (disjoint_set[n]%65536 != 65535) {
        q.push(n);
        n = disjoint_set[n]%65536;
    }
    while (!q.empty()) {
        disjoint_set[q.front()] = n;
        q.pop();
    }
    return n;
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
            if (i > 0 && board[i-1][j] == col)
                disjoint_merge(i*100+j,(i-1)*100+j);
            if (j > 0 && board[i][j-1] == col)
                disjoint_merge(i*100+j,i*100+(j-1));
        }
    }

    int best_size = 0;
    int best_n = -1;
    memset(set_sizes, 0, sizeof(set_sizes));
    for (int i = 0; i < board.size(); ++i) {
        for (int j = 0; j < board[0].size(); ++j) {
            int n = disjoint_find(i*100+j);
            if (++set_sizes[n] > best_size) {
                best_size = set_sizes[n];
                best_n = n;
            }
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
    //cerr << "(" << result[0] << "," << result[1] << ")" <<endl;
    return result;
}

END;

struct ContinuousSameGame {
    vector<int> move(const vector<string>& board) {
        MM17::board = board;
        return MM17::move();
    }
};
