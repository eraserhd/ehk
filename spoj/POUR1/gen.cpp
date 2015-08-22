#include <cstdlib>
#include <cstring>
#include <iostream>
using namespace std;

int main() {
    srand(time(NULL));
    cout << 100 << endl;
    for (int i = 0; i < 100; ++i) {
        int A = rand()%40000 + 1, B = rand()%40000 + 1, C = rand()%40000 + 1;
        cout << A << endl;
        cout << B << endl;
        cout << C << endl;
    }
    return 0;
}

