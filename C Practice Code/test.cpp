#include <iostream>
using namespace std;
 
int func(int n)
{
    if (n < 1)
        return 1;
    else
        return n + func(n - 1);
    return 0;
}
int main()
{
    cout << func(5) << endl;
    return 0;
}
