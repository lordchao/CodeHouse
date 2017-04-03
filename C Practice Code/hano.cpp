#include <iostream>
using namespace std;

void moveone(int disks, string init, string dest){
cout<<"move disk no."<<disks<<" from "<<init<<" to "<<dest<<endl;
}

void move(int disks, string init, string temp, string dest){
if(disks==1) moveone(1,init,dest);
	else{
	move(disks-1,init,dest,temp);

	}
}
int main(){
	move(3,"a","b","c");
	return 0;
}
