#include <iostream>
using namespace std;
int fi_re(int n)
{
	if(n<2) return 1;
	else return fi_re(n-1)+fi_re(n-2);
}
int fi_it(int n){
	/*every result are only related to the previous two result say, pre_one and pre_two. When we want compute the next result what we need to do is re-assign pre_one and pre_two, pre_two equals to the current result and pre_one equals to the pri_two of the previous result*/
int result=1;
int pre_one=1,pre_two=1;
for(;n>=2;n--){
	pre_two=pre_one;
	pre_one=result;
	result=pre_one+pre_two;	
	}
return result;
}
int main(int argc, char *argv[]){ 
	int times;
	cin>>times;
	cout<<"the result caculate bu recursion is :"<<endl;
	for(int i=0;i<times;i++){
		int a=fi_re(i);
		cout<<a<<" ";
	}
	cout<<"the result caculate by iteartion is :"<<endl;
	for(int i=0;i<times;i++){
		int b=fi_it(i);
		cout<<b<<" ";
}
return 0;
}


