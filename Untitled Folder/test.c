#include <stdio.h>
#include <string.h>
int main(){
char dog[]="wang\0miao";
int a=sizeof(dog);
int b=strlen(dog);
printf("%d,%d/n,a,b") ;
return 0;
}
