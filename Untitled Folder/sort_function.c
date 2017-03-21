#include <stdio.h>

//basic implementation of sort funtiocn based on C
//created by Chao Liu on 15 Feburary 2017

void selection_sort(int A[],int n);
void bubble_sort(int A[],int n);
void PrintResult(int A[]);
void insertion_sort(int A[],int n);
int lomuto(int A[],int l,int n);
void swap(int x,int y);
void quick_sort(int A[], int start,int end);

int i,j,temp;
void main()
{
	int A[5];
	printf("please input 5 numbers:\n");
	for(int i=0;i<5;i++)
		scanf("%d",&A[i]);//recieve five numbers

	selection_sort(A,5);
	printf("the sorted array by selection function are:\n");
	for(int i=0;i<5;i++)
		printf("%d\n",A[i]);

	bubble_sort(A,5);
	printf("the sorted by bubble sort array are:\n");
	for(int i=0;i<5;i++)
		printf("%d\n",A[i]);

	insertion_sort(A,5);
	PrintResult(A);

	quick_sort(A,0,5);
	PrintResult(A);
}

void selection_sort(int A[],int n)
{
for(i=0;i<n-1;i++)
	{
		int min=i;
		for(j=i+1;j<n;j++){
		if(A[j]<A[min])
			min=j;	
		}
		if(min!=i){//if current value is not the minmum value then change A[i] and min[i]
			temp=A[i];
			A[i]=A[min];
			A[min]=temp;
		}
	}
}

void bubble_sort(int A[],int n)
{
	for(i=0;i<n;i++){
		for (j = 0; j < n-i; j++)
		{
			if(A[j]>A[j+1]){
			temp=A[j];
			A[j]=A[j+1];
			A[j+1]=temp;
			}
		}
	}
}

void PrintResult(int A[])
{
	printf("the sorted array are:\n");
		for(int i=0;i<5;i++)
			printf("%d\n",A[i]);
}

void insertion_sort(int A[],int n)
{
	int v;
	for(i=1;i<n-1;i++){
		v=A[i];
		j=i-1;
		while ((A[j]>v)&&(j>=0)) {//move value
			A[j+1]=A[j];
			j=j-1;}
		A[j+1]=v;//insert value
	}
}	

void swap(int x,int y){
	temp=x;
	x=y;
	y=temp;
}

int lomuto(int A[],int l,int n)
{
	int s,p=A[l];s=l;
	for (int i = l+1; i < n; i++)
	{
		if(A[i]<p) 
			s+=1;swap(A[s],A[i]);//exchange 
	}
swap(A[l],A[s]);
return s;
}

/*
int quick_select(int A[],int k){
int s,l=0;
s=lomuto(A,5);
if (s=l+k-1) return A[s];
else if (s>l+k-1) quick_select(A[l,s-1],k);
else quick_select(A[s+1,5],l+k-1-s);
}
*/
void quick_sort(int A[], int start,int end)
{
	int s;
	if(start<end){
		s=lomuto(A,start,end);
		quick_sort(A,start,s-1);
		quick_sort(A,s+1,end);
	}
}