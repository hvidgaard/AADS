#include <stdlib.h>
#include <stdio.h>

int Compare(const void* a, const void* b){
	if( *(int*)a > *(int*)b) return(1);
	if( *(int*)a < *(int*)b) return(-1);
	return(0);
}
void DestroyKey(void* a){
	free((int*)a);
}
void DestroyInfo(void* a){
	free((int*)a);
}
void PrintKey(void* a){
	printf("%i",*(int*)a);
}
void PrintInfo(void* a){
	printf("%i",*(int*)a);	
}