#include <stdio.h>
#include <stdlib.h>

#include <time.h>
#include <xmmintrin.h>

#define N (1024*8)

#if (IN_ORDER == 0)
	#define COORD2IDX(r,c) ( r*N + c )
	#define PREFETCH( ptr , hint )
	#ifndef I
		#define I -1
	#endif
#endif

#if (IN_ORDER != 0)
	#define COORD2IDX(r,c) ( c*N + r )
	#define PREFETCH( ptr , hint ) _mm_prefetch( ptr , hint )
	#ifndef I
		#error I is not defined
	#endif
#endif




float a(){
	time_t begin, end;
	int i,r,c;
	
	float* array = (float*)malloc((N*N)*sizeof(float) );
	float* sums  = (float*)malloc(N*sizeof(float) );

	for (i=0 ; i<(N*N) ; i++){
		array[ i ] = ((i%(N+1))==0)?1.0:0.0;
	}
	
	for (c=0 ; c<N ; c++){
		sums[c]=0;
	}
	
	begin = clock();
	for (r=0 ; r<N ; r++){
		for (c=0 ; c<N ; c++){
			sums[c] += array[COORD2IDX(r,c)];
			PREFETCH(array+COORD2IDX(r,c+I), _MM_HINT_T0);
		}
	}
	end = clock();
	
	free(sums);
	free(array);
	
	return (float)(end-begin)/CLOCKS_PER_SEC;
}


int main(){
	printf("%d\t%f\n",I,a());
}

