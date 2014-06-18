#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <xmmintrin.h>
#define PREFETCH(m,type,r,c) _mm_prefetch(((type*)(m)->data)[(m)->order((m)->rows,(m)->cols,RMOD(m,r),CMOD(m,c))], _MM_HINT_T0)

#include "mutils.h" // implies include order.h

#define N 1024


matrix_t* mm(matrix_t* A, matrix_t* B){
	matrix_t* C = malloc_matrix(A->rows, B->cols, sizeof(THE_TYPE));
	
	int i,j,k;
	for (i=0 ; i<C->rows ; i++){
		for ( j=0 ; j<C->cols ; j++ ){
			SET(C,int,i,j,0);
			for ( k=0 ; k<A->cols ; k++ ){
				THE_TYPE res = GET(C,THE_TYPE,i,j) + ( GET(A,THE_TYPE,i,k) * GET(B,THE_TYPE,k,j) );
				SET(C,THE_TYPE,i,j,res);
			}
		}
	}
	
	return C;
}

matrix_t* mm_pf(matrix_t* A, matrix_t* B){
	matrix_t* C = malloc_matrix(A->rows, B->cols, sizeof(THE_TYPE));
	
	int i,j,k;
	for (i=0 ; i<C->rows ; i++){
		for ( j=0 ; j<C->cols ; j++ ){
			SET(C,int,i,j,0);
			for ( k=0 ; k<A->cols ; k++ ){
				THE_TYPE res = GET(C,THE_TYPE,i,j) + ( GET(A,THE_TYPE,i,k) * GET(B,THE_TYPE,k,j) );
				SET(C,THE_TYPE,i,j,res);
				PREFETCH(B,THE_TYPE,k+15,j);
			}
		}
	}
	
	return C;
}

float row_row(int n){
	clock_t begin, end;
	matrix_t* A = malloc_matrix(n, n, sizeof( THE_TYPE ));
	matrix_t* B = malloc_matrix(n, n, sizeof( THE_TYPE ));
	
	matrix_init(A);
	matrix_init(B);
	
	begin = clock();
	matrix_t* C = mm(A,B);
	end = clock();
	
	free(A);
	free(B);
	free(C);
	
	return (float)(end-begin)/CLOCKS_PER_SEC;
}

float row_col(int n){
	clock_t begin, end;
	matrix_t* A = malloc_matrix(n, n, sizeof( THE_TYPE ));
	matrix_t* B = malloc_matrix(n, n, sizeof( THE_TYPE ));
	
	matrix_init(A);
	matrix_init(B);
	
	begin = clock();
	matrix_change_order(B, *col_major_order);
	matrix_t* C = mm(A,B);
	matrix_change_order(B, *row_major_order);
	end = clock();
	
	free(A);
	free(B);
	free(C);
	
	return (float)(end-begin)/CLOCKS_PER_SEC;
}

float col_row(int n){
	clock_t begin, end;
	matrix_t* A = malloc_matrix(n, n, sizeof( THE_TYPE ));
	matrix_t* B = malloc_matrix(n, n, sizeof( THE_TYPE ));
	
	matrix_init(A);
	matrix_init(B);
	
	begin = clock();
	matrix_change_order(A, *col_major_order);
	matrix_t* C = mm(A,B);
	matrix_change_order(A, *row_major_order);
	end = clock();
	
	free(A);
	free(B);
	free(C);
	
	return (float)(end-begin)/CLOCKS_PER_SEC;
}

void MM(){
	int n = N;
	float rr = row_row(n);
	float rc = row_col(n);
	float cr = col_row(n);
	printf("%d\t%f\t%f\t%f\n",n,rr,rc,cr);
}

void ZZ(){
	int n = 16;
	matrix_t* A = malloc_matrix(n, n, sizeof( THE_TYPE ));
	matrix_t* B = malloc_matrix(n, n, sizeof( THE_TYPE ));
	matrix_init(A);
	matrix_init(B);
	//matrix_change_order(A, *row_major_order);
	
	
	matrix_t* C = mm(A,B);
	matrix_print( A );
	printf("\n");
	matrix_print( B );
	printf("\n");
	matrix_print( C );
	
	free( A );
	free( B );
	free( C );
}

void prefetch(){
	int n = 128;
	clock_t begin, end;
	float time1, time2;
	matrix_t* A = malloc_matrix(n, n, sizeof( THE_TYPE ));
	matrix_t* B = malloc_matrix(n, n, sizeof( THE_TYPE ));
	
	matrix_init(A);
	matrix_init(B);
	
	begin = clock();
	matrix_t* C = mm_pf(A,B);
	end = clock();
	free(C);
	time1 = (float)(end-begin)/CLOCKS_PER_SEC;
	
	
	begin = clock();
	C = mm(A,B);
	end = clock();
	time2 = (float)(end-begin)/CLOCKS_PER_SEC;
	
	printf("%d\t%f\t%f\n",n,time1,time2);
	
	free( A );
	free( B );
	free( C );
}

int main(){
	printf("Hello\n");
	prefetch();
}