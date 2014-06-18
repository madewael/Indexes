#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <xmmintrin.h>

#define N 1024
#define THE_TYPE long long


#define GET(m,type,r,c) ((type*)(m)->data)[(m)->order((m)->rows,(m)->cols,r,c)]

#define RMOD(m,r) ( r%((m)->rows) )
#define CMOD(m,c) ( c%((m)->cols) )

#define SET(m,type,r,c,v) ((type*)(m)->data)[(m)->order((m)->rows,(m)->cols,r,c)] = (v) 


int row_major_order(int rs, int cs, int r, int c){
	return (r*cs) + c;
}

int col_major_order(int rs, int cs, int r, int c){
	return r + (rs*c);
}

int Z_order(int rs, int cs, int r, int c){
	int res = 0;
	while( rs>=1 ){
		res *= 2;
		res += (r%2);
		
		res *= 2;
		res += (r%2);
		r /= 2;
		c /= 2;
		rs /= 2;
	}
	return res;
}

typedef int (*order_f)(int, int,int, int);

typedef struct {
	order_f order;
	int rows;
	int cols;
	size_t element_size;
	void* data[];
} matrix_t;

matrix_t* malloc_matrix(int rows, int cols , size_t element_size){
	matrix_t* m = (matrix_t*)malloc( sizeof(matrix_t) +  element_size*rows*cols );
	m->order = *row_major_order;
	m->rows = rows;
	m->cols = cols;
	m->element_size = element_size;
	return m;
}

void matrix_change_order(matrix_t* M, order_f new_order){
	matrix_t* T = malloc_matrix( M->rows, M->cols , M->element_size );
	memcpy(T->data, M->data, (M->rows * M->cols * M->element_size) );
	T->order = M->order;
	M->order = new_order;
	
	int r,c;
	for (r=0 ; r<M->rows ; r++){
		for ( c=0 ; c<M->cols ; c++ ){
			int res = GET(T,THE_TYPE,r,c);
			SET(M,THE_TYPE,r,c,res);
		}
	}
	
	free(T);
}