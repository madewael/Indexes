#include "order.h"

void matrix_print(matrix_t* P){
	int r,c;
	for (r=0 ; r<P->rows ; r++){
		for ( c=0 ; c<P->cols ; c++ ){
			THE_TYPE res = GET(P,THE_TYPE,r,c);
			printf("%lld ", res);
		}
		printf("\n");
	}
}


int matrix_check(matrix_t* P){
	int r,c;
	for (r=0 ; r<P->rows ; r++){
		for ( c=0 ; c<P->cols ; c++ ){
			THE_TYPE res = GET(P,THE_TYPE,r,c);
			THE_TYPE expected = (r==c)?1:0;
			if ( res != expected ) return 0;
		}
	}
	return 1;
}

void matrix_init(matrix_t* P){
	int r,c;
	for (r=0 ; r<P->rows ; r++){
		for ( c=0 ; c<P->cols ; c++ ){
			THE_TYPE res= (r==c)?1:0;
			SET(P,THE_TYPE,r,c,res);
		}
	}
}