#include <stdio.h>
#include <limits.h>
#include <float.h>
#include <stdint.h>
#include <stdlib.h>

void integral(void)
{
	printf("%10s |%6s |%21s |%21s\n",  "type",   "bits", "min", "max");
	printf("%10s |%6d |%21d |%21d\n",  "char",   CHAR_BIT, CHAR_MIN, CHAR_MAX);
	printf("%10s |%6d |%21d |%21d\n",  "int" ,   8*sizeof(int), INT_MIN, INT_MAX);
	printf("%10s |%6d |%21d |%21d\n",  "short",  8*sizeof(short), SHRT_MIN, SHRT_MAX);
	printf("%10s |%6d |%21li |%21li\n",  "long",   8*sizeof(long),LONG_MIN, LONG_MAX);
	printf("%10s |%6d |%21lli |%21lli\n", "long long", 8*sizeof(long long), LLONG_MIN, LLONG_MAX);
}

void fractional(void)
{
	printf("%12s | %5s |%4s |%16s |%16s |%16s\n", "type", "bytes", "dig", "epsilon", "min", "max");
	printf("%12s | %5d |%4d |%16e |%16e |%16e\n", "float",
		   sizeof(float), FLT_DIG, FLT_EPSILON, FLT_MIN, FLT_MAX);
	printf("%12s | %5d |%4d |%16e |%16e |%16e\n", "double",
		   sizeof(double), DBL_DIG, DBL_EPSILON, DBL_MIN, DBL_MAX);
	printf("%12s | %5d |%4d |%16le |%16le |%16le\n", "long double",
		   sizeof(long double), LDBL_DIG, LDBL_EPSILON, LDBL_MIN, LDBL_MAX);
}

typedef union
{
	uint32_t i32;
	uint16_t i16[2];
} endian_t;

void endian(void)
{
	endian_t e;
	e.i32 = 0xdeadbeaf;
	if(e.i16[0] == 0xdead)
		printf("Big-endian Machine\n");
	else if(e.i16[0] == 0xbeaf)
		printf("Little-edian Machine\n");
	else
		printf("confused with %x %x\n", e.i16[0], e.i16[1]);
}

int main(int argc, char *argv[])
{
	int ch;
	while((ch = getopt(argc, argv, "ife")) != -1) {
		switch(ch) {
		case 'i':
			integral();
			break;
		case 'f':
			fractional();
			break;
		case 'e':
			endian();
			break;
		default:
			exit(1);
		}
	}
}
