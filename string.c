#include <stdio.h>
#include <string.h>

void reverse(char s[])
{
	int c, i, j;
	for(i=0,j=strlen(s)-1; i<j; i++,j--) {
		c = s[i];
		s[i] = s[j];
		s[j] = c;
	}
}

char* itoa(int n, char s[], int fw)
{
	int i, neg;
	neg = n < 0 ;
	i=0;
	do {
		s[i++] = abs(n%10) + '0';
	} while ((n/=10) != 0);
	if(neg)
		s[i++] = '-';
	while(i<fw)
		s[i++] = ' ';
	s[i] = '\0';
	reverse(s);
	return s;
}

char* itob(int n, char s[], int b)
{
	if(b>36 || b<2) return NULL;
	char tbl[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
	int i, neg;
	neg = n < 0 ;
	i = 0;
	do {
		s[i++] = tbl[abs(n%b)];
	} while ((n/=b) != 0);
	if(neg)
		s[i++] = '-';
	s[i] = '\0';
	reverse(s);
	return s;	
}

int main()
{
	int n,w;
	char s[128];
	if( scanf("%d, %d", &n, &w) == 2) {
		itob(n, s, w);
		printf("%d:%s\n", strlen(s), s);
	}
}
