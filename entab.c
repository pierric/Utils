#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define LINESIZE 512

char prog[32];
int tab_size = 4;

char* entab(char *line);
void printUsage();

int main (int argc, char *argv[])
{
	char line[LINESIZE];
	int ch;
	strncpy(prog, argv[0], 32);
	while((ch=getopt(argc, argv, "t:h")) != -1) {
		switch(ch) {
		case 't' :
			tab_size = atoi(optarg);
			if(tab_size!=4 && tab_size!=8) {
				printUsage();
				exit(1);
			}
			break;
		case 'h' :
			printUsage();
			exit(0);
		default  :
			exit(1);
		}
	}
	while(fgets(line, LINESIZE, stdin))
		printf(entab(line));
	return 0;
}

char* entab(char *ps)
{
	int cnt  = 0;
	char *pd = ps;
	if( NULL == ps ) return NULL;
	
	while(' ' == *ps) {
		if(++cnt == tab_size) {
			cnt = 0;
			*pd++ = '\t';
		}
		++ps;
	}
	while(cnt--)
		*pd++ = ' ';
	strcpy(pd, ps);
}

void printUsage()
{
	printf("usage: %s [-t tabsize] [-h]\n"
		   "  -t tabsize: tabsize should be either 4 or 8\n"
		   "  -h: display this usage\n", prog);
}
