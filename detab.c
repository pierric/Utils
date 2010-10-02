#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define LINESIZE 512

char prog[32];
int tab_size = 4;

char* detab(char *line);
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
		printf(detab(line));
	return 0;
}

char* detab(char *line)
{
	int cnt = 0;
	if(NULL==line) return NULL;
	char *ps = line;
	while('\t' == *ps++) ++cnt;
	int i = strlen(line) - cnt;
	ps = line + i + cnt;
	char *pd = ps + (tab_size-1) * cnt;
	while(i-->=0)
		*pd-- = *ps--;
	while(pd>=line)
		*pd-- = ' ';
	return line;
}

void printUsage()
{
	printf("usage: %s [-t tabsize] [-h]\n"
		   "  -t tabsize: tabsize should be either 4 or 8\n"
		   "  -h: display this usage\n", prog);
}
