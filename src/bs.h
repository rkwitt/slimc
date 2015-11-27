#ifndef BS_H
#define BS_H

#include <stdio.h>
#include <stdlib.h>
#define procedure
#define prototype 
#define variable
#define record
#define elsif else if
#define bool int
#define true 1
#define false 0
#define FILE FILE*
#define allocMem(a) a = calloc(1,sizeof(*a))
#define writeInt(a) fprintf(stdout,"%d", a);
#define writeChar(a) fprintf(stdout, "%c", a);
#define main(num,list) main(int argc, char *argv[])
#define fopen(name) fopen(name, "r+")
#define ord(a) a
#define arg argv[1]
#endif
