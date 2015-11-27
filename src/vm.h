#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>

#define MAX_FILES	100
#define MAX_FNAME	100

#define ADD		0
#define SUB		1
#define MUL		2
#define DIV		3
#define MOD		4
#define CMP		5
#define CHK		14
#define ADDI	16
#define SUBI	17 
#define MULI	18
#define DIVI	19
#define MODI	20
#define CMPI	21
#define CHKI	30
#define OR		8
#define AND		9 
#define BIC		10  
#define XOR		11  
#define ORI		24  
#define ANDI	25  
#define BICI	26  
#define XORI	27  
#define LSH		12  
#define ASH1	13  
#define LSHI	28  
#define ASHI	29  
#define LDW		32 
#define LDB		33
#define POP		34 
#define STW		36 
#define STB		37 
#define PSH		38 
#define BEQ		40 
#define BNE		41 
#define BLT		42 
#define BGE		43 
#define BLE		44 
#define BGT		45 
#define BSR		46 
#define JSR		48 
#define RET		49 
#define ALL		50 
#define HLDW	51 
#define HSTW	52 
#define ORD		53 
#define ALL		50 
#define HLDW	51 
#define HSTW	52 
#define WRC		55 
#define WRI		56 
#define HFOPEN	57 
#define SFOPEN	58 
#define FCLOSE	59 
#define FEOF	60 
#define FGETC	61 
#define HLT		62 

