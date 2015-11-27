#include "vm.h"
#define MAXMEM 500000

struct timeval *Tps = NULL;
struct timeval *Tpf = NULL;
void *Tzp;
int fd=0; 
int R[32]; 
int M[MAXMEM]; 
int H[MAXMEM]; 
unsigned int C[MAXMEM]; 

char tmp[MAX_FNAME]; 
FILE *fds[MAX_FILES]; 



int getFD(int adr, int heap) {
	int cnt = 0; int *P = M;
	FILE *cfd;
	if (heap) { P=H;}
	while (P[adr]!=0) { 
		tmp[cnt++]=(char)P[adr];
		adr++;
	}
	fd++;
	cfd = fopen(tmp,"r");
	fds[fd] = cfd;
	return fd;
}

int alloc(int size) {
	static int pos=1;
	pos=pos+size;
	return (pos - size)*4;
}

void check(int op, int a, int b, int c,int pc) {
	if (( R[a] < 0) || ( R[a] >= c )) { 
		fprintf(stderr,"boundary exception pc=%d (op=%d, a=%d, b=%d, c=%d)\n",pc,op,a,b,c); 
		getchar();
		exit(-1);
	}
}

void load(char *filename) {
	FILE *fd; unsigned int i = 0;
	fd = fopen(filename, "r");
	if (fd == NULL) { 
		fprintf(stderr,"error opening file\n"); 
		getchar();
		exit(-1);
	}
	while (!feof(fd)) {
		fscanf(fd,"%d", &C[i++]);
	}
	//fprintf(stderr,"loaded %d instructions\n", i-1);
	fclose(fd);
}


void copyParToMem(char arg[], int pos, int length) {
	int c; int i = 0;
	c = (pos/4);
	for (i=0;i<length;i++) {
		H[c+i] = (int)arg[i];
	}
	H[c+length] = 0;

}


int main(int argc, char **argv) {
	int op = 0;
	int a = 0;
	int b = 0;
	int c = 0;
	int length = 0;
	double counter = 0;
	unsigned int pc = 0; 
	unsigned int next = 0;
	Tps = (struct timeval *) malloc(sizeof(struct timeval));
	Tpf = (struct timeval *) malloc(sizeof(struct timeval));
	Tzp = 0;


	if ((argc < 2) || (argc > 3)) {
		fprintf(stderr,"usage: ./vm <vminput> <arg>\n");
		exit(-1);
	}
	
	if (argc == 2) {
		length = 0;
		c = 0;
	} else {
		length = strlen(argv[2])+1;
		c = alloc(length);
		copyParToMem(argv[2],c,length-1);

	}
	R[1] = argc - 1; // argc
	R[2] = 0; // isOnStack
	R[3] = length; // arglength
	R[4] = c; // adr

	load(argv[1]);
	gettimeofday (Tps, Tzp);
	while (1) {
		counter = counter + 1;
		next = pc + 1;
		op = C[pc] / 0x4000000 % 0x40;
		a = C[pc] / 0x200000 % 0x20;
		b = C[pc] / 0x10000 % 0x20;
		c = C[pc] % 0x10000;
		if (op < ADDI) { 
			c = R[c % 0x20]; 
		} else if (c >= 0x8000) { 
			c = c - 0x10000; 
		}
		R[0] = 0;
		
		switch (op) {
			case ADD: case ADDI: 
				R[a] = R[b] + c; 
				break;
			case MUL: case MULI: 
				R[a] = R[b] * c; 
				break;
			case SUBI: case SUB: case CMP: case CMPI: 
				R[a] = R[b] - c; 
				break;
			case BSR: 
				next = pc + c;
				R[31] = (pc + 1) * 4;
				break;
			case PSH: 
				R[b] = R[b] - c;
				M[R[b]/4] = R[a];
				break;
			case POP: 
				R[a] = M[R[b] / 4]; 
				R[b]=R[b]+c; 
				break;
			case STW: 
				M[(R[b] + c) / 4] = R[a];	
				break;
			case LDW: 
				R[a] = M[(R[b] + c) / 4]; 
				break;
			case DIV: case DIVI: 
				R[a] = R[b] / c; 
				break;
			case MOD: case MODI: 
				R[a] = R[b] % c; 
				break;
			case OR:  case ORI:  
				R[a] = R[b] | c; 
				break;
			case AND: case ANDI: 
				R[a] = R[b] & c; 
				break;
			case BIC: case BICI: 
				R[a] = R[b] & (~c); 
				break;
			case XOR: case XORI: 
				R[a] = R[b] ^ c; 
				break;
			case LSH: case LSHI: 
				R[a]=R[b] << c; 
				break;
			case BEQ: 
				if (R[a] == 0) { 
					next = pc + c; 
				} 
				break;
			case BNE: 
				if (R[a] != 0) { 
					next = pc + c; 
				} 
				break;
			case BLT: 
				if (R[a] < 0) { 
					next = pc + c; 
				} 
				break;
			case BGE: 
				if (R[a] >= 0) { 
					next = pc + c; 
				} 
				break;
			case BLE: 
				if (R[a] <= 0) { 
					next = pc + c; 
				} 
				break;
			case BGT: 
				if (R[a] > 0) { 
					next = pc + c; 
				} 
				break;
			case ALL: 
				R[a] = alloc(c/4); 
				break;
			case HSTW: 
				H[(R[b] + c) / 4] = R[a]; 
				break;
			case HLDW: 
				R[a] = H[(R[b] + c) / 4]; 
				break;
			case CHK: case CHKI: 
				check(op,a,b,c,pc); 
				break;
			case SFOPEN: 
				R[a] = getFD((R[b] + c) / 4,0); 
				break;
			case HFOPEN: 
				R[a] = getFD((R[b] + c) / 4,1); 
				break;
			case FCLOSE: 
				fclose(fds[R[c]]); 
				R[a] = 1;
				break;
			case FGETC: 
				R[a] = (int)fgetc(fds[R[c]]); 
				break;
			case FEOF: 
				if (feof(fds[R[c]]) != 0) { 
					R[a]=1;
				} else {
					R[a]=0;
				} 
				break;
			case WRI: 
				printf("%d",R[c]); 
				break;
			case WRC: 
				printf("%c",(char)R[c]); 
				break;
			case RET: 
				next = R[c % 32] / 4; 
				if (next == 6) {
					goto END;
				} 
				break;
			case HLT: 
				fprintf(stderr,"implicit exit with code %d\n", R[c]); 
				getchar();
				exit(-1);
				break;
			case ORD: 
				R[a] = (int)R[c]; 
				break;
		}
		pc = next;
	}
END:
	gettimeofday (Tpf, Tzp);
	fprintf(stderr,"Total VM execution time (usec): %ld\n",
              (Tpf->tv_sec-Tps->tv_sec)*1000000
             + Tpf->tv_usec-Tps->tv_usec);
	fprintf(stderr, "Executed instructions per second: %e\n", counter/(double)(Tpf->tv_sec-Tps->tv_sec));
	return 0;
}

