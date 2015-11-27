#include "bs.h"

record 
struct instrStruct {
	variable int instr;
	variable struct instrStruct *next;
};


/* holds keywords */
record 
struct keyWordStruct {
	variable char keyWord[20]; /* key word name */
	variable int keyWordSymbol; /* key word symbol */
	variable struct keyWordStruct *next; /* next key word */
};

/* symbol table objects */
record 
struct object  {
	variable int val; /* stores pc in case of procedures, a real value in case of const, etc. */
	variable int lev; /* level object is in, 0 global, >0 local to procedure */
	variable int cl; /* class of object */
	variable int typeIndex; /* index of object type in type list */
	variable int retTypeIndex; /* index of return type in type list */
	variable bool hasPrototype; /* do we have a prototype (for procedures) */
	variable bool hasArguments; /* do we have arguments (for procedures) */
	variable char name[100]; /* name of object */
	variable struct object *dsc; /* points to upper level or argument list (for procedures */
	variable struct object *next; /* next object in object list */
};

/* type descriptions */
record 
struct typeDesc {
	variable int index; /* index of type in type list */
	variable int form; /* form of type */
	variable int size; /* size of type, int=4, etc. */
	variable int len; /* length of type, in case we have an array */
	variable int refIndex; /* index of reference type for this type (used for records) */
	variable struct object *fields; /* for record type */
	variable struct typeDesc *base; /* base of array type */
	variable struct typeDesc *next; /* next object in list */
};

/* description of expressions */
record
struct item {
	variable int mode; /* store item mode, which is essentially CLASS_XXX */
	variable int a; 
	variable int b;
	variable int c; /* holds operator for bool operations */
	variable int r; /* holds register where item is stored */
	variable int alr; /* array length register */
	variable int hs; /* register, to hold the boolean value, whether addy is on stack or heap */
	variable int fr; /* field address register */
	variable int idx;
	variable int indLev;
	variable int lev; /* level item is in - set by makeItem */
	variable int typeIndex; /* index of the type the items object has */
};

/* store position of func to fix (in forward decl.) */
record 
struct procFixStruct {
	variable int pos; /* position in code where to fix */
	variable struct object *proc; /* reference to the procedure object */
	variable struct procFixStruct *next; /* next element in fixList */
};

/* global variables */
variable int pc; /* program counter */
variable int nkw; /* number of keywords */
variable int numberVal; /* stores number values */
variable int currentLevel; /* stores current level */
variable int currentSymbol; /* stores current symbol */
variable char currentCharacter; /* stores current character */
variable int intTypeIndex; /* stores index of integer type */
variable int charTypeIndex; /* stores index of character type */
variable int boolTypeIndex; /* stores index of boolean type */
variable int voidTypeIndex; /* stores index of void type */
variable int strLitIndex; /* in slimC, a string literal has an own type */
variable int fileTypeIndex; /* same, for file type */
variable int lineNumber; /* stores line number */
variable bool terminate; /* file end ? Yes -> terminate = true */
variable int bigNum;
variable FILE inputFile; 

variable char idName[100]; /* stores identifier string, which might be a keyword */
variable char stringLiteralParameter[100]; /* stores hardcoded string arguments, such as foo("hello") */
variable int regs[32]; /* registers */
variable int rel[1000]; 

variable struct keyWordStruct *keyWordTable; /* keyword table */
variable struct typeDesc *typeList; /* list of available types */
variable struct procFixStruct *procFixList; /* list of positions, where we have to fix jump addy's */
variable struct object *topScope; /* points to topscope */
variable struct object *universe; /* the universe ;) */
variable struct object *guard; /* holds the guard, which is used for list termination checking most of the time */
variable struct instrStruct *firstInstr;
variable struct instrStruct *lastInstr;

/* general constants */
variable const int wordSize = 4; /* size of an addr, which is int */
variable const int identifierLength = 100; /* maximum length of identifier */
variable const int stringLiteralLength = 100; /* maximum length of string literal param - must be the same as idName length */

/* architecture specific constants */
variable const int lnk = 31; /* ret. addy */
variable const int sp = 30; /* stack pointer */
variable const int fp = 29; /* frame pointer */
variable const int rv = 28; /* return value register */
variable const int dummy = 32;

/* risc instruction set */
variable const int RISC_ADD = 0;
variable const int RISC_SUB = 1;
variable const int RISC_MUL = 2;
variable const int RISC_DIV = 3;
variable const int RISC_MOD = 4;
variable const int RISC_CMP = 5;
variable const int RISC_OR = 8;
variable const int RISC_AND = 9;
variable const int RISC_BIC = 10;
variable const int RISC_XOR = 11;
variable const int RISC_LSH = 12;
variable const int RISC_CHK = 14;
variable const int RISC_ADDI = 16;
variable const int RISC_SUBI = 17;
variable const int RISC_MULI = 18;
variable const int RISC_DIVI = 19;
variable const int RISC_MODI = 20;
variable const int RISC_CMPI = 21;
variable const int RISC_ORI = 24;
variable const int RISC_ANDI = 25;
variable const int RISC_BICI = 26;
variable const int RISC_XORI = 27;
variable const int RISC_LHSI = 28;
variable const int RISC_ASHI = 29;
variable const int RISC_CHKI = 30;
variable const int RISC_LDW = 32;
variable const int RISC_LDB = 33;
variable const int RISC_POP = 34;
variable const int RISC_STW = 36;
variable const int RISC_STB = 37;
variable const int RISC_PSH = 38;
variable const int RISC_BEQ = 40;
variable const int RISC_BNE = 41;
variable const int RISC_BLT = 42;
variable const int RISC_BGE = 43;
variable const int RISC_BLE = 44;
variable const int RISC_BGT = 45;
variable const int RISC_BSR = 46;
variable const int RISC_JSR = 48;
variable const int RISC_RET = 49; 

/* added RISC instructions */
variable const int RISC_ALL = 50; /* allocate memory on heap */
variable const int RISC_HLDW = 51; /* heap load word */
variable const int RISC_HSTW = 52; /* heap store word */
variable const int RISC_ORD = 53;
variable const int RISC_WRC = 55; /* write character */
variable const int RISC_WRI = 56; /* write integer */
variable const int RISC_HFOPEN = 57; /* heap open file */
variable const int RISC_SFOPEN = 58; /* stack open file */
variable const int RISC_FCLOSE = 59; /* close file */
variable const int RISC_FEOF = 60; /* check end of file */
variable const int RISC_FGETC = 61; /* get character from file */
variable const int RISC_HLT = 62; /* hlt instruction */

/* memory size of VM and ENTRYPOINT pos */
variable int RISC_MEMSIZE;
variable const int ENTRY = 5;

/* scanner symbols */
variable const int SYMBOL_END = -2;
variable const int SYMBOL_ADD = 97;
variable const int SYMBOL_SUB = 98;
variable const int SYMBOL_MUL = 99;
variable const int SYMBOL_DIV = 100;
variable const int SYMBOL_ASSIGN = 101;
variable const int SYMBOL_EQUALTEST = 102;
variable const int SYMBOL_LEQ = 103;
variable const int SYMBOL_GEQ = 104;
variable const int SYMBOL_NOT = 105;
variable const int SYMBOL_LESS = 106;
variable const int SYMBOL_GREATER = 107;
variable const int SYMBOL_MOD = 108;
variable const int SYMBOL_OR = 201;
variable const int SYMBOL_AND = 202;
variable const int SYMBOL_SEMICOLON = 203;
variable const int SYMBOL_COMMA = 204;
variable const int SYMBOL_ARROW = 205;
variable const int SYMBOL_LPARAN = 301;
variable const int SYMBOL_RPARAN = 302;
variable const int SYMBOL_RBRAK = 303;
variable const int SYMBOL_LBRAK = 304;
variable const int SYMBOL_CLBRAK = 305;
variable const int SYMBOL_CRBRAK = 306;
variable const int SYMBOL_DOUBLEQUOTE = 401;
variable const int SYMBOL_SINGLEQUOTE = 402;
variable const int SYMBOL_IDENTIFIER = 501;
variable const int SYMBOL_NUMBER = 502;
variable const int SYMBOL_STRINGLITERAL = 503;
variable const int SYMBOL_CHARACTERLITERAL = 504;
variable const int SYMBOL_WHILE = 601;
variable const int SYMBOL_IF = 602;
variable const int SYMBOL_ELSE = 603;
variable const int SYMBOL_ELSIF = 604;
variable const int SYMBOL_RETURN = 605;
variable const int SYMBOL_VOID = 606;
variable const int SYMBOL_INT = 607;
variable const int SYMBOL_CONST = 608;
variable const int SYMBOL_CHAR = 609;
variable const int SYMBOL_FD = 612;
variable const int SYMBOL_PROC = 613;
variable const int SYMBOL_RECORD = 614;
variable const int SYMBOL_VAR = 615;  
variable const int SYMBOL_STRUCT = 616;
variable const int SYMBOL_FPRINTF = 617;
variable const int SYMBOL_NULL = 618;
variable const int SYMBOL_ALLOC = 619;
variable const int SYMBOL_PROT = 620;
variable const int SYMBOL_BOOL = 621;
variable const int SYMBOL_EXIT = 622;
variable const int SYMBOL_INCLUDE = 624;
variable const int SYMBOL_HASHINCLUDE = 625;
variable const int SYMBOL_DOT = 626;
variable const int SYMBOL_UNKNOWN = 900;

/* code generation stuff */
variable const int CLASS_HEAD = 700; /* indicates head of a list */
variable const int CLASS_CONST = 701; /* constant */
variable const int CLASS_FLD = 702; /* field variable in record */
variable const int CLASS_TYP = 703; /* record */
variable const int CLASS_PROC = 704; /* procedure */
variable const int CLASS_VAR = 705; /* variable */
variable const int CLASS_REG = 706; /* indicates that item is in register */
variable const int CLASS_COND = 707; /* condition */
variable const int CLASS_SPROC = 708; /* special procedure, such as IO stuff */
variable const int CLASS_PAR = 709; /* parameter */
variable const int CLASS_IND =  710; /* indicates that we need indirect addressing */

/* available forms variables */
variable const int FORM_INT = 800; /* integer */
variable const int FORM_CHAR = 801; /* character */
variable const int FORM_ARRAY = 802; /* array */
variable const int FORM_RECORD = 803; /* record */
variable const int FORM_REF = 804; 
variable const int FORM_BOOL = 805; /* boolean */
variable const int FORM_VOID = 806; /* void, size is 0 */
variable const int FORM_STRLIT = 807; /* string literal (only used indirectly) */
variable const int FORM_FILE = 808; /* files */

/* parser function prototypes */
prototype void program(void);
prototype void openScope(void);
prototype void closeScope(void);
prototype int declaration(int);
prototype void selector(struct item *);
prototype void factor(struct item *);
prototype void term(struct item *);
prototype void simpleExpression(struct item *);
prototype void expression(struct item *);
prototype void statementSequence(struct object *,int);
prototype void statement(struct object *,int);
prototype void iterationStatement(struct object *,int);
prototype void conditionalBranchStatement(struct object *,int);
prototype void returnStatement(struct object *, int);
prototype void allocStatement(void);
prototype void nullRelation(int op, struct item *);
prototype void functionPrototype(void);
prototype void exitStatement(void);
prototype void includeStatement(void);

/* code generation function prototypes */
prototype void load(struct item *);
prototype void opOne(int op, struct item *);
prototype void opTwo(int op, struct item *, struct item *);
prototype int getReg(void);
prototype void put(int, int, int, int);
prototype int selectOp(int);
prototype int selectOpNegated(int);
prototype void fixLink(int);
prototype void enter(int);
prototype void ret(int);
prototype void enterSproc(int, int , char []);
prototype void IOCall(struct item *, struct item *);
prototype void emitParameter(struct item *, int, int);
prototype struct object * parameter(struct object *);
prototype void call(struct item *, int);
prototype bool isParameter(struct object *);
prototype void cJump(struct item *);
prototype void idx(struct item *, struct item *);
prototype void makeItem(struct item *, struct object *);
prototype void makeConstItem(struct item *, int, int);
prototype void relation(int op, struct item *x, struct item *);
prototype int fJump(int);
prototype void bJump(int);
prototype void store(struct item *, struct item *);
prototype void param(struct item *);
prototype void incLevel(int);
prototype void procFix(void);
prototype void fix(int, int);
prototype void writeString(char []);
prototype void emitHeader(int);
prototype void loadStringLiteralToHeap(struct item *);
prototype void mark(char []);

/* general purpose function prototypes */
prototype struct typeDesc *newType(void);
prototype struct object *find(void);
prototype struct object *findProc(void);
prototype struct object *newObj(int);
prototype struct object *newAnonymousObj(int);
prototype struct typeDesc *getType(int); 
prototype void setupKeyWordTable(void);
prototype void setupMain(void);

/* setup register array */
procedure 
void setupRegister(void) {
	variable int i; i=0;
	while (i < 28) {
		regs[i] = 0;
		i = i + 1;
	}
	regs[0] = 1;
}

/* get free register - caution: sp, fp, rv, 0 are reserved */
procedure 
int getReg(void) {
	variable bool cond;
	variable int r; variable int i;
	i = 0; r = 0; cond = true;
	while ((i < rv) && (cond == true)) {
		i = i + 1;
		if (regs[i] == 0) {
			regs[i] = 1;
			r = i;
			cond = false;
		}
	}
	if (r == 0) {
		mark("no free registers");
		exit(-1);
	}
	return i;

}

/* initialize global variables */
procedure
void globalInit(void) {
	variable int fac;
	fac = 4; pc = 0; nkw = 0; numberVal = 0; currentSymbol = 0;
	currentCharacter = '\0'; intTypeIndex = 0; charTypeIndex = 0;
	currentLevel = 0; lineNumber = 1; procFixList = NULL; terminate = false;
	bigNum = 16384 * fac; RISC_MEMSIZE = 32000;
	allocMem(firstInstr);
	firstInstr->instr = 0;
	firstInstr->next = NULL;
	lastInstr = firstInstr;
	setupRegister(); 
}

/* print out error messages */
procedure 
void mark(char msg[]) {
	writeString("Line: "); writeInt(lineNumber); writeString(": "); writeString(msg);
	writeChar('\n');
}

/* write string (char array) to console */
procedure 
void writeString(char msg[]) {
	variable int i;
	i=0;
	while (msg[i] != '\0') {
		writeChar(msg[i]);
		i=i+1;
	}
}

procedure 
int stringLength(char input[]) {
	variable int length;
	length = 0;
	while (input[length] != '\0') {	
			length = length + 1; 
	}
	return length;
}

/* copy strings */
procedure 
void stringCopy(char src[], char dst[]) {
	variable int srcLength;
	variable int pos;
	srcLength = 0; pos = 0;
	srcLength = stringLength(src);
	while (pos < srcLength) {
		dst[pos]=src[pos];
		pos=pos+1;
	}
	dst[pos] = '\0';
}

/* compare strings */
procedure 
int stringCompare(char src[], char dst[]) {
	variable int j; variable int length; variable int sourceLength; variable int dstLength; 
	j = 0; length = 0;
	sourceLength = stringLength(src);
	dstLength = stringLength(dst);
	if (sourceLength > dstLength) {
		length = sourceLength;
	} else 	{
		length = dstLength;
	}
	while  (j < length) {
		if (src[j] != dst[j]) {
			return -1;
		}
		j = j + 1;
	}
	return 0;	
}

/* manage insert action in linked list of keywords */
procedure 
void enterKeyWord(int keyWordSymbol, char keyWord[]) {
	variable struct keyWordStruct *n; variable struct keyWordStruct *tmp;
	allocMem(n);
	tmp = keyWordTable;
	n->keyWordSymbol = keyWordSymbol;
	stringCopy(keyWord,n->keyWord);
	while ( tmp->next != NULL) { 
		tmp = tmp->next; 
	}
	n->next = NULL;
	tmp->next = n;
	nkw = nkw + 1;
}

/* enter keyword in linked list of keywords */
procedure 
void setupKeyWordTable(void) {
	allocMem(keyWordTable);
	keyWordTable->next =  NULL;
	enterKeyWord(SYMBOL_ALLOC,"allocMem");
	enterKeyWord(SYMBOL_WHILE,"while");
	enterKeyWord(SYMBOL_IF,"if");
	enterKeyWord(SYMBOL_RETURN,"return");
	enterKeyWord(SYMBOL_ELSE,"else");
	enterKeyWord(SYMBOL_VOID,"void");
	enterKeyWord(SYMBOL_INT,"int");
	enterKeyWord(SYMBOL_CONST,"const");
	enterKeyWord(SYMBOL_CHAR, "char");
	enterKeyWord(SYMBOL_FD, "FILE");
	enterKeyWord(SYMBOL_PROC, "procedure");
	enterKeyWord(SYMBOL_PROT, "prototype");
	enterKeyWord(SYMBOL_VAR, "variable");
	enterKeyWord(SYMBOL_RECORD, "record");
	enterKeyWord(SYMBOL_STRUCT,"struct");
	enterKeyWord(SYMBOL_ELSIF, "elsif");
	enterKeyWord(SYMBOL_FPRINTF, "fprintf");
	enterKeyWord(SYMBOL_BOOL, "bool");
	enterKeyWord(SYMBOL_EXIT, "exit");
	enterKeyWord(SYMBOL_INCLUDE, "include");
	enterKeyWord(SYMBOL_NULL,"NULL");
}

/* initialize list that holds the procedure positions, where we have to fix jumps */
procedure 
void setupProcFixList(void) {
	allocMem(procFixList);
	procFixList->pos = 0;
	procFixList->proc = NULL;
	procFixList->next = NULL;
}


/* add procedure to fixList */
procedure 
void addProcToFixList(int pos, struct object *proc) {
	variable struct procFixStruct *tmp; variable struct procFixStruct *fix;
	tmp = procFixList; fix = NULL;
	while (tmp->next != NULL) {
		tmp = tmp->next;
	}
	allocMem(fix);
	fix->pos = pos;
	fix->proc = proc;
	tmp->next = fix;
	fix->next = NULL;
}

procedure 
void getCharacter(void)  {
	variable bool isEOF;
	isEOF = feof(inputFile);
	if (isEOF == false) {
		currentCharacter = fgetc(inputFile);
		if (currentCharacter == '\n') {
			lineNumber = lineNumber + 1;
		}
	} else {
		terminate = true;
	}
}

/* check if identifier is in keyword table or if it is just a variable */
procedure 
int handleIdentifier(void) {
	variable int pos; variable int ret;
	variable struct keyWordStruct *tmp;
	pos = 0; ret = 0; tmp = NULL;
	while (((currentCharacter >= 'A') && (currentCharacter <= 'Z')) || 
		((currentCharacter >= 'a') && (currentCharacter <= 'z')) || (currentCharacter == '_')) {
		if (pos < identifierLength) {
			idName[pos]=currentCharacter;
			pos = pos + 1;
		}		
		getCharacter();
	}
	idName[pos]='\0';
	pos = 0;
	tmp = keyWordTable;
	while (tmp != NULL) {
		ret = stringCompare(tmp->keyWord,idName);
		if (ret == 0)  {
			return tmp->keyWordSymbol;
		}
		tmp = tmp->next;
	}
	return 0;
}

/* handle the occurrence of a number (only integers in slimC */
procedure 
int handleNumber(void) {
	numberVal = 0;
	while ((currentCharacter >= '0') &&  (currentCharacter <= '9')) {
		if (numberVal <= (32500 - (ord(currentCharacter) - 48)) / 10) {
			numberVal = 10 * numberVal + ord(currentCharacter) - 48;
		} else {
			mark("number too large");
			numberVal = 0;
			return -1;
		}
		getCharacter();
	}
	return 0;
}

/* handle the occurrence of a string literal */
procedure 
void handleStringLiteral(void) {
	variable int pos;
	pos=0;
	getCharacter();
	if (currentCharacter == '"') {
		idName[pos]='\0';
	} else {
		while (currentCharacter != '"') {
			if (pos < stringLiteralLength) {
				if (currentCharacter == '\\') {
					getCharacter();
					if (currentCharacter == 'n') {
						idName[pos] = '\n';
					} elsif (currentCharacter == '\\') {
						idName[pos] = '\\';
					} else {
						mark("unknown escape sequence in string literal");
						exit(-1);
					}
				} else {
					idName[pos] = currentCharacter;
				}
				getCharacter();
				pos = pos + 1;
			} else {
				mark("identifier or keyword too long");
				idName[pos] = '\0';
				currentCharacter = '"'; /* break while loop */
			}
		}
		idName[pos]='\0';
		getCharacter();
	}
}

/* handle occurrence of character literal */
procedure 
int handleCharacterLiteral(void) {
	variable int pos;
	pos = 0;
	getCharacter();
	if (currentCharacter == '\'')  {
		idName[pos]='\0';
	} elsif (currentCharacter == '\\') {
		getCharacter();
		if (currentCharacter == '\\') {
			idName[pos] = '\\';
			getCharacter();
		} elsif  (currentCharacter == 'n') {
			idName[pos] = '\n';
			getCharacter();
		} elsif (currentCharacter == '0') {
			idName[pos] = '\0';
			getCharacter();
		} elsif (currentCharacter == '\'') {
			idName[pos] = '\'';
			getCharacter();
		} else {
			mark("unkown espace sequence");
			return -1;
		}
	} else {
		idName[pos] = currentCharacter;
		getCharacter();
	}
	pos = pos + 1;
	if (currentCharacter != '\'') {
		mark("unproper termination of character literal");
		return -1;
	} else {
		idName[pos] = '\0';
		getCharacter();
	}
	return 0;
}

/* determine symbol based on input stream */
procedure 
void getSymbol (void) {
	variable int ret;
	variable int isKeyWord;
	ret = 0;
	isKeyWord = 0;
	currentSymbol=0;
	while ((terminate == false) && (currentCharacter <= ' ')) {
		getCharacter();
	} 

	if (terminate == true )  {
		currentSymbol = SYMBOL_END;
	} elsif (currentCharacter == '&') {	
		getCharacter();
		if (currentCharacter == '&') {
			currentSymbol = SYMBOL_AND;
			getCharacter();
		} else {
			mark("error in logic and");
			currentSymbol = SYMBOL_UNKNOWN;
			getCharacter();
		}
	} elsif (currentCharacter == '|') {	
		getCharacter();
		if (currentCharacter == '|') {
			currentSymbol = SYMBOL_OR;
			getCharacter();
		} else {
			mark("error in logic or");
			currentSymbol = SYMBOL_UNKNOWN;
			getCharacter();
		}
	} elsif (currentCharacter == '*') {
		currentSymbol = SYMBOL_MUL;
		getCharacter();
	} elsif (currentCharacter == '#') {
		getCharacter();
		isKeyWord = handleIdentifier();
		if (isKeyWord == SYMBOL_INCLUDE) {
			currentSymbol = SYMBOL_HASHINCLUDE;
		} else {
			mark("bad include");
			exit(-1);	}
	} elsif (currentCharacter == '.') {
		getCharacter();
		currentSymbol = SYMBOL_DOT;
	} elsif (currentCharacter == '/') {
		currentSymbol = SYMBOL_DIV;
		getCharacter();
		if (currentCharacter == '*') {
			getCharacter();
			while (currentCharacter != '*') {
				getCharacter();
			}
			getCharacter();
			if (currentCharacter == '/') {
				getCharacter();
			} else {
				mark("bad termination of comment");
				exit(-1);
			}
			getSymbol();
		}
	} elsif (currentCharacter == '%') {
		currentSymbol = SYMBOL_MOD;
		getCharacter();
	} elsif (currentCharacter == '(') {	
		currentSymbol = SYMBOL_LPARAN;
		getCharacter();
	} elsif (currentCharacter == ')') {	
		currentSymbol = SYMBOL_RPARAN;
		getCharacter();
	} elsif (currentCharacter == ']') {
		currentSymbol = SYMBOL_RBRAK;
		getCharacter();
	} 	elsif (currentCharacter == '[') {
		currentSymbol = SYMBOL_LBRAK;
		getCharacter();
	} elsif (currentCharacter == '+') {
		currentSymbol = SYMBOL_ADD;
		getCharacter();
	} elsif (currentCharacter == '-') {
		currentSymbol = SYMBOL_SUB;
		getCharacter();
		if (currentCharacter == '>') {
			currentSymbol = SYMBOL_ARROW;
			getCharacter();
		}
	} elsif (currentCharacter == '{') {
		currentSymbol = SYMBOL_CLBRAK;
		getCharacter();
	} elsif (currentCharacter == '}') {
		currentSymbol = SYMBOL_CRBRAK;
		getCharacter();
	} elsif (currentCharacter == '=') {
		currentSymbol = SYMBOL_ASSIGN;
		getCharacter();
		if (currentCharacter == '=') {
			currentSymbol = SYMBOL_EQUALTEST;
			getCharacter();
		}
	} elsif (currentCharacter == '<') {
		currentSymbol = SYMBOL_LESS;
		getCharacter();
		if (currentCharacter == '='){
			currentSymbol = SYMBOL_LEQ;
			getCharacter();
		}
	} elsif (currentCharacter == '>') {
		currentSymbol = SYMBOL_GREATER;
		getCharacter();
		if (currentCharacter == '=') {
			currentSymbol = SYMBOL_GEQ;
			getCharacter();
		}
	} elsif (currentCharacter == '!') {
		getCharacter();
		if (currentCharacter == '=') {
			currentSymbol = SYMBOL_NOT;
			getCharacter();
		} else {
			currentSymbol = SYMBOL_UNKNOWN;
			getCharacter();
		}
	} elsif ((currentCharacter >= '0') &&  (currentCharacter <= '9')) {
		ret = handleNumber();
		if (ret < 0) {
			exit(-1);
		}
		currentSymbol = SYMBOL_NUMBER;
	} 
	elsif (((currentCharacter >= 'A') && (currentCharacter <= 'Z')) || 
		   ((currentCharacter >= 'a') && (currentCharacter <= 'z')) || (currentCharacter == '_')) {
		isKeyWord = handleIdentifier();
		if (isKeyWord > 0) {
			currentSymbol = isKeyWord;
		} else  {
			currentSymbol = SYMBOL_IDENTIFIER;
		}
	} elsif (currentCharacter == '"') {
		handleStringLiteral();
		currentSymbol=SYMBOL_STRINGLITERAL;
	} elsif (currentCharacter == ';') {
		currentSymbol = SYMBOL_SEMICOLON;
		getCharacter();
	} elsif (currentCharacter == ',') {
		currentSymbol = SYMBOL_COMMA;
		getCharacter();
	} elsif (currentCharacter == '\'') { 
		ret = handleCharacterLiteral();
		if (ret == -1) {
			exit(-1);
		}
		currentSymbol = SYMBOL_CHARACTERLITERAL;
	} else {
		currentSymbol = SYMBOL_UNKNOWN;
		getCharacter();
	}
}

/* ---- end: scanner ---- */
/* ---- begin: parser ---- */

procedure
void enterObject(int cl, int n, char name[], int typeIndex) {
	variable struct object *obj;
	obj = NULL;
	allocMem(obj);
	obj->cl = cl; obj->val = n; 
	stringCopy(name,obj->name); 
	obj->typeIndex = typeIndex; obj->next = topScope->next; topScope->next = obj;
}

/* start procedure for parser */
procedure 
void parser(void) {
	variable struct object *obj;
	variable struct typeDesc *type; variable struct typeDesc *tp;
	obj = NULL; type = NULL; tp = NULL;

	setupKeyWordTable();
	setupProcFixList();

	/* allocate memory for type list and initialize */
	allocMem(typeList);
	typeList->next = NULL;
	typeList->index = 0;

	/* enter basic types in type list */
	type = newType();type->form = FORM_INT; type->size = 4; intTypeIndex = type->index;
	type = newType();type->form = FORM_CHAR; type->size = 4; charTypeIndex = type->index;
	type = newType();type->form = FORM_BOOL; type->size = 4; boolTypeIndex = type->index;
	type = newType();type->form = FORM_STRLIT; type->size = 4; strLitIndex = type->index;
	type = newType();type->form = FORM_VOID; voidTypeIndex = type->index;
	
	/* build file type: like record with reference type */
	type = newType(); type->form = FORM_FILE; type->size = 4; fileTypeIndex = type->index;
	tp = newType(); tp->base = type; tp->size = wordSize; tp->form = FORM_REF;	
	type->refIndex = tp->index;

	/* topScope is empty for now */
	topScope = NULL;

	/* allocate memory for guard, which is used in list searching */
	allocMem(guard);
	guard->typeIndex = intTypeIndex;

	/* open a new scope now and enter basic objects */
	openScope();
	enterObject(CLASS_CONST, 1, "true", boolTypeIndex);
	enterObject(CLASS_CONST, 0, "false", boolTypeIndex);
	enterObject(CLASS_TYP, 0, "FILE", fileTypeIndex);

	/* the universe holds all special procedures and basic objects */
	universe = topScope;

	/* enter special procedures - enterSproc(retTypeIndex,sprocNum,name) */
	enterSproc(0,0,"writeChar"); 
	enterSproc(0,1,"writeInt"); 
	type = getType(fileTypeIndex);
	enterSproc(type->refIndex,2,"fopen");
	enterSproc(boolTypeIndex,3,"fclose");
	enterSproc(boolTypeIndex,4,"feof");
	enterSproc(charTypeIndex,5,"fgetc");
	enterSproc(intTypeIndex,6,"ord");
	getCharacter();

	/* setup entry procedure */
	setupMain();

	


	/* start parsing with non-terminal program, see EBNF */
	program();

	/* fixups */
	procFix();
}

/* semicolon := ';' */
procedure 
void semicolon(void) {
	if (currentSymbol != SYMBOL_SEMICOLON) {
		mark("missing semicolon");
		exit(-1);
	} else {
		getSymbol();
	}
}

/* constDefinition := 'variable' 'const' typeSpecifier identifier '=' simpleExpression ';' */
procedure 
struct object * constDefinition(void)  {
	variable struct object *obj;
	variable struct item *x;
	x = NULL; obj = NULL;
	allocMem(x);
	if ((currentSymbol == SYMBOL_INT) || (currentSymbol == SYMBOL_CHAR) || (currentSymbol == SYMBOL_BOOL)) {
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			obj = newObj(CLASS_CONST);
			getSymbol();
			if (currentSymbol == SYMBOL_ASSIGN) {
				getSymbol();
				simpleExpression(x);
				if (x->mode == CLASS_CONST) {
					obj->val = x->a;
					obj->typeIndex = x->typeIndex;
				}
			} else {
				mark("missing assignment operator");
				exit(-1);
			}
		} else {
			mark("missing identifier in constant defintion");
			exit(-1);
		}
	} else {
		mark("bad type in constant definition");
		exit(-1);
	}
	return obj;
}

/* variableDeclaration := variableDefinition | constDefinition | structDefinition | fileDefinition */
procedure 
struct object * variableDeclaration(int cl)  {
	variable int n;
	variable struct typeDesc *baseType; variable struct typeDesc *type; variable struct typeDesc *tp;
	variable struct object *obj; variable struct object *typeObj;
	baseType = NULL; typeObj = NULL; obj = NULL; type = NULL; tp = NULL; n = 1;
	if (currentSymbol == SYMBOL_CONST) {
		getSymbol();
		obj = constDefinition();
	} elsif (currentSymbol == SYMBOL_FD) {
		typeObj = find(); 
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			obj = newObj(cl);
			getSymbol();
			if (typeObj->cl == CLASS_TYP) { 
				baseType = getType(typeObj->typeIndex);
				obj->typeIndex = baseType->refIndex;
			} else { 
				mark("type?"); 
				exit(-1);
			}
		} else {
			mark("missing identifier in FILE definition");
			exit(-1);
		}
	} elsif ((currentSymbol == SYMBOL_INT) || (currentSymbol == SYMBOL_CHAR) || (currentSymbol == SYMBOL_BOOL)) {
		if (currentSymbol == SYMBOL_INT) { 
			type = getType(intTypeIndex);
		} elsif (currentSymbol == SYMBOL_CHAR) { 
			type= getType(charTypeIndex);
		} else {
			type = getType(boolTypeIndex);
		}
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			getSymbol();
			obj = newObj(cl);
			obj->typeIndex = type->index;
			if (currentSymbol == SYMBOL_LBRAK) {
				getSymbol();
				if (currentSymbol == SYMBOL_NUMBER) {
					tp = newType();
					tp->form = FORM_ARRAY; tp->base = type;	tp->len = numberVal; tp->size = type->size * numberVal;
					obj->typeIndex = tp->index;
					getSymbol();
				} else {
					mark("missing number in brackets");
					exit(-1);
				}
				if (currentSymbol == SYMBOL_RBRAK) {
					getSymbol();
				} else {
					mark("missing right bracket");
					exit(-1);
				}
			}
		} else {
			mark("missing identifier");
			exit(-1);
		}
	} elsif (currentSymbol == SYMBOL_STRUCT) {
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			getSymbol();
			typeObj = find();
			if (currentSymbol == SYMBOL_MUL) {
				getSymbol();
				if (currentSymbol == SYMBOL_IDENTIFIER) {
					obj = newObj(cl);
					if (typeObj->cl == CLASS_TYP) { 
						baseType = getType(typeObj->typeIndex);
						obj->typeIndex = baseType->refIndex;
					} else { 
						mark("type?"); 
						exit(-1);
					}
					getSymbol();
				} else {
					mark("missing identifier");
					exit(-1);
				}
			} else {
				mark("missing struct variable identifier");
				exit(-1);
			}
		} else {
			mark("missing identifier in struct definition");
			exit(-1);
		}
	} else {
		mark("unknown or mising type specifier");
		exit(-1);
	}
	semicolon();
	return obj;
}

/* find object in symbol table by its name */
procedure 
struct object * find (void) {
	variable bool cond;
	variable int ret;
	variable struct object *s; variable struct object *x; variable struct object *obj;
	s = NULL; x = NULL;	obj = NULL; ret = -1; cond = true;
	s = topScope;

	/* copy idname to guard */
	stringCopy(idName,guard->name); 
	while (cond == true){ 
		x = s->next; 
		ret = stringCompare(x->name,idName);	
		while (ret != 0) { /* iterate over objects in currentLevel, while names are different */
			x = x->next; 
			ret = stringCompare(x->name,idName);
		}

		/* did we find the guard */
		if (x != guard) { 
			obj = x;
			cond = false;
		}
		
		/* if we reach the universe and nothing was found --> error */
		if ((s == universe) && (cond == true)) { 
			obj = x;
			mark("undef");
			exit(-1);
		}
		s = s->dsc; /* step up one level */
	}
	return obj;
}

/* find procedure object in symbol table by its name - do not complain if not found */
procedure
struct object *findProc(void) {
	variable int ret; variable bool cond;
	variable struct object *s; variable struct object *obj; variable struct object *x;
	s = NULL; obj = NULL; x = NULL; cond = true;
	s = topScope;
	stringCopy(idName,guard->name);
	while (cond == true) {
		x=s->next;
		ret = stringCompare(x->name,idName);	
		while (ret != 0) {
			x = x->next;
			ret = stringCompare(x->name,idName);
		}
		if ((x != guard) || (s == universe)) {
			cond = false;
		}
		obj = x;
		s = s->dsc;
	}
	return obj;
}

/* structDeclaration := 'record' structSpecifier '{' variableDeclaration {variableDeclaration} '}' ';'. */
procedure 
void structDeclaration(void) {
	variable struct object *tmpObj; variable struct object *obj;
	variable struct typeDesc *tmpType; variable struct typeDesc *type; variable struct typeDesc *ref;
	ref = NULL; obj = NULL; type = NULL; tmpObj = NULL; tmpType = NULL;
	if (currentSymbol == SYMBOL_STRUCT) {
		getSymbol();		
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			obj = newObj(CLASS_TYP);
			getSymbol();
			if (currentSymbol == SYMBOL_CLBRAK) {
				getSymbol();
				if (currentSymbol == SYMBOL_CRBRAK) {
					mark("declaration of empty struct not allowed");
					exit(-1);
				}
				type = newType(); 	/* rec. type */
				type->form = FORM_RECORD; type->size = 0;	
				obj->typeIndex = type->index;
				ref = newType(); 	/* ref. to rec. */
				ref->form = FORM_REF; 
				ref->size = wordSize; 
				ref->base = type; 
				type->refIndex = ref->index;
				openScope();
				while (currentSymbol == SYMBOL_VAR) {
					getSymbol();
					tmpObj = variableDeclaration(CLASS_FLD);
					tmpObj->val = type->size;
					tmpType = getType(tmpObj->typeIndex);
					type->size = type->size + tmpType->size;
				}
				if (currentSymbol != SYMBOL_CRBRAK) {
					mark("bad variable declaration");
					exit(-1);
				}
			}
			if (currentSymbol == SYMBOL_CRBRAK){
				getSymbol();
			} else {
				mark("missing right curly bracket in struct declaration");
				exit(-1);
			}
			semicolon();
		} else {
			mark("missing struct identifier");
			exit(-1);
		}
		type->fields = topScope->next;
		closeScope();
	} else {
		mark("missing struct keyword");
		exit(-1);
	}
}

procedure 
int argument(struct object *proc, int parBlkSize, struct object *currentParam)  {
	variable struct object *obj;
	variable struct typeDesc *type; variable struct typeDesc *tp;
	variable int tpIndex; variable int parSize;
	obj = NULL; tp = NULL; type = NULL; parSize = 0;tpIndex=0;
	
	if ((currentSymbol == SYMBOL_INT) || (currentSymbol == SYMBOL_CHAR) || (currentSymbol == SYMBOL_BOOL)) {
		if (currentSymbol == SYMBOL_INT){
			tpIndex = intTypeIndex;
		} elsif (currentSymbol == SYMBOL_CHAR){
			tpIndex = charTypeIndex;
		} elsif (currentSymbol == SYMBOL_BOOL) {
			tpIndex = boolTypeIndex;
		}
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) { /* int x */
			if (proc->hasPrototype == true) {
				stringCopy(idName,currentParam->name); 
			} else { 
				obj = newObj(CLASS_VAR); 
				obj->typeIndex = tpIndex;
			}
			type = getType(tpIndex);
			parSize = parBlkSize + type->size;
			getSymbol();
			if (currentSymbol == SYMBOL_LBRAK) {
				getSymbol();
				if (currentSymbol == SYMBOL_RBRAK) { /* we have an array, such as char a[] */
					if (proc->hasPrototype == true) { /* check consistency */
						type = getType(currentParam->typeIndex);
						if ((type->form != FORM_ARRAY) || (type->base->index != tpIndex))  { /* check form, base (differ) */
							mark("type mismatch in prototype and declaration");
							exit(-1);
						}
					} else {
						tp = newType();
						tp->form = FORM_ARRAY;
						type = getType(tpIndex);
						tp->base = type;
						tp->size = wordSize + 8; /* adr + len + onstack */ 
						obj->typeIndex = tp->index;
						obj->cl = CLASS_PAR; 
					}
					parSize = parBlkSize + wordSize + 8;
					getSymbol();
				} else {
					mark("missing right bracket");
					exit(-1);
				}
			} else { /* consistency type check for non-array vars */
				if (proc->hasPrototype == true) {
					if (currentParam->typeIndex != tpIndex) {
						mark("type mismatch in prototype or declaration");
						exit(-1);
					}
				}
			}
			
		} else {
			mark("missing identifier");
			exit(-1);
		}
	} elsif (currentSymbol == SYMBOL_STRUCT) { 
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			getSymbol();
			obj = find();
			type = getType(obj->typeIndex);
			if (currentSymbol == SYMBOL_MUL) {
				getSymbol();
				if (currentSymbol == SYMBOL_IDENTIFIER) {
					if (proc->hasPrototype == true) {
						stringCopy(idName,currentParam->name);
						if (currentParam->typeIndex != type->refIndex) {
							mark("type mismatch in prototype/declaration");
							exit(-1);
						}
					} else {
						obj = newObj(CLASS_PAR);
						obj->typeIndex = type->refIndex;
					}
					parSize = parBlkSize + wordSize;
					getSymbol();
				} else {
					mark("missing variable name");
					exit(-1);
				}
			} else {
				mark("bad struct variable");
				exit(-1);
			}
		} else {
			mark("missing identifier");
			exit(-1);
		}
	} else {
		mark("bad argument");
		exit(-1);
	}
	return parSize;
}

procedure 
int prototypeArgument(int parBlkSize)  {
	variable struct object *obj;
	variable struct typeDesc *type; variable struct typeDesc *tp;
	variable int tpIndex; variable int parSize;
	obj = NULL; tp = NULL; type = NULL; parSize = 0;tpIndex=0;

	if ((currentSymbol == SYMBOL_INT) || (currentSymbol == SYMBOL_CHAR) || (currentSymbol == SYMBOL_BOOL)) { /* int, char, bool */
		if (currentSymbol == SYMBOL_INT){
			tpIndex = intTypeIndex;
		} elsif (currentSymbol == SYMBOL_CHAR){
			tpIndex = charTypeIndex;
		} elsif (currentSymbol == SYMBOL_BOOL) {
			tpIndex = boolTypeIndex;
		}
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			obj = newObj(CLASS_VAR);
			getSymbol();
		} else {
			obj = newAnonymousObj(CLASS_VAR); 
		}
		obj->typeIndex = tpIndex;
		type = getType(tpIndex);
		parSize = parBlkSize + type->size;
		if (currentSymbol == SYMBOL_LBRAK) { /* char [] */
			getSymbol();
			if (currentSymbol == SYMBOL_RBRAK) { 
				tp = newType();
				tp->form = FORM_ARRAY;
				type = getType(tpIndex);
				tp->base = type;
				tp->size = wordSize + 8; 
				obj->typeIndex = tp->index;
				obj->cl = CLASS_PAR; 
				parSize = parBlkSize + wordSize + 8; 
				getSymbol();
			} else {
				mark("missing right bracket");
				exit(-1);
			}
		}	
	} elsif (currentSymbol == SYMBOL_STRUCT) { 
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			getSymbol();
			obj = find();
			type = getType(obj->typeIndex);
			if (currentSymbol == SYMBOL_MUL) {
				getSymbol();
				if (currentSymbol == SYMBOL_IDENTIFIER) {
					obj = newObj(CLASS_PAR);
					getSymbol();
				} else {
					obj = newAnonymousObj(CLASS_PAR);
				}
				obj->typeIndex = type->refIndex;
				parSize = parBlkSize + wordSize;
			} else {
				mark("bad struct variable");
				exit(-1);
			}
		} else {
			mark("missing struct qualifier");
			exit(-1);
		}
	} else {
		mark("bad argument");
		exit(-1);
	}
	return parSize;
}

procedure 
int argumentList(int isPrototype, struct object *proc, int parBlkSize) {
	variable int parSize; 
	variable struct object *first;
	parSize = 0; 
	if (isPrototype==1) {
		parSize = prototypeArgument(parBlkSize);
	} else {
		first = proc->dsc; /* not next because we are still at the head */
		parSize = argument(proc,parBlkSize,first);
		if (proc->hasPrototype == true) {
			first = first->next; /* next argument (might be guard) */
		}
	}
	while (currentSymbol == SYMBOL_COMMA) {
		getSymbol();
		if (isPrototype==1) {
			parSize = prototypeArgument(parSize);
		} else {
			if (proc->hasPrototype == true) {
				if (first == guard) {
					mark("number of args differ");
					exit(-1);
				}
				parSize = argument(proc,parSize,first);
				first = first->next;
			} else {
				parSize = argument(proc,parSize,first);
			}
		}
		
	}
	if ((isPrototype == 0) && (proc->hasPrototype == true)) {
		if (first != guard) {
			mark("prototype requires more arguments");
			exit(-1);
		}
	}
	return parSize;
}

procedure 
int arguments(int isPrototype, struct object *proc, int parBlkSize) {
	variable int parSize;
	variable struct object *first;
	parSize = 0; first = NULL;
	if (currentSymbol == SYMBOL_VOID) {
		getSymbol();
		if (isPrototype == 1) {
			proc->hasArguments = false;
		} else {
			if (proc->hasArguments == true) {
				mark("prototype requires at least one argument");
				exit(-1);
			}
		}
		parSize = parBlkSize + 0;
	} elsif ((currentSymbol == SYMBOL_INT) || (currentSymbol == SYMBOL_CHAR)  || 
		(currentSymbol == SYMBOL_BOOL) || (currentSymbol == SYMBOL_STRUCT)) {
			if (isPrototype == 1 ) {
				proc->hasArguments = true;
			} else {
				if ((proc->hasArguments == false) && (proc->hasPrototype == true)) {
					mark("prototype requires void");
					exit(-1);
				}
			}
			parSize = argumentList(isPrototype,proc,parBlkSize);
	}
	return parSize;
}

/* handle if..elsif..else constructs */
procedure 
void conditionalBranchStatement(struct object *proc, int retSize) {
	variable int L;
	variable struct item *x;
	x=NULL; L=0;
	allocMem(x);
	if (currentSymbol == SYMBOL_LPARAN) {
		getSymbol();
		expression(x);
		cJump(x);
		if (currentSymbol == SYMBOL_RPARAN) {
			getSymbol();
		} else {
			mark("missing closing right paranetheses of if statement");
			exit(-1);
		}
	} else {
		mark("missing left paranetheses in if statement");
		exit(-1);
	}
	if (currentSymbol == SYMBOL_CLBRAK) {
		getSymbol(); 
		statementSequence(proc, retSize); L = 0;
		if (currentSymbol == SYMBOL_CRBRAK) {
			getSymbol();
		} else {
			mark("missing right curly bracket");
			exit(-1);
		}
	} else {
		mark("missing left curly bracket");
		exit(-1);
	}
	while (currentSymbol == SYMBOL_ELSIF) {
		getSymbol();
		if (currentSymbol == SYMBOL_LPARAN) {
			getSymbol(); L = fJump(L); fixLink(x->a); expression(x); cJump(x);
			if (currentSymbol == SYMBOL_RPARAN) {
				getSymbol();
			} else {
				mark("missing closing right paranetheses of if statement");
				exit(-1);
			}
		}
		if (currentSymbol == SYMBOL_CLBRAK) {
			getSymbol(); statementSequence(proc, retSize);
			if (currentSymbol == SYMBOL_CRBRAK) {
				getSymbol();
			} else {
				mark("missing right curly bracket in elseif");
					exit(-1);
			}
		} else {
			mark("missing left curly bracket");
			exit(-1);
		}
	}
    if (currentSymbol == SYMBOL_ELSE) {
		getSymbol(); 
		L = fJump(L); 
		fixLink(x->a);
		if (currentSymbol == SYMBOL_CLBRAK) {
			getSymbol(); statementSequence(proc, retSize);
			if (currentSymbol == SYMBOL_CRBRAK) {
				getSymbol();
			} else {
				mark("missing right curly bracket");
				exit(-1);
			}
		} else {
			mark("missing left curly bracket in else");
			exit(-1);
		}
	} else {
		fixLink(x->a);
	}
	fixLink(L);
}

/* handle simpleExpression := term { '+'|'-'|'||' term } */
procedure
void simpleExpression(struct item *x) {
	variable int op;
	variable struct item *y;
	y = NULL; op = 0;
	allocMem(y);
	if (currentSymbol == SYMBOL_SUB) {
		getSymbol(); term(x);
		if (currentSymbol == SYMBOL_CHARACTERLITERAL) { /* no negative char */
			mark("negative character ?");
			exit(-1);
		} else {
			opOne(SYMBOL_SUB,x);
		}
	} else {
		term(x);
	}
	while ((currentSymbol == SYMBOL_SUB) || (currentSymbol == SYMBOL_ADD) || (currentSymbol == SYMBOL_OR)) {
		op = currentSymbol;
		getSymbol();
		if (op == SYMBOL_OR) {
			opOne(op,x);
		}
		term(y);
		opTwo(op,x,y);
		if (op == SYMBOL_OR) {
			if(x->r != y->r) {
				regs[x->r] = 0;
				x->r = y->r;
			}
		}
	}
}

/* find a record field */
procedure 
struct object *findField(struct object *fields) {
	variable int ret;
	ret=0;
	stringCopy(idName,guard->name);
	ret = stringCompare(fields->name,idName);
	while (ret != 0) {
		fields = fields->next;
		ret = stringCompare(fields->name,idName);
	}
	return fields;
}

/* field accessor; */
procedure
void emitField(struct item *x, struct object *y, int indLev) {
	variable int rA;
	variable struct typeDesc *type;
	type = getType(y->typeIndex);
	rA = getReg(); 
	if ((x->lev == 0) && (indLev == 0)) {
		x->a = RISC_MEMSIZE + x->a;
		x->indLev = x->indLev + 1;
	}
	
	/* if indirection level > 0, heap addr. required */
	if (indLev > 0) {
		put(RISC_HLDW,rA,x->r,x->a);
		put(RISC_ADDI,rA,rA,y->val);
		x->indLev = indLev;
	} else {	
		put(RISC_LDW,rA,x->r,x->a); 
		put(RISC_ADDI,rA,rA,y->val);
	}
	regs[x->r] = 0;
	x->r = rA; x->a = 0; x->typeIndex = y->typeIndex; 
	if (y->cl == CLASS_FLD) { 
		/* reset item mode to indirect addr. */
		x->mode = CLASS_IND;  
	}
}

/* find out whether we access an array or a record */
procedure
void selector(struct item *x) {
	variable struct object *obj;
	variable struct typeDesc *type;
	variable struct item *y;
	variable int indLev; 
	y = NULL; type = NULL; obj = NULL; indLev = 0;
	allocMem(y);
	while ((currentSymbol == SYMBOL_LBRAK) || (currentSymbol == SYMBOL_ARROW)) {
		
		/* deal with array access */
		if (currentSymbol == SYMBOL_LBRAK) {
			getSymbol();
			simpleExpression(y);
			type = getType(x->typeIndex);
			if (type->form == FORM_ARRAY) {
				idx(x,y);
			} else {
				mark("not an array");
				exit(-1);
			}
			if (currentSymbol == SYMBOL_RBRAK) {
				getSymbol();
			} else {
				mark("missing right bracket");
				exit(-1);
			}
		
		/* deal with record access */
		} else { 
			getSymbol();
			if (currentSymbol == SYMBOL_IDENTIFIER) {
				type = getType(x->typeIndex);
				if (type->base->form == FORM_RECORD) {
					obj = findField(type->base->fields);
					getSymbol();
				}
				if (obj != guard) {
					emitField(x,obj,indLev);
				} else {
					mark("field undef");
					exit(-1);
				}
			} else {
				mark("missing identifier");
				exit(-1);
			}
			indLev = indLev + 1;
		}
	}
}

procedure 
void factor(struct item *x) {
	variable struct object *obj; variable struct object *par; variable struct object *procObj;
	variable struct item *y;
	variable bool cond;
	variable int jumpTo;
	obj = NULL; par = NULL; procObj = NULL; cond = true; jumpTo = 0; 
	allocMem(y);
	if (currentSymbol == SYMBOL_IDENTIFIER) {
		obj = find();
		getSymbol();
		makeItem(x,obj);
		if (x->mode == CLASS_PROC) { 
			procObj = obj;
			if (procObj->val < 0) { /* prototype => dsc still points to head for further use */
				par = obj->dsc->next;
			} else {
				par = obj->dsc;
			}
			if (currentSymbol == SYMBOL_LPARAN) { 
				getSymbol(); 
				if (currentSymbol == SYMBOL_RPARAN) {
					getSymbol();
				} else {
					par = parameter(par);
					while (cond == true) {
						if (currentSymbol == SYMBOL_COMMA) {
							getSymbol();
						} elsif (currentSymbol == SYMBOL_RPARAN) {
							getSymbol();
							cond = false;
						}
						if (cond == true) {
							par = parameter(par);
						}
					}
				}
			}
			if (isParameter(par) != true) {
				if (procObj->val < 0) {
					addProcToFixList(pc,procObj);
					jumpTo = 0;
				} else {
					jumpTo = x->a - pc;
				}
				call(x,jumpTo);
			} else {
				mark("too few parameters");
				exit(-1);
			}
		} elsif (x->mode == CLASS_SPROC) { 
			param(y);
			if (obj->val <= 6) {
				IOCall(x,y);
			}
		} else {
			selector(x);
		}
	} elsif (currentSymbol == SYMBOL_NUMBER) {
		makeConstItem(x,intTypeIndex,numberVal);
		getSymbol();
	} elsif (currentSymbol == SYMBOL_CHARACTERLITERAL) {
		/* here: implicit cast from char to int - allowed */
		makeConstItem(x,charTypeIndex,idName[0]); 
		getSymbol();
	} elsif (currentSymbol == SYMBOL_LPARAN) {
		getSymbol();
		expression(x);
		if (currentSymbol == SYMBOL_RPARAN) {
			getSymbol();
		} else {
			mark("missing right paranetheses");
			exit(-1);
		}
	} else {
		mark("factor ?");
		exit(-1);
	}
}

procedure 
void expression(struct item *x) {
	variable int op;
	variable struct item *y;
	y = NULL; op=0;
	allocMem(y);
	simpleExpression(x);
	if ((currentSymbol == SYMBOL_LESS) || (currentSymbol == SYMBOL_GREATER) ||
		(currentSymbol == SYMBOL_NOT)  || (currentSymbol == SYMBOL_GEQ) ||
		(currentSymbol == SYMBOL_LEQ)  || (currentSymbol == SYMBOL_EQUALTEST)) {
		op = currentSymbol;
		getSymbol();
		if (currentSymbol == SYMBOL_NULL) { /* handle null relation */
			getSymbol();
			if ((op == SYMBOL_EQUALTEST) || (op == SYMBOL_NOT)) {
				nullRelation(op,x);
			} else {
				mark("NULL in incompatible relation");
				exit(-1);
			}
		} else {
			simpleExpression(y);
			relation(op,x,y);
		}
	}
}

procedure
void iterationStatement(struct object *proc, int retSize) {
	variable struct item *x;
	variable int L;
	x = NULL;
	allocMem(x);
	L = pc;
	if (currentSymbol == SYMBOL_LPARAN) {
		getSymbol(); expression(x); cJump(x);
		if (currentSymbol == SYMBOL_RPARAN) {
			getSymbol();
		} else {
			mark("missing right parantheses");
			exit(-1);
		}
		if (currentSymbol == SYMBOL_CLBRAK) {
			getSymbol();
			statementSequence(proc, retSize); bJump(L); fixLink(x->a);
			if (currentSymbol == SYMBOL_CRBRAK) {
				getSymbol();
			} else {
				mark("missing curly right parantheses");
				exit(-1);
			}
		} else {
			mark("missing curly left parantheses");
			exit(-1);
		}
	} else {
		mark("missing left parantheses");
		exit(-1);
	}
}

procedure 
void term(struct item *x) {
	variable struct item *y;
	variable int op;
	op = 0; y = NULL;
	allocMem(y);
	factor(x);
	while ((currentSymbol == SYMBOL_MUL) || (currentSymbol == SYMBOL_DIV) || 
		(currentSymbol == SYMBOL_AND) ||
		(currentSymbol == SYMBOL_MOD)) {
			op = currentSymbol;
			getSymbol();
			if (op == SYMBOL_AND) {
				opOne(op,x);
			}
			factor(y);
			opTwo(op,x,y);
			if (op == SYMBOL_AND) {
				if(x->r != y->r) {
					regs[x->r] = 0;
					x->r = y->r;
				}
			}
		}
}

/* returnStatement := 'return' [expression] ';' */
procedure
void returnStatement(struct object *proc, int retSize)  {
	variable struct item *x;
	x = NULL;
	allocMem(x);
	if (currentSymbol == SYMBOL_SEMICOLON)  {
		getSymbol(); 
	} else  {
		expression(x);
		if (proc->retTypeIndex == x->typeIndex) {
			if (x->mode == CLASS_CONST) {
				put(RISC_ADDI, rv, 0, x->a);
			} elsif (x->mode == CLASS_VAR) {
				put(RISC_LDW, rv, fp, x->a);
			} elsif (x->mode == CLASS_IND) {
				put(RISC_HLDW, rv,x->r,x->a);
			} elsif (x->mode == CLASS_REG) {
				put(RISC_ADD, rv, 0, x->r);
			}
			regs[x->r] = 0; /* free x->r */
			ret(retSize);
		} else {
			mark("wrong return type");
			exit(-1);
		}
		semicolon();
	}
}

procedure 
void statement(struct object *proc, int retSize)  {
	variable struct item *x; variable struct item *y;
	variable struct object *obj; variable struct object *procObj; variable struct object *par;
	variable struct typeDesc *type;
	variable bool cond; variable int jumpTo;
	x = NULL; y = NULL; obj = NULL; par = NULL; type = NULL; procObj = NULL; jumpTo = 0;
	cond = true;
	allocMem(x); allocMem(y);	
	if (currentSymbol == SYMBOL_IDENTIFIER) {
		obj = find();
		getSymbol();
		makeItem(x,obj);
		selector(x);
		if (currentSymbol == SYMBOL_ASSIGN) {
			getSymbol();
			if (currentSymbol == SYMBOL_CHARACTERLITERAL) {
				simpleExpression(y);
				store(x,y);
			} elsif (currentSymbol == SYMBOL_NULL) {
				type = getType(x->typeIndex);
				if (type->form == FORM_REF) {
					if (x->mode == CLASS_VAR) {
						if (x->lev == 0) {
							x->a = RISC_MEMSIZE + x->a;
						}
						put(RISC_STW,0,x->r,x->a);
					} elsif (x->mode == CLASS_IND) { 
						put(RISC_HSTW,0,x->r,x->a);
					} else {
						mark("incompatible NULL pointer assignment");
						exit(-1);
					}
					regs[x->r] = 0; /* free x->r */
				} else {
					mark("incompatible NULL pointer assignment");
					exit(-1);
				}
				getSymbol();
			} else 	{
				expression(y);
				store(x,y);
			}
			semicolon();
		} elsif (x->mode == CLASS_PROC) { 
			procObj = obj;
			if (procObj->val < 0) { /* prototype => dsc still points to head for further use */
				par = obj->dsc->next;
			} else {
				par = obj->dsc;
			}
			if (currentSymbol == SYMBOL_LPARAN) { 
				getSymbol(); 
				if (currentSymbol == SYMBOL_RPARAN) {
					getSymbol();
				} else {
					par = parameter(par);
					while (cond == true) {
						if (currentSymbol == SYMBOL_COMMA) {
							getSymbol();
						} elsif (currentSymbol == SYMBOL_RPARAN) {
							getSymbol();
							cond = false;
						}
						if (cond == true) {
							par = parameter(par);
						}
					}
				}
			}
			semicolon();
			if (isParameter(par) != true) {
				if (procObj->val < 0) { /* only prototype exists till now */
					addProcToFixList(pc,procObj);
					jumpTo = 0;
				} else {
					jumpTo = x->a - pc;
				}
				call(x,jumpTo);
			} else {
				mark("too few parameters");
				exit(-1);
			}
		} elsif (x->mode == CLASS_SPROC) { 
			param(y);
			if (obj->val <= 6) {
				IOCall(x,y);
			}
			semicolon();
		}
	} elsif (currentSymbol == SYMBOL_WHILE) {
		getSymbol();
		iterationStatement(proc, retSize);
	} elsif (currentSymbol == SYMBOL_IF) {
		getSymbol();
		conditionalBranchStatement(proc,retSize);
	} elsif (currentSymbol == SYMBOL_RETURN) {
		getSymbol();
		returnStatement(proc, retSize);
	} elsif (currentSymbol == SYMBOL_ALLOC) {
		getSymbol();
		allocStatement();
	} elsif (currentSymbol == SYMBOL_EXIT) {
		getSymbol();
		exitStatement();
	} else {
		mark("unknown statement");
		exit(-1);
	}
}

procedure 
void exitStatement(void) {
	variable int r;
	variable struct item *x;
	allocMem(x);
	if (currentSymbol == SYMBOL_LPARAN) {
		getSymbol();
		if (currentSymbol == SYMBOL_RPARAN) {
			mark("empty exit");
			exit(-1);
		} else {
			expression(x);
			if (x->typeIndex == intTypeIndex) {
				if (x->mode != CLASS_REG) {
					load(x);
				}
				r = x->r;
				put(RISC_HLT,0,0,r);
				regs[r] = 0;
			} else {
				mark("only int allowed in exit");
				exit(-1);
			}
			if (currentSymbol == SYMBOL_RPARAN) {
				getSymbol();
			} else {
				mark("missing closing parantheses");
				exit(-1);
			}
			semicolon();
		}	
	} else {
		mark("standalone exit");
		exit(-1);
	}
}

procedure 
void allocStatement(void) {
	variable int r;
	variable struct object *obj;
	variable struct typeDesc *type;
	variable struct item *x;
	obj = NULL; type = NULL; x = NULL;
	allocMem(x);
	if (currentSymbol == SYMBOL_LPARAN) {
		getSymbol();
		if (currentSymbol == SYMBOL_RPARAN) {
			mark("empty alloc");
			exit(-1);
		} else {
			if (currentSymbol == SYMBOL_IDENTIFIER) {
				getSymbol();
				obj = find();
				makeItem(x,obj);
				type = getType(obj->typeIndex); /* this is the reference type */
				if (type->base->form == FORM_RECORD) {
					r = getReg();
					put(RISC_ALL,r,0,type->base->size);
					if (x->lev == 0) {
						x->a = RISC_MEMSIZE + x->a;
					}
					put(RISC_STW,r,x->r,x->a);
					regs[r] = 0;
				} else {
					mark("cannot alloc mem for datatype");
					exit(-1);
				}
			} else {
				mark("missing identifier");
				exit(-1);
			}
		}
		if (currentSymbol == SYMBOL_RPARAN) {
			getSymbol();
		} else {
			mark("missing closing right paran");
			exit(-1);
		}
		semicolon();
	} else {
		mark("missing left parantheses");
		exit(-1);
	}
}

procedure
void statementSequence(struct object *proc, int retSize) {
	if ((currentSymbol == SYMBOL_IF) || (currentSymbol == SYMBOL_EXIT) || (currentSymbol == SYMBOL_WHILE) || (currentSymbol == SYMBOL_RETURN) || 
		(currentSymbol == SYMBOL_ALLOC) || (currentSymbol == SYMBOL_IDENTIFIER)) {
		statement(proc, retSize);
	} else {
		mark("at least one statement required");
		exit(-1);
	}
	while ((currentSymbol == SYMBOL_IF) ||  (currentSymbol == SYMBOL_EXIT) || (currentSymbol == SYMBOL_WHILE) || (currentSymbol == SYMBOL_RETURN) || 
		(currentSymbol == SYMBOL_ALLOC) || (currentSymbol == SYMBOL_IDENTIFIER)) {
		statement(proc, retSize);
	}	
}

procedure
int functionBlock(struct object *proc, int varSize, int retSize) {
	variable int ret;
	variable struct object *obj;
	variable struct typeDesc *type;
	obj = NULL; type = NULL; ret = 0;
	if (currentSymbol == SYMBOL_CLBRAK) {
		getSymbol();
		if (currentSymbol == SYMBOL_CRBRAK) {
			mark("empty function");
			exit(-1);
		}
	} else {
		mark("missing left curly bracket in function block");
		exit(-1);
	}
	while (currentSymbol == SYMBOL_VAR) {
			getSymbol();
			obj = variableDeclaration(CLASS_VAR);
			type = getType(obj->typeIndex);
			varSize = varSize + type->size;
			obj->val = -varSize;
			obj->lev = currentLevel;
	}
	proc->val = pc; 
	enter(varSize);
	statementSequence(proc, retSize);
	if (currentSymbol == SYMBOL_CRBRAK) {
		getSymbol();
	} else {
		mark("missing closing curly bracket in function declaration");
		exit(-1);
	}
	return varSize;
}

procedure 
void functionPrototype(void) {
	variable struct object *obj; variable struct object *typeObj; variable struct object *proc;
	variable struct typeDesc *retType; variable struct typeDesc *varType;
	variable int markSize; variable int lclBlkSize;	variable int parBlkSize; variable int protoTypeDeclared;
	variable char procid[100];
	obj = NULL;	proc = NULL; retType = NULL; varType = NULL; typeObj = NULL;
	markSize = 8; lclBlkSize = 0; parBlkSize = 0; protoTypeDeclared = 1;

	if (currentSymbol == SYMBOL_INT) {
		retType = getType(intTypeIndex);
		getSymbol();	
	} elsif (currentSymbol == SYMBOL_FD) {
		retType = getType(fileTypeIndex);
		retType = getType(retType->refIndex);
		getSymbol();
	} elsif (currentSymbol == SYMBOL_CHAR) {
		retType = getType(charTypeIndex);
		getSymbol();
	} elsif (currentSymbol == SYMBOL_BOOL) {
		retType = getType(boolTypeIndex);
		getSymbol();
	} elsif (currentSymbol == SYMBOL_VOID) {
		getSymbol();
		retType = getType(voidTypeIndex);
	} elsif (currentSymbol == SYMBOL_STRUCT) { 
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			typeObj = find();
			retType = getType(typeObj->typeIndex);
			retType = getType(retType->refIndex);
			getSymbol();
			if (currentSymbol == SYMBOL_MUL) {
				getSymbol(); 
			} else {
				mark("malformed return value in prototype");
				exit(-1);
			}
		} else {
			mark("missing identifier in prototype");
			exit(-1);
		}
	} else {
		mark("unkown return value");
		exit(-1);
	}
	if (currentSymbol == SYMBOL_IDENTIFIER) {
		getSymbol();
		proc = findProc();
		if (proc != guard) {
			mark("multiple prototypes not allowed");
			exit(-1);
		} else {
			stringCopy(idName,procid);
			proc = newObj(CLASS_PROC);
			proc->hasPrototype = true;
			proc->retTypeIndex = retType->index;
			parBlkSize = markSize;
			incLevel(1);
			openScope();
			proc->val = -1;
		}
	} else {
		mark("missing identifier in procedure prototype");
		exit(-1);
	}
	if (currentSymbol == SYMBOL_LPARAN) {
		getSymbol();
		parBlkSize = arguments(1,proc,parBlkSize);
		lclBlkSize = parBlkSize;
		obj = topScope->next;
		while (obj != guard) {
			obj->lev = currentLevel;
			varType = getType(obj->typeIndex);
			lclBlkSize = lclBlkSize - varType->size;
			obj->val = lclBlkSize;
			obj = obj->next;
		}
		proc->dsc = topScope; /* keep possibiliy for setting topScope correctly in functionBlock */
		if (currentSymbol == SYMBOL_RPARAN) {
			getSymbol();
		} 
		else {
			mark("missing right paranthesis in procedure declaration/prototype");
			exit(-1);
		}
	} else {
		mark("no valid argument definitions");
		exit(-1);
	}
	if (currentSymbol == SYMBOL_SEMICOLON)  { 
		closeScope(); incLevel(-1);
		getSymbol();
	} else {
		mark("prototype already declared");
		exit(-1);
	}
}

/* setup main procedure */
procedure
void setupMain(void) {
	variable struct object *proc; variable struct object *firstParam; 
	variable struct object *secondParam; variable struct object *it;
	variable struct typeDesc *tp; variable struct typeDesc *type;
	proc = newAnonymousObj(CLASS_PROC);
	stringCopy("main",proc->name);
	proc->retTypeIndex = intTypeIndex;
	proc->hasArguments = true;
	proc->hasPrototype = true;

	openScope();
	it = topScope;

	/* setup argc */
	allocMem(firstParam);
	stringCopy("argc", firstParam->name);
	firstParam->typeIndex = intTypeIndex;
	firstParam->next = it->next; it->next = firstParam;
	firstParam->cl = CLASS_VAR; /* value */
	firstParam->lev = 1; /* local */
	firstParam->val = 20;
	it = it->next;

	/* setup argv */
	allocMem(secondParam);
	stringCopy("argv", secondParam->name);
	tp = newType();
	tp->form = FORM_ARRAY;
	type = getType(charTypeIndex);
	tp->base = type;
	tp->size = wordSize + 8;
	secondParam->typeIndex = tp->index;
	secondParam->cl = CLASS_PAR; 
	secondParam->next = it->next; it->next = secondParam;
	secondParam->lev = 1;
	secondParam->val = 8;
	it = it->next;
	
	addProcToFixList(ENTRY,proc);

	/* set dsc to HEAD of inner scope */
	proc->dsc = topScope;
	closeScope();
	

}

/* parse function declaration */
procedure
void functionDeclaration(void)  {
	variable struct object *obj; variable struct object *typeObj;variable struct object *proc;
	variable struct typeDesc *retType; variable struct typeDesc *varType;
	variable int markSize; variable int lclBlkSize; variable int parBlkSize; variable int prototypeExists;
	variable char procid[100];
	obj = NULL;	proc = NULL; retType = NULL; varType = NULL; typeObj = NULL; 
	markSize = 8; lclBlkSize = 0; parBlkSize = 0; prototypeExists = 0; 

	if (currentSymbol == SYMBOL_INT) {
		retType = getType(intTypeIndex);
		getSymbol();	
	} elsif (currentSymbol == SYMBOL_FD) {
		retType = getType(fileTypeIndex);
		retType = getType(retType->refIndex);
		getSymbol();
	} elsif (currentSymbol == SYMBOL_CHAR) {
		retType = getType(charTypeIndex);
		getSymbol();
	} elsif (currentSymbol == SYMBOL_VOID) {
		getSymbol();
		retType = getType(voidTypeIndex);
	} elsif (currentSymbol == SYMBOL_BOOL) {
		getSymbol();
		retType = getType(boolTypeIndex);
	} elsif (currentSymbol == SYMBOL_STRUCT) { 
		getSymbol();
		if (currentSymbol == SYMBOL_IDENTIFIER) {
			typeObj = find();
			retType = getType(typeObj->typeIndex);
			retType = getType(retType->refIndex);
			getSymbol();
			if (currentSymbol == SYMBOL_MUL) {
				getSymbol(); 
			} else {
				mark("malformed return value");
				exit(-1);
			}
		} else {
			mark("missing identifier");
			exit(-1);
		}
	} else {
		mark("unkown return value");
		exit(-1);
	}
	if (currentSymbol == SYMBOL_IDENTIFIER) {
		getSymbol();
		proc = findProc(); /* check if procedure has a prototype */
		if (proc == guard) { /* we dont have a prototype */
			stringCopy(idName,procid);
			proc = newObj(CLASS_PROC);
			proc->retTypeIndex = retType->index;
			parBlkSize = markSize;
			incLevel(1);
			openScope();
			proc->val = -1; 
		} else { 
			prototypeExists = 1;
			topScope = proc->dsc;
			proc->dsc = topScope->next;
			parBlkSize = markSize;
			incLevel(1);
			if (proc->retTypeIndex != retType->index) {
				mark("return value mismatch in prototype/declaration");
				exit(-1);
			}
		}
	} else {
		mark("missing identifier in procedure declaration/prototype");
		exit(-1);
	}
	if (currentSymbol == SYMBOL_LPARAN) {
		getSymbol();
		parBlkSize = arguments(0,proc,parBlkSize); /* 0 indicates that we parse a declaration */
		lclBlkSize = parBlkSize;
		if (prototypeExists != 1) { /* calc. sizes in case no prototype exists */
			obj = topScope->next;
			while (obj != guard) {
				obj->lev = currentLevel;
				varType = getType(obj->typeIndex);
				lclBlkSize = lclBlkSize - varType->size;
				obj->val = lclBlkSize;
				obj = obj->next;
			}
			proc->dsc = topScope->next;
		}
		if (currentSymbol == SYMBOL_RPARAN) {
				getSymbol();
		} else {
			mark("missing right paranthesis in procedure declaration/prototype");
			exit(-1);
		}
	} else {
		mark("no valid argument definitions");
		exit(-1);
	}
	lclBlkSize = 0;
	lclBlkSize = functionBlock(proc,lclBlkSize, parBlkSize - markSize);
	ret(parBlkSize - markSize); 
	closeScope(); 
	incLevel(-1);
}

/* declaration := variableDeclaration | structDeclaration.*/
procedure
int declaration(int varSize) {
	variable struct object *obj;
	variable struct typeDesc *type;
	type = NULL; obj = NULL;
	/* structDeclaration */
	if (currentSymbol == SYMBOL_RECORD) { 
		getSymbol();
		structDeclaration();
	/* variableDeclaration */
	} elsif (currentSymbol == SYMBOL_VAR) {
		getSymbol();
		obj = variableDeclaration(CLASS_VAR);
		if (obj->cl != CLASS_CONST) {
			type = getType(obj->typeIndex);
			varSize = varSize + type->size;
			obj->val = -varSize;
			obj->lev = currentLevel;
		}
	}
	return varSize;
}

procedure 
void includeStatement(void) {
	if (currentSymbol == SYMBOL_STRINGLITERAL) {
			getSymbol();
	} else {
		mark("bad include");
		exit(-1);
	}
}

/* programm := { declaration | functionPrototypeDeclaration} functionDeclaration {functionDeclaration}. */
procedure
void program(void) {
	variable int varSize;
	varSize = 0;
    getSymbol();
    if (currentSymbol == SYMBOL_END) {
        mark("no declaration found");
        exit(-1);
    }

   while (currentSymbol == SYMBOL_HASHINCLUDE) {
		getSymbol();
		includeStatement();
	}

   while ((currentSymbol == SYMBOL_VAR) || (currentSymbol == SYMBOL_RECORD) || (currentSymbol == SYMBOL_PROT)) {
		if (currentSymbol == SYMBOL_PROT) {
			getSymbol();
			functionPrototype();
		} else {
			varSize = declaration(varSize);
		}
	}
	/* emit program header (for stack pointer initialization) */
	emitHeader(varSize);
	
	if (currentSymbol == SYMBOL_PROC) {
		getSymbol();
		functionDeclaration();
	} else {
		mark("at least one function required");
		exit(-1);
	}
	while (currentSymbol != SYMBOL_END) {
		if (currentSymbol == SYMBOL_PROC) {
            getSymbol();
			functionDeclaration();
        } else {
            mark("only function declarations allowed here");
            exit(-1);
        }
    }
}

procedure
void param(struct item *x) {
	if (currentSymbol == SYMBOL_LPARAN) {
		getSymbol();
	} else {
		mark("missing (");
		exit(-1);
	}
	if (currentSymbol == SYMBOL_STRINGLITERAL) {
		loadStringLiteralToHeap(x);
		getSymbol();
	} else {
		simpleExpression(x);
	}
	if (currentSymbol == SYMBOL_RPARAN) {
		getSymbol();
	} else {
		mark("missing right paranetheses");
		exit(-1);
	}
}

/* ---- end: parser ---- */
/* ---- begin: code generation ---- */

/* store one item into another, that is x := y */
procedure 
void store(struct item *x, struct item *y) {
	variable struct typeDesc *typeX; variable struct typeDesc *typeY;variable int r;
	variable int op;typeX = NULL; typeY = NULL; op = 0; 

	/* get the type of both items */
	typeX = getType(x->typeIndex); typeY = getType(y->typeIndex);

	/* type checking */
	if (((typeX->form == FORM_INT) || (typeX->form == FORM_CHAR) || 
		(typeX->form == FORM_REF) || (typeX->form == FORM_BOOL)) && (typeX->form == typeY->form)) {
		
		/* handle conditon on right hand side of assignment */
		if (y->mode == CLASS_COND) {
			op = selectOpNegated(y->c);
			put(op, y->r, 0, y->a); 
			regs[y->r] = 0; 
			y->a = pc - 1;
			fixLink(y->b); 
			r = getReg();
			y->r = r; 
			put(RISC_ADDI,y->r,0,1); 
			put(RISC_BEQ,0,0,2);
			fixLink(y->a); 
			put(RISC_ADDI,y->r,0,0);
		} elsif ((y->mode != CLASS_REG) && (y->mode != CLASS_PROC)) {
			load(y);
		}
		if (x->mode == CLASS_VAR) {
			if (x->lev == 0) {
				x->a = RISC_MEMSIZE + x->a;
			}
			if (x->hs != 0) { 
				put(RISC_BEQ,x->hs,0,3); 
				put(RISC_STW,y->r,x->r,x->a); 
				put(RISC_BEQ,0,0,2);
				put(RISC_HSTW,y->r,x->r,x->a); 
			} else {
				put(RISC_STW,y->r,x->r,x->a); 
			}
		} elsif (x->mode == CLASS_IND) { 
			put(RISC_HSTW,y->r,x->r,x->a);
		} else {
			mark("illegal assignment");
			exit(-1);
		}
		regs[x->hs] = 0;
		regs[y->r]  = 0;
		regs[x->r]  = 0;
	} else {
		mark("incompatible assignment");
		exit(-1);
	}	
}

/* check if symtab object represents parameter */
procedure 
bool isParameter(struct object *obj) {
	variable bool cond;
	cond = false;
	if ((obj->cl == CLASS_PAR) || ((obj->cl == CLASS_VAR) && (obj->val > 0))) {
		cond = true;
	}
	return cond;
}

/* load string literal into heap */
procedure 
void loadStringLiteralToHeap(struct item *x) {
	variable struct typeDesc *type; 
	variable int r; variable int i; variable int t;
	variable int length;
	type = NULL; r = 0;	i = 0; length = 0;

	/* get type and string length + 1 (for terminating  0) */
	type = getType(strLitIndex);
	length = stringLength(idName) + 1;
	type->len = length;
	x->typeIndex = type->index;	x->mode = CLASS_VAR; t = getReg(); x->r = t; x->a = 0; 
	
	/* allocate memory for string literal */
	put(RISC_ALL,x->r,0, type->len * wordSize); /* alocate memory for string param */
	r = getReg();

	/* store string literal on heap, be directly emitting instructions */
	while (idName[i] != '\0') { /* write string to heap */
		put(RISC_ADDI,r,0, idName[i]);
		put(RISC_HSTW,r,x->r,0);
		put(RISC_ADDI,x->r,x->r,wordSize);
		i = i + 1;
	}

	/* store terminating 0 and decrease address in x->r by length of string */
	put(RISC_HSTW,0,x->r,0); /* write terminating 0 */
	put(RISC_ADDI,x->r,x->r,wordSize);
	put(RISC_SUBI,x->r,x->r,type->len * wordSize);
	regs[r] = 0;
}

procedure 
struct object *parameter(struct object *fp) {
	variable int r; variable int i;
	variable bool cond;
	variable struct item *x;
	variable struct typeDesc *type;
	x = NULL; cond = false; type = NULL; r = 0; i = 0;
	allocMem(x);
	if (currentSymbol == SYMBOL_STRINGLITERAL) {
		loadStringLiteralToHeap(x);
		x->lev = fp->lev;
		getSymbol();
	} else {
		simpleExpression(x);
	}
	cond = isParameter(fp);
	if (cond == true) {
		emitParameter(x,fp->typeIndex,fp->cl);
		fp = fp->next;
	} else {
		mark("parameter error resulting from prototype or declaration");
		exit(-1);
	}
	return fp;
}

/* handle parameter passing */
procedure
void emitParameter(struct item *x, int fTypeIndex, int cl) {
	variable struct typeDesc *typeActual; variable struct typeDesc *typeFormal;
	variable int rA; variable int rB;
	rA = 0; rB = 0; typeActual = NULL; typeFormal = NULL;
	
	/* get type of actual and formal parameter */
	typeActual = getType(x->typeIndex);
	typeFormal = getType(fTypeIndex);

	/* cases: */
	/* (i) indices are identical */
	/* (ii) same form and base (arrays) */
	/* (iii) formal form is array, actual form is string literal */
	/* (iv) formal is int, actual is char (implicit casting) */

	if ((x->typeIndex == fTypeIndex) || 
		((typeActual->form == typeFormal->form) && (typeActual->base == typeActual->base)) || 
		((typeFormal->form == FORM_ARRAY) && (typeActual->form == FORM_STRLIT)) || 
		((x->typeIndex == charTypeIndex) && (fTypeIndex == intTypeIndex)))  {
		
		/* pass by value */	
		if (cl == CLASS_VAR) {
			if (x->mode != CLASS_REG) {
				load(x);
			}
				put(RISC_PSH,x->r,sp,4);
				regs[x->r] = 0;
		/* pass by reference */
		} elsif (cl == CLASS_PAR) {
			if ((x->mode == CLASS_VAR) && (typeActual->form == FORM_STRLIT)) {
				rA = x->r; 
			} elsif (x->mode == CLASS_VAR) {
				if (x->a != 0 ) {
					if (x->lev == 0) {
						x->a = RISC_MEMSIZE + x->a;
					}
					rA = getReg();
					if (typeActual->form == FORM_REF) {
						put(RISC_LDW,rA,x->r,x->a);
					} else { /* array */
						put(RISC_ADDI,rA,x->r,x->a);
					}
				} else {
					rA = x->r;
				}

			/* record references or arrays in records */
			} elsif (x->mode == CLASS_IND) {
				if (typeActual->form == FORM_ARRAY) {
					rB = getReg();
					put(RISC_ADDI,rB,0,0);
					put(RISC_PSH,rB,sp,4);
					put(RISC_ADDI,rB,0,typeActual->len);
					put(RISC_PSH, rB,sp,4);
					regs[rB] = 0;
					rA = x->r;
				} 
				else {
					rA = x->r;
					put(RISC_HLDW,rA,x->r,x->a);
				} 
			} else {
				mark("illegal parameter mode");
				exit(-1);
			}
		
			/* cases fr array parameter: */
			/* (i) if actual form is string literal, then addy will be on heap */
			/* (ii) if actual form is also array, then addy will be on stack */
			/* stack layout will be: isOnStack | length | addr (either heap or stack addy) */

			if ((typeFormal->form == FORM_ARRAY) && (x->mode != CLASS_IND)) {
				rB = getReg();
				if (typeActual->form == FORM_STRLIT) { 
					put(RISC_ADDI,rB,0,0);
				} else {
					if (x->hs != 0) {
						put(RISC_BEQ,x->hs,0,3); 
						put(RISC_ADDI,rB,0,1);
						put(RISC_BEQ,0,0,2);
						put(RISC_ADDI,rB,0,0);
					} else {
						put(RISC_ADDI,rB,0,1);
					}
				}
				/* push isOnStack */
				put(RISC_PSH,rB,sp,4);
		
				/* push length */
				if (x->hs != 0) { /* something is on stack */
					put(RISC_ADD,rB,0,x->alr);
					put(RISC_PSH, rB,sp,4);
				} else { /* local char array given as parameter */
					put(RISC_ADDI,rB,0,typeActual->len);
					put(RISC_PSH, rB,sp,4);
				}
				regs[x->alr] = 0;
				regs[x->hs] = 0;
				regs[rB] = 0;
			}
			/* push address */
			put(RISC_PSH,rA,sp,4);
			regs[rA] = 0;
		}
	} else {
		mark("bad parameter type");
		exit(-1);
	}
}

/* jump to specific position in code array */
procedure 
void call(struct item *x, int jumpTo) {
	put(RISC_BSR, 0,0,jumpTo);
}

/* handle supported IO procedures - fix type checking */
procedure 
void IOCall(struct item *x, struct item *y) {
	variable struct typeDesc *type;
	variable int r; r = 0;
	type = getType(y->typeIndex);
	/* writeChar */
	if (x->a == 0) {
		if (type->index != charTypeIndex) {
			mark("not a char in writeChar");
			exit(-1);
		}
		if (y->mode != CLASS_REG) {
			load(y);
		}
		put(RISC_WRC,0,0,y->r);
		regs[y->r] = 0;

	/* writeInt */
	} elsif (x->a == 1) {
		if (type->index != intTypeIndex) {
			mark("not an int in writeInt");
			exit(-1);
		}
		if (y->mode != CLASS_REG) {
			load(y);
		}
		put(RISC_WRI,0,0,y->r);
		regs[y->r]=0;

	/* handle fopen */
	} elsif (x->a == 2) {
		put(RISC_ALL,rv,0,wordSize); 
		r = getReg();

		/* if form is string literal, then addy is on heap */
		if (type->form == FORM_STRLIT) {
			put(RISC_HFOPEN,r,y->r,y->a);
		} elsif (type->form == FORM_ARRAY) {
			if (y->hs != 0) {
				r = getReg();
				put(RISC_BEQ,y->hs,0,3); 
				put(RISC_SFOPEN,r,y->r,y->a);
				put(RISC_BEQ,0,0,2);
				put(RISC_HFOPEN,r,y->r,y->a);
				regs[x->hs] = 0;
			} else {
				put(RISC_SFOPEN,r,y->r,y->a);
			}
		} else {
			mark("bad type in fopen");
			exit(-1);
		}
		put(RISC_HSTW,r,rv,0);
		regs[y->r] = 0;
		regs[r] = 0;

	/* handle fclose */
	} elsif (x->a == 3) {
		type = getType(fileTypeIndex);
		if (type->refIndex != y->typeIndex) {
			mark("not a file type in fclose");
			exit(-1);

		/* load fd in register and close (fd is on heap) */
		} else {
			r = getReg();
			put(RISC_LDW,r,y->r,y->a);
			put(RISC_HLDW,r,r,0);
			put(RISC_FCLOSE,rv,0,r);
			regs[r] = 0;
		}
	
	/* handle feof */
	} elsif (x->a == 4) {
		type = getType(fileTypeIndex);
		if (type->refIndex != y->typeIndex) {
			mark("not a file type in feof");
			exit(-1);
		} else {
			if (y->mode != CLASS_REG) {
				load(y);
			}
			r = y->r;
			put(RISC_HLDW,r,r,0);
			put(RISC_FEOF,rv,0,r);
			regs[r] = 0;
		}

	/* handle fgetc */
	} elsif (x->a == 5) {
		type = getType(fileTypeIndex);
		if (type->refIndex != y->typeIndex) {
			mark("not a file type in feof");
			exit(-1);
		} else {
			if (y->mode != CLASS_REG) {
				load(y);
			}
			r = y->r;
			put(RISC_HLDW,r,r,0);
			put(RISC_FGETC,rv,0,r);
			regs[r] = 0;
		}
	} elsif (x->a == 6) {
		if (type->index != charTypeIndex) {
			mark("ord only for char");
			exit(-1);
		} else {
			if (y->mode != CLASS_REG) {
				load(y);
			}
			r = y->r;
			put(RISC_ORD,rv,0,r);
			y->typeIndex = intTypeIndex;
			regs[r] = 0;
		}
	}
}

/* increment level - here: global or local - no nested procedures */
procedure 
void incLevel(int n) {
	currentLevel = currentLevel + n;
}

/* procedure prolog - save sp, fp, etc. */
procedure
void enter(int size) {
	put(RISC_PSH, lnk, sp, 4);
	put(RISC_PSH, fp, sp, 4);
	put(RISC_ADD, fp, 0, sp);
	put(RISC_SUBI, sp, sp, size);
}

/* procedure exit point - restore fp,sp etc. */
procedure
void ret(int size) {
	put(RISC_ADD, sp, 0, fp);
	put(RISC_POP, fp, sp, 4);
	put(RISC_POP, lnk, sp, size + 4);
	put(RISC_RET, 0,0,lnk);
}

procedure
struct instrStruct *getInstruction(int pos) {
	variable struct instrStruct *element;
	variable int i;
	i=0;
	element = firstInstr->next;
	while (i < pos) {
		element = element->next;
		i = i + 1;
	}
	return element;
}

/* correct jumps to procedures */
procedure 
void procFix(void) {
	variable struct procFixStruct *list;
	list = procFixList->next;
	while (list != NULL) {
		if (list->proc->val < 0) {
			mark("found lonley prototype");
			exit(-1);
		} else {
			fix(list->pos,list->proc->val - list->pos);
		}
		list = list->next;
	}
}

/* enter special IO procedures in universe */
procedure 
void enterSproc(int retTypeIndex, int n, char name[]) {
	variable struct object *obj;
	obj=NULL;
	allocMem(obj);
	obj->cl = CLASS_SPROC; obj->val = n; obj->dsc = NULL; obj->typeIndex = 0; obj->next = topScope->next;
	obj->retTypeIndex = retTypeIndex;
	stringCopy(name,obj->name);
	topScope->next = obj;
}

/* open a new scope when entering a procedure */
procedure 
void openScope(void) {
	variable struct object *s;
	allocMem(s);
	s->cl = CLASS_HEAD; s->dsc = topScope; s->next = guard; topScope = s;
}

/* close current scope and return to level-1 */
procedure
void closeScope(void) {
	topScope = topScope->dsc;
}

/* enter a new type into the global type list */
procedure 
struct typeDesc * newType(void)  {
	variable struct typeDesc *type;
	variable struct typeDesc *it;
	type = NULL; it = NULL;
	allocMem(type);
	it = typeList;
	while (it->next != NULL) { 
		it = it->next; 
	}
	type->next = it->next; type->index = it->index + 1;
	it->next = type;
	return type;
}

/* get a type from the type list, based on its type index */
procedure 
struct typeDesc *getType(int index)  {
	variable bool cond;
	variable struct typeDesc *type;
	type = typeList;
	cond = true;
	while ((type != NULL) &&  (cond == true)) {
		if (type->index == index) {
			cond = false;
		}
		if (cond == true) {
			type = type->next;
		}
	}
	if (type == NULL) {
		mark("type undefined");
		exit(-1);
	}
	return type;
}

/* create a new anonymous object in symbol table (comes from prototype */
procedure
struct object * newAnonymousObj(int cl) {
	variable struct object *newObj; variable struct object *x;
	newObj=NULL; x=topScope;
	while (x->next != guard) { 
		x = x->next; 
	}
	allocMem(newObj);
	newObj->cl = cl; newObj->next = guard; x->next = newObj;
	return newObj;
}

/* create a new object in the symbol table */
procedure
struct object *newObj(int cl) {
	variable int ret;
	variable struct object *newObject; variable struct object *x;
	ret = 0; x = topScope;
	stringCopy(idName,guard->name);
	ret = stringCompare(idName,x->next->name);
	while (ret != 0) {
		x = x->next;
		ret = stringCompare(idName,x->next->name);
	}
	if (x->next == guard) {
		allocMem(newObject);
		newObject->cl = cl;	newObject->next = guard; x->next = newObject;
		stringCopy(idName,newObject->name);	
		return newObject;
	} else {
		mark("multiple definition");
		exit(-1);
	}
}

/* calculate x to the power of n */
procedure 
int cPow(int x, int n) {
	variable int i; variable int r;
	r = 1; i = 0;
	while (i < n) {
		r = r * x;
		i = i + 1;
	}
	return r;
}

/* add instruction */
procedure 
void addInstruction(int instr) {
	variable struct instrStruct *newInstr;
	allocMem(newInstr);
	newInstr->instr = instr;
	newInstr->next = lastInstr->next;
	lastInstr->next = newInstr;
	lastInstr = newInstr;
}

/* encode instruction into integer and store it in code array */
procedure
void put(int op,int a, int b, int c) {
	variable int instr;
	instr=0;
	if (op >= 32) {	
		op = op - 64; 
	}
	if (c < 0) { 
		c = bigNum + c; 
	} else { 
		c = c % bigNum; 
	}

	instr = op * cPow(2,5) + a; 
	instr = instr * cPow(2,5) + b; 
	instr = instr * cPow(2,16) + c;
	addInstruction(instr);
	pc = pc + 1;
}

procedure
void putOp(int cd, struct item *x, struct item *y) {
	variable int r;

	/* if x is not in register, load it; then x->r holds the register the value is in */
	if (x->mode != CLASS_REG) { 
		load(x);
	}

	/* const. 0 at lh, e.g. 0 + x => use x->r to store value */
	if (x->r == 0) { 
		r = getReg();
		x->r = r; 

		/*  r[x] = r[0] + r[1] */
		r = 0; 
	} else {

		/* set r to the register the value is in */
		r = x->r; 
	}

	/* constants require immediate instructions (ADDI,CMPI,...) */
	if (y->mode == CLASS_CONST)  {
		put(cd + 16,r,x->r,y->a); 
	} else {
		if (y->mode != CLASS_REG) {
			load(y);
		}
		put(cd,x->r,r,y->r);
		regs[y->r] = 0;
	}
}

procedure
void loadBool(struct item *x) {
	variable struct typeDesc *type;
	type = getType(x->typeIndex);
	if (type->form != FORM_BOOL){
		mark("not bool");
		exit(-1);
	}
	load(x);
	x->mode = CLASS_COND;
	x->a = 0; x->b = 0; x->c = 1;
}

/* load item into register */
procedure 
void load(struct item *x) {
	variable int r;
	r = 0;
	
	/* load items based on their stack addr. */
	if (x->mode == CLASS_VAR) {
		if (x->lev == 0) {
			x->a = RISC_MEMSIZE + x->a;
		}

		/* cases: */
		/* (i) R[x->hs] == 0 => load addr. from heap */
		/* (ii) R[x->hs] == 1 => load addr. from stack */
		if (x->hs != 0) {
			r = getReg();
			put(RISC_BEQ,x->hs,0,3); 
			put(RISC_LDW,r,x->r,x->a); 
			put(RISC_BEQ,0,0,2);
			put(RISC_HLDW,r,x->r,x->a); 
			regs[x->hs] = 0;

		/* simply load variable into R[r] */
		} else {
			r = getReg();
			put(RISC_LDW,r,x->r,x->a);
		}
		regs[x->r] = 0;
		x->r = r;

	 /* record elements reside on heap */
	} elsif (x->mode == CLASS_IND) {
		if ((x->lev == 0) && (x->indLev == 0)) {
			x->a = RISC_MEMSIZE + x->a;	
		}
		r = getReg();
		put(RISC_HLDW,r,x->r,x->a);
		regs[x->r] = 0;
		x->r = r;

	/* constant stuff can be stored into R[r] */
	} elsif (x->mode == CLASS_CONST) {
		if (x->a == 0) {
			x->r = 0;
		} else {
			r = getReg();
			x->r = r;
			put(RISC_ADDI,x->r,0,x->a);
		}
	}
	x->mode = CLASS_REG;
}

/* conditional jump */
procedure
void cJump(struct item *x) {
	variable int riscOp;
	variable struct typeDesc *type;
	type = NULL;
	riscOp = 0;
	type = getType(x->typeIndex);
	if (type->form == FORM_BOOL) {
		if (x->mode != CLASS_COND) {
			loadBool(x);
		}
		riscOp = selectOpNegated(x->c);
		put(riscOp,x->r,0,x->a);
		regs[x->r] = 0;
		fixLink(x->b);
		x->a = pc - 1;
	} else {
		mark("boolean ?");
		exit(-1);
	}
}

/* forward jump */
procedure 
int fJump(int L) {
	put(RISC_BEQ,0,0,L); L = pc -1;
	return L;
}

/* backward jump */
procedure 
void bJump(int L) {
	put(RISC_BEQ,0,0,L-pc);
}

/* fix instruction at position "at" with the value in given by "with" */
procedure
void fix(int at, int with) {
	variable struct instrStruct *element;
	element = getInstruction(at);
	element->instr = ((element->instr / bigNum) * bigNum) + (with % bigNum);
}

procedure 
void fixLink(int L) {
	variable int La;
	variable struct instrStruct *element;
	while (L != 0) {
		element = getInstruction(L);
		La = element->instr % bigNum; if (La < 0) { La = bigNum + La; } fix(L,pc-L);  L = La;
	}
}

/* initialize stack pointer */
procedure
void emitHeader(int size) {
	put(RISC_ADDI,sp,0,RISC_MEMSIZE - size - 16); /* -16 because of int argc (4), char argv[] (12) */
	put(RISC_PSH,1,sp,4);
	put(RISC_PSH,2,sp,4);
	put(RISC_PSH,3,sp,4);
	put(RISC_PSH,4,sp,4);
	put(RISC_BSR,0,0,0); /* will be fixed with entry point */
}

procedure
void makeConstItem(struct item *x, int typeIndex, int val) {
	x->mode = CLASS_CONST;
	x->typeIndex = typeIndex; x->a = val; x->b = 0; x->c = 0; x->idx = 0; x->lev = 0; x->r=0;
}

/* make item x based on object y properties */
procedure 
void makeItem(struct item *x, struct object *y) {
	variable struct typeDesc *type;
	variable int r;
	r = 0; type =  NULL;
	x->mode = y->cl; x->typeIndex = y->typeIndex; x->lev = y->lev; x->a = y->val; 

	/* in case of proc or sproc, x->r is return value register */
	if ((y->cl == CLASS_PROC) || (y->cl == CLASS_SPROC)) {
		x->typeIndex = y->retTypeIndex;
		x->r = rv;
	} else {
		if (y->lev == 0) { 
			x->r = 0; 
		} elsif (y->lev == currentLevel) {
			x->r = fp;
		} else {
			mark("level");
			exit(-1);
		}
	}
	
	/* load paramater object */
	if (y->cl == CLASS_PAR) {
		type = getType(y->typeIndex);

		/* in case of array: first load length (+4) | isOnStack (+8) */
		if (type->form == FORM_ARRAY) {
			r = getReg();
			put(RISC_LDW,r,x->r,x->a + 4); 
			x->alr = r;
			r = getReg();
			put(RISC_LDW,r,x->r,x->a + 8); 
			x->hs = r;
			r = getReg();
			put(RISC_LDW,r,x->r,x->a);
			x->mode = CLASS_VAR; x->r = r; x->a = 0; 
		} else {
			x->mode = CLASS_VAR;
		}
		
	}
}

/* get the inverse operator to the given symbol, eg.: BEQ->BNE */
procedure
int selectOpNegated(int c) {
	variable int riscOp;
	riscOp=0;
	if (c == SYMBOL_EQUALTEST) {
		riscOp = RISC_BNE;
	} elsif (c == SYMBOL_LESS) {
		riscOp = RISC_BGE;
	} elsif (c == SYMBOL_GREATER) {
		riscOp = RISC_BLE;
	} elsif (c == SYMBOL_LEQ) {
		riscOp = RISC_BGT;
	} elsif (c == SYMBOL_GEQ) {
		riscOp = RISC_BLT;
	} elsif (c == SYMBOL_NOT) {
		riscOp = RISC_BEQ;
	}
	return riscOp;
}

/* get operator based on given symbol */
procedure
int selectOp(int c) {
	variable int riscOp;
	riscOp=0;
	if (c == SYMBOL_EQUALTEST) {
		riscOp = RISC_BEQ;
	} elsif (c == SYMBOL_LESS) {
		riscOp = RISC_BLT;
	} elsif (c == SYMBOL_GREATER) {
		riscOp = RISC_BGT;
	} elsif (c == SYMBOL_LEQ) {
		riscOp = RISC_BLE;
	} elsif (c == SYMBOL_GEQ) {
		riscOp = RISC_BGE;
	} elsif (c == SYMBOL_NOT) {
		riscOp = RISC_BNE;
	}
	return riscOp;
}

procedure 
int merged(int lA, int lB) {
	variable int lC; variable int lD;
	variable struct instrStruct *element;
	lC = 0; lD = 0;
	if (lA != 0) {
		lC = lA;
		element = getInstruction(lC);
		lD = element->instr % bigNum;
		while (lD != 0) {
			lC = lD;
			element = getInstruction(lC);
			lD = element->instr % bigNum;
		}
		element->instr = element->instr - lD + lB;
		return lA;
	}
	return lB;
}

procedure 
void opOne(int op, struct item *x) {
	variable int riscOp;
	variable struct typeDesc *type;
	type = NULL; riscOp = 0;
	if (op == SYMBOL_SUB) {
		type = getType(x->typeIndex);
		if (type->form != FORM_INT) {
			mark("not an integer");
			exit(-1);

		/* constant value can be immediately transformed */
		} elsif (x->mode == CLASS_CONST) {
			x->a = -x->a;

		/* variable has to be loaded and negated by R[x->r] = R[0] - R[x->r] */
		} else {
			if ((x->mode == CLASS_VAR) || (x->mode == CLASS_IND)) {
				load(x);
			}
			put(RISC_SUB,x->r,0,x->r);
		}
	} elsif (op == SYMBOL_AND) {
		if (x->mode != CLASS_COND) {
			loadBool(x);
		}
		riscOp = selectOpNegated(x->c);
		put(riscOp,x->r,0,x->a);
		regs[x->r] = 0;
		x->a = pc - 1;
		fixLink(x->b);
		x->b = 0;
	} elsif (op == SYMBOL_OR) {
		if (x->mode != CLASS_COND) {
			loadBool(x);
		}
		riscOp = selectOp(x->c);
 		put(riscOp,x->r,0,x->b);
		regs[x->r] = 0;
		x->b = pc - 1;
		fixLink(x->a);
		x->a = 0;
	}
}

procedure
void opTwo(int op, struct item *x, struct item *y) {	
	variable struct typeDesc *typeX;
	variable struct typeDesc *typeY;
	typeX = NULL; typeY = NULL;
	typeX = getType(x->typeIndex);
	typeY = getType(y->typeIndex);
	if ((typeX->form == FORM_INT) && (typeY->form == FORM_INT)) {
		if ((x->mode == CLASS_CONST) && (y->mode == CLASS_CONST)) {
			if (op == SYMBOL_ADD) {
				x->a = x->a + y->a;
			} elsif (op == SYMBOL_SUB) {
				x->a = x->a - y->a;
			} elsif (op == SYMBOL_MUL) {
				x->a = x->a * y->a;
			} elsif (op == SYMBOL_DIV) {
				x->a = x->a / y->a;
			} elsif (op == SYMBOL_MOD) {
				x->a = x->a % y->a;
			}
		} else {
			if (op == SYMBOL_ADD) {
				putOp(RISC_ADD,x,y);
			} elsif (op == SYMBOL_SUB) {
				putOp(RISC_SUB,x,y);
			} elsif (op == SYMBOL_MUL) {
				putOp(RISC_MUL,x,y);
			} elsif (op == SYMBOL_DIV) {			
				putOp(RISC_DIV,x,y);
			} elsif (op == SYMBOL_MOD) {
				putOp(RISC_MOD,x,y);
			}
		}
	} elsif ((typeX->form  == FORM_BOOL) && (typeY->form == FORM_BOOL)) {
		if (y->mode != CLASS_COND) {
			loadBool(y);
		}
		if (op == SYMBOL_OR) {
			x->a = y->a;
			x->b = merged(y->b,x->b);
			x->c = y->c;
		} elsif (op == SYMBOL_AND) {
			x->a = merged(y->a,x->a);
			x->b = y->b;
			x->c = y->c;
		} else {
			mark("bad type");
			exit(-1);
		}
	}
}

/* load index of array variable */
procedure 
void idx(struct item *x, struct item *y) {
	variable int r;
	variable struct typeDesc *type;
	type=NULL; r = 0;
	if (y->typeIndex != intTypeIndex) {
		mark("index not an integer");
		exit(-1);
	}
	type = getType(x->typeIndex);

	 /* array is given as parameter */
	if (x->alr != 0) {
		if (y->mode == CLASS_CONST) {
			r = getReg();
			put(RISC_ADDI,r,0,y->a); 
			put(RISC_CHK,r,0,x->alr); 

			put(RISC_MULI,r,r,type->base->size);
			if (x->r != 0) { 
			
				/* R[r] := FP/global + R[r] */
				put(RISC_ADD,r,x->r,r); 
				regs[x->r] = 0;
			}
			x->r = r;
			regs[x->alr] = 0;
		} else {
			if (y->mode != CLASS_REG) {
				load(y);
			}
			put(RISC_CHK,y->r,0,x->alr);
			put(RISC_MULI,y->r,y->r,type->base->size);
			if (x->r != 0) {
				put(RISC_ADD,y->r,x->r,y->r);
				regs[x->r] = 0;
			}
			x->r = y->r;
			regs[x->alr] = 0;
		}
	} else {
		if (y->mode == CLASS_CONST) {
			if ((y->a < 0) || (y->a >= type->len)) {
				mark("index out of range");
				exit(-1);
			}
			x->a = x->a + y->a * type->base->size; /* fix */
		} else {
			if (y->mode != CLASS_REG) {
				load(y);
			}
			put(RISC_CHKI,y->r,0,type->len);
			put(RISC_MULI,y->r,y->r,type->base->size);
			if (x->r != 0) {
				put(RISC_ADD,y->r,x->r,y->r);
				regs[x->r] = 0;
			}
			x->r = y->r;
		}
	}
	x->typeIndex = type->base->index;
}

/* check reference against NULL */
procedure
void nullRelation(int op, struct item *x) {
	variable struct item *y;
	variable struct typeDesc *type;
	type = NULL; y = NULL;
	allocMem(y);
	type = getType(x->typeIndex);
	if (type->form != FORM_REF) {
		mark("reference required in relation");
		exit(-1);
	}
	y->mode = CLASS_REG; y->r = 0;
	putOp(RISC_CMP,x,y);
	x->c = op; x->mode = CLASS_COND; x->typeIndex = boolTypeIndex; x->a=0; x->b=0;
}


/* emit op codes for relations */
procedure
void relation(int op, struct item *x, struct item *y) {
	variable struct typeDesc *typeX; variable struct typeDesc *typeY;

	/* get types of both items */
	typeX = getType(x->typeIndex); typeY = getType(y->typeIndex);

	/* enter if both items are neither int nor ref */
	if (((x->typeIndex != intTypeIndex) || (y->typeIndex != intTypeIndex)) &&
		((typeX->form != FORM_REF) || (typeY->form != FORM_REF))) { 
		
		/* if items represent boolean stuff, ops are limited to == and != */
		if ((x->typeIndex == boolTypeIndex) && (y->typeIndex == boolTypeIndex)) { 
			if ((op == SYMBOL_EQUALTEST) || (op == SYMBOL_NOT)) {
				putOp(RISC_CMP,x,y);
				regs[y->r] = 0;
				x->c = op;
			} else {
				mark("bad relation for bool");
				exit(-1);
			}

		/* items might also represent chars */
		} elsif((x->typeIndex == charTypeIndex) && (y->typeIndex == charTypeIndex)) {
			putOp(RISC_CMP,x,y);
			regs[y->r] = 0;
			x->c = op;
		} else {
			mark("bad type in relation");
			exit(-1);
		}
	} else {
		if (typeX->form == FORM_REF) { /* check if reference */
			if ((op != SYMBOL_EQUALTEST) && (op != SYMBOL_NOT)) { /* only == or != allowed */
				mark("bad relation of reference type");
				exit(-1);
			}
		}
		if ((y->mode == CLASS_CONST) && (y->a == 0)) {
			load(x);
		} else {
			putOp(RISC_CMP,x,y);
			regs[y->r] = 0;
		}
		x->c = op;
	}
	x->mode = CLASS_COND;
	x->typeIndex = boolTypeIndex;
	x->a = 0; x->b = 0;
}

/* start compilation */
procedure
void compile(void) {
	parser(); /* call parser */
}

/* main procedure - only one commandline argument allowed */
procedure 
int main(int argc, char arg[]) {
	variable int i;
	variable struct instrStruct *element;
	i=0;
	if (argc != 2) {
	   mark("argument error");
	   exit(-1);
	}
	globalInit();
	inputFile = fopen(arg);
	compile();

	element = firstInstr->next;
	while (element != NULL) {
		writeInt(element->instr);
		writeChar('\n');
		element = element->next;
	}
} 

