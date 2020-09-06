/*
	SDT Parser for PL0 Language
*/
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sample_interpreterNew.c"

void yyerror(char*);
int yylex();

#define CONST 	0
#define VAR 	1
#define PROC 	2
#define IDENT 	3  /* CONST + VAR */
#define FUNCT	4
#define ARG		5
#define TBSIZE 100	// symbol table size
#define LVLmax 20		// max. level depth

// global variables from interpreter.c 
// extern struct Instruction;
// extern Instruction Code[];


// symbol table
struct {
	char name[20];
	int type;		/* 0-CONST	1-VARIABLE	2-PROCEDURE */
	int lvl;
	int offst;
	int link;
	} table[TBSIZE];

struct {
	char lab[10];
	int address;
} labelOffst[cxmax];

 int labelOffstIndx=0;
// int labelOffstVal[cxmax];
// char labelOffst[cxmax][10];

int hashBucket[TBSIZE];		// Define a Hash Bucket
int block[LVLmax]; // Data for Set/Reset symbol table

int Lookup(char *, int);
void Enter(char *, int, int, int);
void SetBlock();
void ResetBlock();
void DisplayTable();
void GenLab(char *);
void EmitLab(char *);
void Emit1(char *, int, int);
void Emit2(char *, int, char *);
void Emit3(char *, char *);
void Emit(char *);

void EmitErr(char *, char *);
void assignOffst();
void printBinry();
void printResult();

int ln=1, cp=0;
int hbIndex=0;
int lookedupLev=0; int tx=0, level=0;
int LDiff=0, Lno=0, OFFSET=0;
char Lname[10];

int instrNum=0;
int numOfVar=0;

int procNum=0;
// int varOffst=3;
// int numOfCallArg=0;

%}

%union {
	char ident[50];	// id lvalue
	int number;	// num lvalue
	}
%token TCONST TVAR TPROC TFUNC TCALL TBEGIN TIF TTHEN TWHILE TDO TEND ODD NE LE GE AND ASSIGN ERROR TRETURN
%token <ident> ID 
%token <number> NUM
%type <number> Dcl VarDcl Ident_list ProcHead ArgDef_list ArgSnd LookArg_list // ArgDef_list ArgDcl ProcDef_list ProcDef 
%left '+' '-'
%left '*' '/'
%left UM

%%
Program: Block '.'  { Emit("END"); printf(" ==== valid syntax ====\n" );
						assignOffst(); printBinry(); interprete(); printResult(); } 
		;

Block:  { GenLab(Lname); Emit3("JMP", strcpy($<ident>$, Lname)); }	
	
	Dcl  { EmitLab($<ident>1); Emit1("INT", 0, $2); }	
	
	Statement  { DisplayTable(); } 
	;
	
FuncBlock: { GenLab(Lname); Emit3("JMP", strcpy($<ident>$, Lname)); }	
	
		Dcl  {  EmitLab($<ident>1); Emit1("INT", 0, $2); }	
	
		Statement  { DisplayTable(); }  

		TRETURN Expression
		;	

Dcl: ConstDcl VarDcl ProcDef_list FuncDef_list	{ $$=$2; } ;	

ConstDcl:
	| TCONST Constdef_list ';' 
	;

Constdef_list: Constdef_list ',' ID '=' NUM 	{ Enter($3, CONST, level, $5); }
	| ID '=' NUM 	 { Enter($1, CONST, level, $3); }
	;

VarDcl: TVAR Ident_list ';'	{ $$=$2; }
	|		{ $$=3; } 
	;

Ident_list: Ident_list ',' ID	{ if(level==0){ numOfVar++; } Enter($3, VAR, level, $1); $$=$1+1; }	// symbol size = 1. check duplicate in current block
	| ID 		{  if(level==0){ numOfVar++; } Enter($1, VAR, level, 3); $$=4; }  
	;

ProcDef_list: ProcDef_list ProcDef 
	| 	 
	;

ProcDef: ProcHead  Block ';' { Emit("RET"); ResetBlock(); }
		;

ProcHead: TPROC ID ';' { Enter($2, PROC, level, 1); EmitLab($2); SetBlock(); }  
		;


FuncDef_list: FuncDef_list FuncDef 
	| 
	;

FuncDef: FuncHead FuncBlock ';' { Emit("FRT"); ResetBlock(); }
		// | { /***/printf("Fail to parse in 'Function Definition'\n"); }
		;

FuncHead: TFUNC ID '(' ArgDcl ')' ';' { Enter($2, FUNCT, level, 1); EmitLab($2); SetBlock(); }
		// | { /***/printf("Fail to parse in 'Function Head'\n"); }
		;	

ArgDcl: ArgDef_list 
	|	
	;

ArgDef_list: ArgDef_list ',' ID { Enter($3, ARG, level, $1); $$=$1+1; } 
		| 	ID { Enter($1, ARG, level, 0); $$=1; }
		;


Statement: ID ASSIGN Expression  { if( Lookup($1, VAR)==1 ) { Emit1("STO", LDiff, OFFSET); }
			        else {EmitErr("Fail to Lookup Variable\n", $1);} /* error: undefined var */ ; }
	| TCALL ID		{ if(Lookup($2, PROC)) Emit2("CAL", LDiff, $2); 	// 비지역변수 참조를 위해서는 level이 필요
			  else  EmitErr("Fail to Lookup Procedure\n", $2); /* error: undefined proc */ ; }
	// | ID ASSIGN Function { if(Lookup($3, )) }
	| TBEGIN Statement_list TEND

	// control flow를 다루는 두 파트.
	| TIF Condition 		{ GenLab(Lname); Emit3("JPC", strcpy($<ident>$, Lname));  }		// if가 true면 실행, false면 jump하는 instruction 필요
		TTHEN Statement	 { EmitLab($<ident>3); }	//	block label을 전역변수로 쓰려면 stack 형식으로 써라. 아니면 중첩 if문에서 문제 발생.

	| TWHILE 		{ GenLab(Lname); EmitLab(strcpy($<ident>$, Lname));  }	
		Condition 	{ GenLab(Lname); Emit3("JPC", strcpy($<ident>$, Lname));  		}
		TDO Statement 	{ Emit3("JMP", $<ident>2);  EmitLab($<ident>4); 	}
	| 
	;

Statement_list: Statement_list ';' Statement
	| Statement 
	// | { /***/printf("Fail to parse in 'Statement list'\n"); }
	;

Condition: ODD Expression			{ Emit("ODD"); }		// Intermediate Language ppt 19p. 대로 만들자.
	| Expression AND Expression		{ Emit("AND"); }
	| Expression '=' Expression		{ Emit("EQ"); }
	| Expression NE Expression		{ Emit("NE"); }
	| Expression '<' Expression		{ Emit("LT"); }
	| Expression '>' Expression		{ Emit("GT"); }
	| Expression GE Expression		{ Emit("GE"); }
	| Expression LE Expression		{ Emit("LE"); }  
	// | { /***/printf("Fail to parse in 'Condition'\n"); }
	;

Expression: Expression '+' Term		{ Emit("ADD"); }
	| Expression '-' Term		{ Emit("SUB"); }
	| '+' Term %prec UM
	| '-' Term %prec UM		{Emit("NEG"); }
	| Term 
	// | { /***/printf("Fail to parse in 'Expression'\n"); }
	;

Term: Term '*' Factor			{ Emit("MUL"); }
	| Term '/' Factor			{ Emit("DIV"); }
	| Factor 
	// | { /***/printf("Fail to parse in 'Term'\n"); }
	;

Factor: ID			{ if(Lookup($1, IDENT)==1) { Emit1("LOD",LDiff, OFFSET); } 		// if it's Var, Load the value of var. 
						else if(Lookup($1, IDENT)==2){ Emit1("LIT", 0, OFFSET); }	// if it's Const, Lit the number. 
						else if(Lookup($1, ARG)==1){ Emit1("ALD", LDiff, OFFSET); }		// if it's Argument, Load the Arg.
						// /***/else if(Lookup($1, FUNCT)==1){ Emit2("FCL", LDiff, $1); }
						else { EmitErr("Fail to Lookup Identity\n", $1); } }
	| NUM			{ Emit1("LIT", 0, $1); }
	| '(' Expression ')' 
	| ID '(' ArgSnd ')' { if(Lookup($1, FUNCT)==1){ Emit1("LIT", 0, $3); Emit2("FCL", LDiff, $1); } }
	// | { /***/printf("Fail to parse in 'Factor'\n"); }
	;

ArgSnd: LookArg_list { $$=$1; }
	| 	{ $$=0; }
	;

LookArg_list: LookArg_list ',' Expression { $$=$1+1; }	// Use 'Factor' rather than 'Expression' because of the stack 
			| Expression { $$=1; }
			;
	
%%
#include "lex.yy.c"
#include <string.h>
void yyerror(char* s) {
	printf("line: %d cp: %d %s\n", ln, cp, s);
}

int Lookup(char *name, int type) { 
	int hbIndex = TBSIZE % (name[0]-'0');
	int index = hashBucket[hbIndex];
	
	while(strcmp(table[index].name, name) != 0){	// the result is not the same char*
		if(table[index].link == -1){
			return 0;
		}
		index = table[index].link;
	}

	if(type == IDENT){
		if( table[index].type==VAR){
			LDiff = level - table[index].lvl;
			OFFSET = table[index].offst;
			return 1;
		} else if(table[index].type==CONST){
			OFFSET = table[index].offst;
			return 2;	
		}
	}

	if(table[index].type == type){
		LDiff = level - table[index].lvl;
		OFFSET = table[index].offst;
		return 1;
	} else {
		return 0;
	}
}

void Enter(char *name, int type, int lvl, int offst) {
	int hbIndex = TBSIZE % (name[0]-'0');
	int index = tx;
	int preHashIndex = -1;

	if(hashBucket[hbIndex] != -1){
		preHashIndex = hashBucket[hbIndex];
	}

	while(table[index].name[0] != -1){
		if(index < TBSIZE){
			index++;
		} else {
			index=0;
		}
	}

	strcpy(table[index].name, name);
	table[index].type = type;
	table[index].lvl =lvl;
	table[index].offst = offst;
	table[index].link = preHashIndex;
	hashBucket[hbIndex] = index;
	if(index < TBSIZE){
			tx = index+1;
			block[level] = tx;
		} else {
			printf("the hash is full\n");
		}
}

void SetBlock() {		//no problem.
	block[level++]=tx;
	/***/printf("Setblock: level=%d,  table index=%d\n", level, tx);
}

void ResetBlock() { 

	int hb = TBSIZE;
	int isHashDelete;

	level--;

	// for(int i=0; i<5; i++){
	// 	/***/printf("before enter while in ResetBlock. block[i] = %d, i = %d\n", block[i], i);
	// }

	while(--hb>=0){						// check every hash bucket
		if(hashBucket[hb] != -1){		// check valid hash bucket
			int deletedItem, tmp = hashBucket[hb];
			isHashDelete=0;

			// printf("hash check-- hash[%d] %d block[level]=%d\n", hb, hashBucket[hb], block[level]);
			while(tmp>=block[level]){				// find the backward chained items in this block
				deletedItem = tmp;
				tmp = table[tmp].link;				// save the next linked item

				table[deletedItem].name[0]=-1;		// reset the name value	
				table[deletedItem].link=-1;			// reset the link value
			}

			hashBucket[hb] = tmp;
		}
	}

	tx=block[level];
	/***/printf("Resetblock: level=%d,  table index=%d\n", level, tx);
}

void DisplayTable() { 
	int idx, hb;
	if(tx==0){
		idx = TBSIZE;
	} else {
		idx=tx;
	}
	hb=TBSIZE;

	printf("\n====== Hash Bucket ======\n");

	while(--hb>=0){
		if(hashBucket[hb] != -1)
			/***/printf("%d Hash Bucket = %d\n", hb, hashBucket[hb]);
	}

	printf("\n====== Symbol Table ======\n");

	while (--idx>=0) { 
		printf("%d  %s	%d	%d	%d	link %d\n", idx, table[idx].name, 
		table[idx].type, table[idx].lvl, table[idx].offst, table[idx].link);
	}

	printf("====== Symbol Table End ======\n\n");
}

void GenLab(char *label) {
	sprintf(label, "LAB%d", Lno);
	Lno++;
}

void EmitLab(char *label) {
	
	strcpy(labelOffst[labelOffstIndx].lab, label);
	labelOffst[labelOffstIndx].address = instrNum;
	labelOffstIndx++;
	// for(int i=0; i<labelOffstIndx; i++){
	// 	printf("labelOffst[%d] = %s\n", i, labelOffst[i]);
	// }
	printf("%s\n", label);
}

void Emit1(char *code, int ld, int offst) {
	Instruction tmp;
	if(strcmp(code, "LIT")==0){
		tmp.f = Lit;
		tmp.l = 0;
	} else if(strcmp(code, "LOD")==0){
		tmp.f = Lod;
		tmp.l = ld;
	} else if(strcmp(code, "STO")==0){
		tmp.f = Sto;
		tmp.l = ld;
	} else if(strcmp(code, "INT")==0){		// INT
		tmp.f = Int;
		tmp.l = 0;
	} else {		// ALD
		tmp.f = Ald;
		tmp.l = ld;
	}
	tmp.a=offst;
	// tmp.lab="NULL";

	Code[instrNum] = tmp;
	strcpy(Code[instrNum].lab, "NULL");
	printf("%d	%s	%d	%d\n", instrNum++, code, ld, offst);
}

void Emit2(char *code, int ld, char *name) {	// Call
	Instruction tmp;
	if(strcmp(code, "CAL")==0){
		tmp.f = Cal;
	} else {
		tmp.f = Fcl;
	}
	tmp.l = ld;
	tmp.a = -1;									// Need Address !! 
	// strcpy(tmp.lab, name);
	Code[instrNum] = tmp;
	strcpy(Code[instrNum].lab, name);
	printf("%d	%s	%d	%s\n", instrNum++, code, ld, name);
}

void Emit3(char *code, char *label) {	// Jpc Jmp
	Instruction tmp;
	if(strcmp(code, "JPC")==0){
		tmp.f = Jpc;
	} else {	// JMP
		tmp.f =Jmp;
	}
	
	tmp.l=0;
	tmp.a=-1;									// Need Address !! 
	
	// strcpy(tmp.lab, label);
	Code[instrNum] = tmp;
	strcpy(Code[instrNum].lab, label);
	printf("%d	%s	%s\n", instrNum++, code, label);
}

void Emit(char *code) {
	Instruction tmp;
	tmp.f = Opr;
	tmp.l=0;
	// tmp.lab="NULL";

	if(strcmp(code, "RET")==0){
		tmp.a=0;
	} else if(strcmp(code, "NEG")==0){
		tmp.a=1;
	} else if(strcmp(code, "ADD")==0){
		tmp.a=2;
	} else if(strcmp(code, "SUB")==0){
		tmp.a=3;
	} else if(strcmp(code, "MUL")==0){
		tmp.a=4;
	} else if(strcmp(code, "DIV")==0){
		tmp.a=5;
	} else if(strcmp(code, "ODD")==0){
		tmp.a=6;
	} else if(strcmp(code, "AND")==0){
		tmp.a=7;
	} else if(strcmp(code, "EQ")==0){
		tmp.a=8;
	} else if(strcmp(code, "NE")==0){
		tmp.a=9;
	} else if(strcmp(code, "LT")==0){
		tmp.a=10;
	} else if(strcmp(code, "GE")==0){
		tmp.a=11;
	} else if(strcmp(code, "GT")==0){
		tmp.a=12;
	} else if(strcmp(code, "LE")==0){
		tmp.a=13;
	} else if(strcmp(code, "FRT")==0){
		tmp.a=14;
	} else {	// code == END
		tmp.a=20;
		Code[instrNum] = tmp;
		Instruction end;
		end.f=0; end.l=0; end.a=0;
		++instrNum;
		Code[instrNum] = end;
		return;
	}
	
	Code[instrNum] = tmp;
	strcpy(Code[instrNum].lab, "NULL");
	printf("%d	%s\n", instrNum++, code);
}

// void EmitFunc(char *code, int ld, char *name){

// }

// void Emit2(char *code, int ld, char *name) {	// Call
// 	Instruction tmp;
// 	tmp.f = Cal;
// 	tmp.l = ld;
// 	tmp.a = -1;									// Need Address !! 
// 	// strcpy(tmp.lab, name);
// 	Code[instrNum] = tmp;
// 	strcpy(Code[instrNum].lab, name);
// 	printf("%d	%s	%d	%s\n", instrNum++, code, ld, name);
// }

void EmitErr(char *errorMsg, char *id) {
	//yyerror(errorMsg);
	printf("***	%s '%s' !!\n", errorMsg, id);
}

void assignOffst(){
	for(int i=0; i<labelOffstIndx; i++){
		// /***/printf("labelOffst[%d].lab = %s\n", i, labelOffst[i].lab);
		for(int j=0; j<instrNum; j++){
			if( strcmp(Code[j].lab, labelOffst[i].lab)==0 ){
				// /***/printf("Code[%d].lab = %s labelOffst[%d].lab = %s\n", j, Code[j].lab, i, labelOffst[i].lab);
				Code[j].a = labelOffst[i].address;
			}
		}
	}
}

void printBinry(){
	printf(" \n==== Binary Code ====\n");

	for(int i=0; i<instrNum; i++){
		printf("%d	%d	%d	%d\n", i, Code[i].f, Code[i].l, Code[i].a);
	}

	printf("--------------------------------------\n");
}

void printResult(){
	int validStack = numOfVar + 2;

	printf("\n=== execution result(global var. contents) ===\n");
	printf("The number of Global Variable:	%d\n",numOfVar);
	for(int i=validStack; i > 2; i--){
		printf("stack:	%d	%d\n", i ,s[i]);
	}
} 

int main() {

	for(int i=0; i<TBSIZE; i++){
		hashBucket[i]=-1;
        table[i].name[0]=-1;
		table[i].link=-1;
	}

	for(int i=0; i<cxmax; i++){
		Instruction tmp;
		tmp.f=0;
		tmp.l=-1;
		tmp.lab[0]=-1;
		Code[i]=tmp;
		labelOffst[i].lab[0]=-1;
		labelOffst[i].address=0;
		
	}

	return yyparse();
}
