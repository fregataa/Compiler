// PL0 interpreter

/*******************  Include this in Yacc  ****************/
/*******************  numeric code is needed. not the symbolic code !!  ****************/

#define cxmax 200
#define stksize 500

typedef enum { Lit, Opr, Lod, Sto, Cal, Int, Jmp, Jpc, Fcl, Ald } fct;
typedef struct {
	fct f; // function code
	int l; // level
	int a; // offset
	char lab[10];
} Instruction;

int pc=0, mp=0, sp=-1;  // program counter, base, stacktop reg.

Instruction Code[cxmax]; // code_store indexed by pc
int s[stksize]; // data_store (stack storage) indexed by sp
// int returnVal=0;

// local stack frame (Ȱ�����ڵ�) based mp
// 	offset 0: s[mp]: SL	Static Link
// 	offset 1: s[mp+1]: DL	Dynamic Link
// 	offset 2: s[mp+2]: RA	Return Address

// lod/sto l,a
//	l: level difference
//	a: offset
//
//	cal l,a
//	l: level difference
//	a: procedure entry pt.

int base(int l) {			// following static link. 비지역 참조하는 local block base.
	int b1;
	b1=mp; // find base l levels down???
	while (l>0) {
		b1=s[b1]; l--;
	}
	return b1;
} // end base

void interprete() {
	Instruction i; // IR reg.
	printf("\n=== start PL0 ===\n");
	s[0]=s[1]=s[2]=0; // stack clear
	do {
		// for(int i=0; i<sp; i++){
		// 	/***/printf("stack processing: %d	%d\n", i, s[i]);
		// }

		i=Code[pc++]; // fetch currrent instr.
		//  /***/printf("Code is: %d	%d %d\n", pc, i.f, i.a);
		switch (i.f) { // branch by ft. code
			case Lit: s[++sp]=i.a; break; // assignment
			case Opr: switch (i.a) {
				case 0: sp=mp-1; pc=s[sp+3]; mp=s[sp+2]; break; // return
				case 1: s[sp]=-s[sp]; break; 	// negate	1(negate)				ATTENTION ! Labels such as jmp, call etc should be checked.
				case 2: sp--; s[sp]=s[sp]+s[sp+1]; break;	// +		2(+), 3(-), 4(*), 5(div)
				case 3: sp--; s[sp]=s[sp]-s[sp+1]; break;	// -
				case 4: sp--; s[sp]=s[sp]*s[sp+1]; break;	// *
				case 5: if(s[sp] == 0){ printf("Opr Error! Divided by 0\n"); return; } sp--; s[sp]=s[sp]/s[sp+1]; break;	// div
				case 6: sp--; s[sp]=s[sp]%2; break;	// odd
				case 7: if(s[sp-1]==1 && s[sp+1]==1) {s[sp]=1;} else {s[sp]=0;} break;	// and
				case 8: sp--; if(s[sp]==s[sp+1]) {s[sp]=1;} else {s[sp]=0;} break;	// =
				case 9: sp--; if(s[sp]!=s[sp+1]) {s[sp]=1;} else {s[sp]=0;} break;	// <>
				case 10: sp--; if(s[sp]<s[sp+1]) {s[sp]=1;} else {s[sp]=0;} break;	// <
				case 11: sp--; if(s[sp]>=s[sp+1]) {s[sp]=1;} else {s[sp]=0;} break;	// >=
				case 12: sp--; if(s[sp]>s[sp+1]) {s[sp]=1;} else {s[sp]=0;} break;	// >
				case 13: sp--; if(s[sp]<=s[sp+1]) {s[sp]=1;} else {s[sp]=0;} break;	// <=
				case 14: s[mp-1]=s[sp]; sp=mp-1; pc=s[sp+3]; mp=s[sp+2]; break;	// Function Return
				case 20: pc=0; return;	// End
			}; break;
			case Lod: s[++sp]=s[base(i.l)+i.a]; break;		// load
			case Sto: s[base(i.l)+i.a]=s[sp]; --sp; break;		// store
			case Cal: s[sp+1]=base(i.l); s[sp+2]=mp; s[sp+3]=pc; mp=sp+1; pc=i.a; break;	// offset 0~2 => 0.base = Static Link.	1.mp = 최근 활성화된 base. 2.
			case Int: sp=sp+i.a; break;			// Increasing stack-top
			case Jmp: pc=i.a; break;			// there is an address in a-field.
			case Jpc: if(s[sp]==0){pc=i.a;} break;	// conditional jump. jumping when the stack-top is false
			case Fcl: sp++; s[sp+1]=base(i.l); s[sp+2]=mp; s[sp+3]=pc; mp=sp+1; pc=i.a; break;
			case Ald: s[++sp]=s[mp-2 -(s[mp-2]) + i.a]; break;
			// case Frt: s[base(mp-2)+] sp=mp-1; pc=s[sp+3]; mp=s[sp+2]; break;
		};
		// /***/for(int in=mp+3; in<=sp; in++){ printf("stack processing: %d	%d", in, s[in]); }
		// printf("\n");
	} while (pc);  // loop until pc=0
};

// global variable 하나마다 stack이 하나씩 발생
// instruction 실행마다 stack top 혹은 stack을 display 해주는 게 좋다.
// function은 구현은 안 되도 확장을 해봐라. 발표때 해봐라.
// function은 return 값이 있어야 한다. 
// error 가 발생했을 때, 어디서 어떻게 발생했는지 출력해줘야 한다.