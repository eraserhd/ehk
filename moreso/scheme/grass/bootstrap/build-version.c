/* Generated from build-version.scm by the CHICKEN compiler
   http://www.call-cc.org
   2013-03-12 01:03
   Version 4.8.0.3 (stability/4.8.0) (rev 091c3d9)
   macosx-unix-clang-x86-64 [ 64bit manyargs ptables ]
   compiled 2013-03-12 on aeryn.xorinia.dim (Darwin)
   command line: build-version.scm -optimize-level 2 -include-path . -include-path ./ -inline -ignore-repository -feature chicken-bootstrap -no-warnings -specialize -types ./types.db -explicit-use -no-trace -output-file build-version.c
   unit: build_2dversion
*/

#include "chicken.h"

#include "buildtag.h"

static C_PTABLE_ENTRY *create_ptable(void);

static C_TLS C_word lf[8];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,17),40,35,35,115,121,115,35,98,117,105,108,100,45,116,97,103,41,0,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_188)
static void C_ccall f_188(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(C_build_2dversion_toplevel)
C_externexport void C_ccall C_build_2dversion_toplevel(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

/* ##sys#build-tag */
static void C_ccall f_188(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_188,2,t0,t1);}
/* ##sys#peek-c-string */
t2=*((C_word*)lf[1]+1);
((C_proc4)(void*)(*((C_word*)t2+1)))(4,t2,t1,C_mpointer(&a,(void*)C_BUILD_TAG),C_fix(0));}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_noret_decl(toplevel_trampoline)
static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall toplevel_trampoline(void *dummy){
C_build_2dversion_toplevel(2,C_SCHEME_UNDEFINED,C_restore);}

void C_ccall C_build_2dversion_toplevel(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);
else C_toplevel_entry(C_text("build_2dversion_toplevel"));
C_check_nursery_minimum(3);
if(!C_demand(3)){
C_save(t1);
C_reclaim((void*)toplevel_trampoline,NULL);}
toplevel_initialized=1;
if(!C_demand_2(50)){
C_save(t1);
C_rereclaim2(50*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,8);
lf[0]=C_h_intern(&lf[0],13,"\003sysbuild-tag");
lf[1]=C_h_intern(&lf[1],17,"\003syspeek-c-string");
lf[2]=C_h_intern(&lf[2],12,"\003sysbuild-id");
lf[3]=C_decode_literal(C_heaptop,"\376B\000\000\007091c3d9");
lf[4]=C_h_intern(&lf[4],16,"\003sysbuild-branch");
lf[5]=C_decode_literal(C_heaptop,"\376B\000\000\017stability/4.8.0");
lf[6]=C_h_intern(&lf[6],17,"\003sysbuild-version");
lf[7]=C_decode_literal(C_heaptop,"\376B\000\000\0074.8.0.3");
C_register_lf2(lf,8,create_ptable());
t2=C_mutate((C_word*)lf[0]+1 /* (set! ##sys#build-tag ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_188,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t3=C_mutate((C_word*)lf[2]+1 /* (set! ##sys#build-id ...) */,lf[3]);
t4=C_mutate((C_word*)lf[4]+1 /* (set! ##sys#build-branch ...) */,lf[5]);
t5=C_mutate((C_word*)lf[6]+1 /* (set! ##sys#build-version ...) */,lf[7]);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_SCHEME_UNDEFINED);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[3] = {
{"f_188:build_2dversion_2escm",(void*)f_188},
{"toplevel:build_2dversion_2escm",(void*)C_build_2dversion_toplevel},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
o|eliminated procedure checks: 1 
o|safe globals: (##sys#build-version ##sys#build-branch ##sys#build-id ##sys#build-tag) 
o|replaced variables: 1 
o|removed binding forms: 4 
o|removed binding forms: 1 
*/
/* end of file */
