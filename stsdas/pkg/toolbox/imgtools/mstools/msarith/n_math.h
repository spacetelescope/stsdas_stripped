
/*  Math library function definitions.

    There are two sets of underlying functions, one for NICMOS and 
    one for STIS. The upper level routines are instrument-independent.
*/

int n_constAdd    (GenericGroup *, mathConst *);
int n_constSub    (GenericGroup *, mathConst *);
int n_constSubInv (GenericGroup *, mathConst *);
int n_constMult   (GenericGroup *, mathConst *, Bool);
int n_constDiv    (GenericGroup *, mathConst *, Bool);
int n_constDivInv (GenericGroup *, mathConst *, int,int *,float);
int n_imageAdd    (GenericGroup *, GenericGroup *,Bool,int,int *,float);
int n_imageSub    (GenericGroup *, GenericGroup *);
int n_imageMult   (GenericGroup *, GenericGroup *);
int n_imageDiv    (GenericGroup *, GenericGroup *, int,int *,float);

/* NICMOS */
int nn_constAdd    (SingleNicmosGroup *, mathConst *);
int nn_constSub    (SingleNicmosGroup *, mathConst *);
int nn_constSubInv (SingleNicmosGroup *, mathConst *);
int nn_constMult   (SingleNicmosGroup *, mathConst *, Bool);
int nn_constDiv    (SingleNicmosGroup *, mathConst *, Bool);
int nn_constDivInv (SingleNicmosGroup *, mathConst *, int,int *,float);
int nn_imageAdd    (SingleNicmosGroup *, SingleNicmosGroup *,Bool,int,int *,
                    float);
int nn_imageSub    (SingleNicmosGroup *, SingleNicmosGroup *);
int nn_imageMult   (SingleNicmosGroup *, SingleNicmosGroup *);
int nn_imageDiv    (SingleNicmosGroup *, SingleNicmosGroup *, int,int *,float);

/* STIS */
int ns_constAdd    (SingleGroup *, mathConst *);
int ns_constSub    (SingleGroup *, mathConst *);
int ns_constSubInv (SingleGroup *, mathConst *);
int ns_constMult   (SingleGroup *, mathConst *, Bool);
int ns_constDiv    (SingleGroup *, mathConst *, Bool);
int ns_constDivInv (SingleGroup *, mathConst *, int,int *,float);
int ns_imageAdd    (SingleGroup *, SingleGroup *,int,int *,float);
int ns_imageSub    (SingleGroup *, SingleGroup *);
int ns_imageMult   (SingleGroup *, SingleGroup *);
int ns_imageDiv    (SingleGroup *, SingleGroup *, int,int *,float);
