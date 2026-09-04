$PROB
Hartmann 2025 Population PK model for oral drug in pediatric/adult ILD patients (J Pharmacokinet Pharmacodyn/CPT:PSP, Syst Pharma 2025)

# Model Annotations: 

block   name       descr                                  unit                  
------  ---------  -------------------------------------  ----------------------
CMT     GUT        Dosing compartment                     depot                 
CMT     CENT       Central compartment                    .                     
PARAM   TVCL       Typical clearance                      L/h                   
PARAM   TVV        Typical volume of distribution         L                     
PARAM   TVKA       Typical absorption rate constant       1/h                   
PARAM   TVALAG     Typical absorption lag time            h                     
PARAM   THETA5     RACEREG Other effect on F1             non-White, non-Korean 
PARAM   THETA6     RACEREG Korean effect on F1            .                     
PARAM   THETA7     DIAG6 effect on F1                     SSc-ILD               
PARAM   THETA8     LDHBL effect on F1                     .                     
PARAM   THETA9     Pediatric age effect on IOV of F1      .                     
PARAM   WTKG       Body weight                            kg                    
PARAM   RACEREG1   Race-region indicator, White           .                     
PARAM   RACEREG3   Race-region indicator, Korean          .                     
PARAM   SSCSUB4    SSc-ILD subgroup indicator             .                     
PARAM   LDHBL      Baseline LDH                           U/L                   
PARAM   AGEY       Age                                    years                 
PARAM   OCC        Occasion number for IOV on F1          1-6                   
OMEGA   ETA1       IIV on V                               .                     
OMEGA   ETA2       IIV on KA                              .                     
OMEGA   ETA3       IIV on F1                              .                     
OMEGA   ETA4       IOV on F1, occasion 1                  .                     
OMEGA   ETA5       IOV on F1, occasion 2                  .                     
OMEGA   ETA6       IOV on F1, occasion 3                  .                     
OMEGA   ETA7       IOV on F1, occasion 4                  .                     
OMEGA   ETA8       IOV on F1, occasion 5                  .                     
OMEGA   ETA9       IOV on F1, occasion 6                  .                     
SIGMA   EPS1       Additive residual error on log scale   .                     

$PARAM
TVCL = 909.629
TVV = 10696.3
TVKA = 2.73232
TVALAG = 0.71714
THETA5 = 0.3289
THETA6 = -0.143797
THETA7 = -0.138784
THETA8 = 0.00155711
THETA9 = -0.0525537
WTKG = 75
RACEREG1 = 1
RACEREG3 = 0
SSCSUB4 = 0
LDHBL = 206
AGEY = 30
OCC = 1

$INIT
GUT = 0
CENT = 0

$OMEGA
@block
@labels ETA1 ETA2 ETA3
// row 1
0.0952477
// row 2
0
1.56911
// row 3
0
0
0.173479

$OMEGA
@block
@labels ETA4 ETA5 ETA6 ETA7 ETA8 ETA9
// row 1
0.0961255
// row 2
0
0.0961255
// row 3
0
0
0.0961255
// row 4
0
0
0
0.0961255
// row 5
0
0
0
0
0.0961255
// row 6
0
0
0
0
0
0.0961255

$SIGMA
@block
@labels EPS1
// row 1
0.153471

$MAIN
// Weight effects on CL and V
double WTCL = pow(WTKG/75, 0.75);
double WTV  = WTKG/75;
double CL = TVCL*WTCL;
double V  = TVV*WTV*exp(ETA(1));
double KA = TVKA*exp(ETA(2));
double ALAG1 = TVALAG;
// Race-region effect on F1
double F1RACEREG;
if(RACEREG1==1) F1RACEREG = 1;
else if(RACEREG1==0 && RACEREG3!=1) F1RACEREG = 1 + THETA5;
else if(RACEREG3==1) F1RACEREG = 1 + THETA6;
else F1RACEREG = 1;
// Diagnosis subgroup effect on F1
double F1DIAG6;
if(SSCSUB4==0) F1DIAG6 = 1;
else F1DIAG6 = 1 + THETA7;
// Baseline LDH effect on F1
double F1LDHBL = exp(THETA8*(LDHBL - 206));
double TVF1 = 1*F1RACEREG*F1DIAG6*F1LDHBL;
// Pediatric age effect on IOV magnitude
double IOVPEDAGE;
if(AGEY >= 18) IOVPEDAGE = 1;
else IOVPEDAGE = exp(THETA9*(AGEY - 18));
double TVETIOF = IOVPEDAGE;
// Occasion-specific IOV on F1
double IOVF = 0;
if(OCC==1) IOVF = ETA(4)*TVETIOF;
if(OCC==2) IOVF = ETA(5)*TVETIOF;
if(OCC==3) IOVF = ETA(6)*TVETIOF;
if(OCC==4) IOVF = ETA(7)*TVETIOF;
if(OCC==5) IOVF = ETA(8)*TVETIOF;
if(OCC==6) IOVF = ETA(9)*TVETIOF;
double F1A = TVF1*exp(ETA(3));
double F1  = F1A*exp(IOVF);
F_GUT = F1;
ALAG_GUT = ALAG1;
 
$ODE
dxdt_GUT  = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/V)*CENT;
 
$TABLE
double DEL = 0.0000001;
double CP = CENT/V;
double IPRED = log(CP + DEL);
double DV = IPRED + EPS(1);
 
$CAPTURE
IPRED
DV

