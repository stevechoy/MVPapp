$PROB
Sugemalimab PopPK model with time-dependent clearance (Gibiansky et al., 2014); bridging Asian to Caucasian NSCLC patients

# Model Annotations: 

block   name       descr                                            unit                                                        
------  ---------  -----------------------------------------------  ------------------------------------------------------------
CMT     CENT       Central compartment                              ug/mL scaled, mg dose                                       
CMT     PERIPH     Peripheral compartment                           .                                                           
PARAM   POPCL0     Time-independent clearance                       L/day                                                       
PARAM   POPCLT     Time-dependent (non-linear) clearance            L/day                                                       
PARAM   POPKDES    Decay coefficient of time-dependent clearance    1/day                                                       
PARAM   POPVC      Central volume of distribution                   L                                                           
PARAM   POPQ       Inter-compartmental clearance                    L/day                                                       
PARAM   POPVP      Peripheral volume of distribution                L                                                           
PARAM   PROPRUV    Proportional residual error SD                   .                                                           
PARAM   SEXVCTH    Sex effect on VC                                 exponent                                                    
PARAM   WTCL0TH    Weight effect on CL0                             exponent                                                    
PARAM   ALBCL0TH   Albumin effect on CL0                            exponent                                                    
PARAM   WTVCTH     Weight effect on VC                              exponent                                                    
PARAM   SEXCLTTH   Sex effect on CLT                                exponent                                                    
PARAM   KDESL      Lymphoma effect on KDES                          .                                                           
PARAM   ALBVCTH    Albumin effect on VC                             exponent                                                    
PARAM   ALBVPTH    Albumin effect on VP                             exponent                                                    
PARAM   VCL        Lymphoma effect on VC                            .                                                           
PARAM   TITERCLT   Titer effect on CLT                              .                                                           
PARAM   VPN        NSCLC stage III/IV effect on VP                  .                                                           
PARAM   SEXCL0TH   Sex effect on CL0                                exponent                                                    
PARAM   SDA1       Cholesky SD for CL0                              .                                                           
PARAM   SDA2       Cholesky SD for VC                               .                                                           
PARAM   CORA21     Cholesky correlation CL0-VC                      .                                                           
PARAM   SDA3       Cholesky SD for VP                               .                                                           
PARAM   CORA31     Cholesky correlation CL0-VP                      .                                                           
PARAM   CORA32     Cholesky correlation VC-VP                       .                                                           
PARAM   SDA4       Cholesky SD for CLT                              .                                                           
PARAM   CORA41     Cholesky correlation CLT-CL0                     .                                                           
PARAM   CORA42     Cholesky correlation CLT-VC                      .                                                           
PARAM   CORA43     Cholesky correlation CLT-VP                      .                                                           
PARAM   SDA5       Cholesky SD for KDES                             .                                                           
PARAM   CORA51     Cholesky correlation KDES-CL0                    .                                                           
PARAM   CORA52     Cholesky correlation KDES-VC                     .                                                           
PARAM   CORA53     Cholesky correlation KDES-VP                     .                                                           
PARAM   CORA54     Cholesky correlation KDES-CLT                    .                                                           
PARAM   SEX        Sex                                              0/1                                                         
PARAM   WT         Body weight                                      kg                                                          
PARAM   ALB        Albumin                                          g/L                                                         
PARAM   DSSTAT     Disease status                                   1=Lymphoma, >=2=NSCLC stage III/IV, else=other solid tumors 
PARAM   TITERLCF   Time-varying ADA titer level                     baseline negative=0.5                                       
OMEGA   ETA1       Raw ETA1 used for Cholesky-derived BSV on CL0    .                                                           
OMEGA   ETA2       Raw ETA2 used for Cholesky-derived BSV on VC     .                                                           
OMEGA   ETA3       Raw ETA3 used for Cholesky-derived BSV on VP     .                                                           
OMEGA   ETA4       Raw ETA4 used for Cholesky-derived BSV on CLT    .                                                           
OMEGA   ETA5       Raw ETA5 used for Cholesky-derived BSV on KDES   .                                                           
SIGMA   EPS1       Proportional residual error                      scaled by PROPRUV in $TABLE                                 

$PARAM
POPCL0 = 0.156
POPCLT = 0.09
POPKDES = 0.022
POPVC = 3.5
POPQ = 0.363
POPVP = 0.8
PROPRUV = 0.171
SEXVCTH = 0.852
WTCL0TH = 0.75
ALBCL0TH = -0.98
WTVCTH = 0.45
SEXCLTTH = 0.85
KDESL = 0.06
ALBVCTH = -0.39
ALBVPTH = -1.8
VCL = 0.9
TITERCLT = 0.1
VPN = 2.2
SEXCL0TH = 0.85
SDA1 = 0.267
SDA2 = 0.185
CORA21 = 0.347
SDA3 = 0.658
CORA31 = 0
CORA32 = 0
SDA4 = 0.492
CORA41 = 0
CORA42 = 0.556
CORA43 = 0
SDA5 = 1.185
CORA51 = 0
CORA52 = 0
CORA53 = 0
CORA54 = 0.41
SEX = 0
WT = 61.5
ALB = 41.9
DSSTAT = 0
TITERLCF = 0.5

$INIT
CENT = 0
PERIPH = 0

$OMEGA
@block
@labels ETA1 ETA2 ETA3 ETA4 ETA5
// row 1
1
// row 2
0
1
// row 3
0
0
1
// row 4
0
0
0
1
// row 5
0
0
0
0
1

$SIGMA
@block
@labels EPS1
// row 1
1

$MAIN
// Cholesky decomposition for correlated ETAs
double CH_A22 = sqrt(1-(CORA21*CORA21));
double CH_A32 = (CORA32-CORA21*CORA31)/CH_A22;
double CH_A42 = (CORA42-CORA21*CORA41)/CH_A22;
double CH_A52 = (CORA52-CORA21*CORA51)/CH_A22;
double CH_A33 = sqrt(1-(CORA31*CORA31+CH_A32*CH_A32));
double CH_A43 = (CORA43-(CORA31*CORA41+CH_A32*CH_A42))/CH_A33;
double CH_A53 = (CORA53-(CORA31*CORA51+CH_A32*CH_A52))/CH_A33;
double CH_A44 = sqrt(1-(CORA41*CORA41+CH_A42*CH_A42+CH_A43*CH_A43));
double CH_A54 = (CORA54-(CORA41*CORA51+CH_A42*CH_A52+CH_A43*CH_A53))/CH_A44;
double CH_A55 = sqrt(1-(CORA51*CORA51+CH_A52*CH_A52+CH_A53*CH_A53+CH_A54*CH_A54));
double ETA_1 = ETA1*SDA1;
double ETA_2 = ETA1*CORA21*SDA2 + ETA2*CH_A22*SDA2;
double ETA_3 = ETA1*CORA31*SDA3 + ETA2*CH_A32*SDA3 + ETA3*CH_A33*SDA3;
double ETA_4 = ETA1*CORA41*SDA4 + ETA2*CH_A42*SDA4 + ETA3*CH_A43*SDA4 + ETA4*CH_A44*SDA4;
double ETA_5 = ETA1*CORA51*SDA5 + ETA2*CH_A52*SDA5 + ETA3*CH_A53*SDA5 + ETA4*CH_A54*SDA5 + ETA5*CH_A55*SDA5;
// Covariate effects
double SEXVC = pow(SEXVCTH, SEX);
double WTCL0 = pow(WT/61.5, WTCL0TH);
double ALBCL0 = pow(ALB/41.9, ALBCL0TH);
double WTVC = pow(WT/61.5, WTVCTH);
double SEXCLT = pow(SEXCLTTH, SEX);
double ALBVC = pow(ALB/41.9, ALBVCTH);
double ALBVP = pow(ALB/41.9, ALBVPTH);
double SEXCL0 = pow(SEXCL0TH, SEX);
double DSVC = 1.0;
if(DSSTAT==1) DSVC = VCL; // Lymphoma effect on VC
double DSVP = 1.0;
if(DSSTAT>=2) DSVP = VPN; // NSCLC stage III/IV effect on VP
// Baseline (negative ADA) titer set to 0.5 (half dilution factor)
double TITERCOV = 1 + TITERCLT*(log(TITERLCF) - log(0.5));
// Fixed effect (typical value) parameters, unit conversion day->hr
double TVCL0 = POPCL0/24;
double TVCLT = POPCLT/24;
double TVKDES = POPKDES;
if(DSSTAT==1) TVKDES = KDESL; // Lymphoma disease effect on KDES
double TVVC = POPVC;
double TVQ = POPQ/24;
double TVVP = POPVP;
// Individual parameters
double CL0 = TVCL0 * WTCL0 * ALBCL0 * SEXCL0 * exp(ETA_1);
double VC = TVVC * SEXVC * WTVC * ALBVC * DSVC * exp(ETA_2);
double Q = TVQ;
double VP = TVVP * ALBVP * DSVP * exp(ETA_3);
double CLT = TVCLT * SEXCLT * TITERCOV * exp(ETA_4);
double KDES = TVKDES * exp(ETA_5);
 
$ODE
// Time-dependent (decaying) clearance; SOLVERTIME in hours, KDES in 1/day
double CL = CLT*exp(-KDES*SOLVERTIME/24) + CL0;
dxdt_CENT = -(CL/VC)*CENT - (Q/VC)*CENT + (Q/VP)*PERIPH;
dxdt_PERIPH = (Q/VC)*CENT - (Q/VP)*PERIPH;
 
$TABLE
CL = CLT*exp(-KDES*TIME/24) + CL0;
double IPRED = CENT/VC;
double DV = IPRED*(1 + PROPRUV*EPS1);
 
$CAPTURE
IPRED
DV
CL

