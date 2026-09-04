$PROB
Population PKPD model of intravitreal bevacizumab in neovascular AMD (del Amo et al., Eur J Pharm Biopharm, 2023)

# Model Annotations: 

block   name    descr                                             unit                
------  ------  ------------------------------------------------  --------------------
CMT     VIT     Vitreous bevacizumab amount                       ug                  
CMT     RESP    Change in BCVA from baseline, R prime             number of letters   
PARAM   CL      Intravitreal clearance                            mL/day              
PARAM   VSS     Intravitreal volume of distribution               mL                  
PARAM   KIN     Synthesis rate constant of response               letters/day         
PARAM   KOUT    Degradation rate constant of response             1/day               
PARAM   SMAX    Maximum fractional stimulation of response        unitless            
PARAM   C50     Drug concentration for half-maximal stimulation   ug/mL               
OMEGA   ESMAX   ETA on Smax                                       variance, SD = 0.23 
OMEGA   EKIN    ETA on kin                                        variance, SD = 0.41 
OMEGA   EKOUT   ETA on kout                                       variance, SD = 0.54 
OMEGA   EC50    ETA on C50                                        variance, SD = 4.93 
SIGMA   PROP    Proportional residual variability                 variance, b = 0.05  
SIGMA   ADD     Additive residual variability                     variance, a = 3.91  

$PARAM
CL = 1.008
VSS = 9.39
KIN = 0.28
KOUT = 0.03
SMAX = 2.27
C50 = 2.11

$INIT
VIT = 0
RESP = 0

$OMEGA
@block
@labels ESMAX EKIN EKOUT EC50
// row 1
0.0529
// row 2
0
0.1681
// row 3
0
0
0.2916
// row 4
0
0
0
24.3049

$SIGMA
@block
@labels PROP ADD
// row 1
0.0025
// row 2
0
15.2881

$MAIN
// Individual PKPD parameters with exponential (log-normal) IIV
double KINi  = KIN  * exp(EKIN);
double KOUTi = KOUT * exp(EKOUT);
double SMAXi = SMAX * exp(ESMAX);
double C50i  = C50  * exp(EC50);
// PK parameters fixed for all patients (no IIV reported)
double CLi  = CL;
double VSSi = VSS;
 
$ODE
// One-compartment bolus PK model, mono-exponential elimination
dxdt_VIT = -(CLi/VSSi) * VIT;
// Drug concentration in vitreous (ug/mL)
double CP = VIT / VSSi;
// Turnover PD model for change in BCVA from baseline (eq. 2)
// kprog set equal to kin so that R' -> 0 as t -> infinity
dxdt_RESP = KINi * (SMAXi * CP / (CP + C50i)) - KOUTi * RESP - KINi;
 
$TABLE
double IPRED = RESP;
double DV = IPRED * (1 + EPS(1)) + EPS(2);
 
$CAPTURE
DV
IPRED

