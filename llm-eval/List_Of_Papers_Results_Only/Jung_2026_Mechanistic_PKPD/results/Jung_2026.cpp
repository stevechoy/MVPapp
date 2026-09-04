$PROB
PCAB gastric pH and drug PK/PD interaction model (Woojin Jung, Supplementary Material)

# Model Annotations: 

block   name        descr                                                        unit                                                
------  ----------  -----------------------------------------------------------  ----------------------------------------------------
CMT     DEPO        Depot compartment                                            drug in stomach                                     
CMT     CENT        Central compartment                                          drug in plasma                                      
CMT     STPH        Gastric pH compartment                                       .                                                   
CMT     STFO        Stomach food compartment                                     .                                                   
PARAM   TVKIN       Typical value of Kin                                         pH indirect response synthesis rate                 
PARAM   TVBASE      Typical value of Base                                        pH baseline                                         
PARAM   TVPHMAX     Typical value of physiological maximum threshold             pHMAX                                               
PARAM   TVSFOOD     Typical value of food effect scaling                         SFOOD                                               
PARAM   TVKG        Typical value of gastric emptying rate                       KG, 1/h                                             
PARAM   TVAMP       Typical value of circadian amplitude                         AMP                                                 
PARAM   TVSCALE     Typical value of Gaussian peak location                      SCALE                                               
PARAM   TVSHAPE     Typical value of Gaussian peak width                         SHAPE                                               
PARAM   TVKA        Typical value of absorption rate constant                    1/h                                                 
PARAM   TVCL        Typical value of clearance                                   L/h                                                 
PARAM   TVV2        Typical value of volume of distribution                      L                                                   
PARAM   TVSDRUG     Typical value of drug effect scaling                         unitless                                            
PARAM   TVSLOPE     Typical value of concentration exponent                      SLOPE                                               
PARAM   PROPERR     Proportional residual error SD for PK observations           log-scale                                           
PARAM   ADDERR      Additive (log-scale) residual error SD for PD observations   pH                                                  
OMEGA   EKIN        ETA on KIN                                                   IIV                                                 
OMEGA   EBASE       ETA on BASE                                                  IIV                                                 
OMEGA   EAMP        ETA on AMP                                                   IIV                                                 
OMEGA   EKAIIV      ETA on KA                                                    IIV                                                 
OMEGA   ECLIIV      ETA on CL                                                    IIV                                                 
OMEGA   EV2IIV      ETA on V2                                                    IIV                                                 
OMEGA   ESDRUGIIV   ETA on SDRUG                                                 IIV                                                 
OMEGA   ESCALEIIV   ETA on SCALE                                                 IIV                                                 
OMEGA   EKAIOV      ETA on KA                                                    occasion-level variability                          
OMEGA   ECLIOV      ETA on CL                                                    occasion-level variability                          
OMEGA   EV2IOV      ETA on V2                                                    occasion-level variability                          
OMEGA   ESDRUGIOV   ETA on SDRUG                                                 occasion-level variability                          
OMEGA   ESCALEIOV   ETA on SCALE                                                 occasion-level variability                          
SIGMA   EPS1        Residual error scale                                         fixed; actual magnitude applied through W in $TABLE 

$PARAM
TVKIN = 0.450723
TVBASE = 1.10936
TVPHMAX = 5.5853
TVSFOOD = 0.039692
TVKG = 1.7575
TVAMP = 0.685304
TVSCALE = 4.06916
TVSHAPE = 2.63001
TVKA = 0.15694
TVCL = 98.5176
TVV2 = 55.8148
TVSDRUG = 0.675558
TVSLOPE = 0.5569
PROPERR = 0.539736
ADDERR = 0.87495

$INIT
DEPO = 0
CENT = 0
STPH = 1.794664
STFO = 0

$OMEGA
@block
@labels EKIN EBASE EAMP
// row 1
0.6491
// row 2
-0.06988
0.00881655
// row 3
-0.139072
-0.000206803
0.32749

$OMEGA
@block
@labels EKAIIV ECLIIV EV2IIV ESDRUGIIV ESCALEIIV
// row 1
0.0527715
// row 2
0
0.0938912
// row 3
0
0
0.145554
// row 4
0
0
0
0.325182
// row 5
0
0
0
0
0.0927809

$OMEGA
@block
@labels EKAIOV ECLIOV EV2IOV ESDRUGIOV ESCALEIOV
// row 1
0.0904289
// row 2
0
1.28431
// row 3
0
0
0.667401
// row 4
0
0
0
0.0986337
// row 5
0
0
0
0
0.0120663

$SIGMA
@block
@labels EPS1
// row 1
1

$MAIN
// ======= pharmacodynamic params ====================================
double KIN = TVKIN * exp(EKIN);
double BASE = TVBASE * exp(EBASE);
double KOUT = KIN/BASE;
double PHMAX = TVPHMAX;
double SFOOD = TVSFOOD;
double KG = TVKG;
double AMP = TVAMP * exp(EAMP);
// gaussian peak function params
double SCALE = TVSCALE * exp(ESCALEIIV) * exp(ESCALEIOV);
double SHAPE = TVSHAPE;
// ======= pharmacokinetic params ====================================
double KA = TVKA * exp(EKAIIV) * exp(EKAIOV);
double CL = TVCL * exp(ECLIIV) * exp(ECLIOV);
double V2 = TVV2 * exp(EV2IIV) * exp(EV2IOV);
double SDRUG = TVSDRUG * exp(ESDRUGIIV) * exp(ESDRUGIOV);
double SLOPE = TVSLOPE;
double KE = CL/V2;
double S2 = V2/1000; // amount scaling (mg to mcg)
// initial condition for pH compartment (baseline + amplitude)
STPH_0 = BASE + AMP;
 
$ODE
// gaussian peak function (feedback of pH on drug absorption)
double FEED = exp(-(pow(STPH-SCALE,2))/(pow(SHAPE,2)));
double SCP = 0;
if(CENT>0) SCP = SDRUG*pow(CENT,SLOPE); // scaled drug conc.
double SPH = SFOOD*STFO; // scaled food effect
double SCON = SCP + SPH; // sum of contributions
// circadian rhythm term, cycle fixed at 24 hours, base-level normalized
double PIVAL = 3.141592653;
double CIR = AMP * cos(2*PIVAL*(SOLVERTIME)/24) / BASE;
// hyperbolic tangent (variation), base-level normalized
double PHS = PHMAX * (2/(1 + exp(-2*SCON)) - 1) / BASE;
dxdt_DEPO = -KA*DEPO*FEED;
dxdt_CENT = KA*DEPO*FEED - KE*CENT;
dxdt_STPH = (1+CIR+PHS)*KIN - STPH*KOUT;
dxdt_STFO = -KG*STFO;
 
$TABLE
double CONC = CENT/S2; // drug concentration
double PH = STPH; // gastric pH
double IPRED = 0;
double W = 0;
// observation-type dependent (log-scale) error model matching original NONMEM code
if(CMT==2) {
IPRED = log(CONC);
W = fabs(PROPERR);
}
if(CMT==3) {
IPRED = log(PH);
W = fabs(ADDERR/PH);
}
double Y = IPRED + W*EPS1;
double DV = Y;
 
$CAPTURE
CONC
PH
IPRED
W
DV

