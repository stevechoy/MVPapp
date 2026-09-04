$PROB
Nedosiran POP-PKPD model in patients with primary hyperoxaluria type 1 (Zhang et al., Br J Clin Pharmacol 2024)

# Model Annotations: 

block   name        descr                                                              unit                           
------  ----------  -----------------------------------------------------------------  -------------------------------
CMT     GUTSLOW1    Slow pathway input compartment                                     dose fraction FR1              
CMT     GUTSLOW2    Slow pathway transit compartment 1                                 .                              
CMT     GUTFAST1    Fast pathway input compartment                                     dose fraction 1-FR1            
CMT     GUTFAST2    Fast pathway transit compartment 1                                 .                              
CMT     GUTFAST3    Fast pathway transit compartment 2                                 .                              
CMT     GUTFAST4    Fast pathway transit compartment 3                                 .                              
CMT     CENT        Central compartment                                                nedosiran plasma               
CMT     PERIPH      Peripheral compartment                                             .                              
CMT     EFF         Effect compartment concentration                                   .                              
CMT     UOX         24 hour urinary oxalate compartment                                .                              
PARAM   WT          Body weight                                                        kg                             
PARAM   EGFR        Estimated glomerular filtration rate                               mL/min/1.73m2                  
PARAM   RMF         Renal maturation function                                          unitless, 1 for adults         
PARAM   PH1         Disease status indicator                                           1 = PH1 patient, 0 = otherwise 
PARAM   TVCL        Typical apparent clearance                                         L/h                            
PARAM   CLWTEXP     Fixed allometric exponent of WT on CL/F and Q/F                    .                              
PARAM   CLEGFREXP   Exponent of eGFR effect on CL/F                                    .                              
PARAM   TVQ         Typical apparent intercompartmental clearance                      L/h                            
PARAM   TVVC        Typical apparent central volume of distribution                    L                              
PARAM   VCWTEXP     Fixed allometric exponent of WT on Vc/F and Vp/F                   .                              
PARAM   VCEGFREXP   Exponent of eGFR effect on Vc/F                                    .                              
PARAM   TVVP        Typical apparent peripheral volume of distribution                 L                              
PARAM   TVVMAX      Typical maximum metabolic rate                                     mg/h                           
PARAM   TVKM        Michaelis-Menten constant                                          ng/mL                          
PARAM   TVKA1       Typical first-order absorption rate constant, slow pathway         1/h                            
PARAM   TVKA2       Typical first-order absorption rate constant, fast pathway         1/h                            
PARAM   KAWTEXP     Exponent of WT on ka1 and ka2                                      .                              
PARAM   PH1KA1      Covariate effect of PH1 disease status on ka1                      .                              
PARAM   TVFR1       Typical fraction of dose absorbed via the slow pathway             .                              
PARAM   TVUOX0      Typical baseline 24-h urinary oxalate                              umol/24h                       
PARAM   TVKOUT      First-order elimination rate constant of 24-h Uox                  1/week                         
PARAM   TVIMAX      Maximum inhibitory effect on Uox production                        fraction                       
PARAM   TVIC50      Half-maximal inhibitory concentration in effect compartment        ng/mL                          
PARAM   HILL        Hill coefficient                                                   .                              
PARAM   TVLAMBDA    Equilibration half-life in effect compartment                      week                           
OMEGA   EVC         ETA on Vc/F                                                        .                              
OMEGA   EKA1        ETA on ka1                                                         .                              
OMEGA   EKA2        ETA on ka2                                                         .                              
OMEGA   EVMAX       ETA on Vmax                                                        .                              
OMEGA   EFR1        ETA on FR1                                                         additive                       
OMEGA   EUOX0       ETA on baseline 24-h Uox                                           .                              
OMEGA   EIC50       ETA on IC50                                                        .                              
OMEGA   EIMAX       ETA on Imax                                                        .                              
SIGMA   PROP        Proportional residual error variance for nedosiran concentration   .                              
SIGMA   ADD         Additive residual error variance for 24-h Uox^2                    umol/24h                       

$PARAM
WT = 70
EGFR = 90
RMF = 1
PH1 = 1
TVCL = 5.73
CLWTEXP = 0.75
CLEGFREXP = 0.864
TVQ = 2.45
TVVC = 145
VCWTEXP = 1
VCEGFREXP = 0.19
TVVP = 5300
TVVMAX = 3.94
TVKM = 293
TVKA1 = 0.214
TVKA2 = 16.6
KAWTEXP = -0.816
PH1KA1 = 1.56
TVFR1 = 0.698
TVUOX0 = 1420
TVKOUT = 0.338
TVIMAX = 0.645
TVIC50 = 1.04
HILL = 2.56
TVLAMBDA = 21.9

$INIT
GUTSLOW1 = 0
GUTSLOW2 = 0
GUTFAST1 = 0
GUTFAST2 = 0
GUTFAST3 = 0
GUTFAST4 = 0
CENT = 0
PERIPH = 0
EFF = 0
UOX = 1420

$OMEGA
@block
@labels EVC EKA1 EKA2 EVMAX EFR1 EUOX0 EIC50 EIMAX
// row 1
0.0709
// row 2
0
0.163
// row 3
0
0
0.0734
// row 4
0
0
0
0.117
// row 5
0
0
0
0
0.35
// row 6
0
0
0
0
0
0.0956
// row 7
0
0
0
0
0
0
0.554
// row 8
0
0
0
0
0
0
0
0.0828

$SIGMA
@block
@labels PROP ADD
// row 1
0.0767
// row 2
0
42025

$MAIN
// Allometric and covariate relationships (weight, eGFR, disease status)
double CL = TVCL*pow(WT/70, CLWTEXP)*pow(EGFR*RMF/90, CLEGFREXP);
double Q  = TVQ*pow(WT/70, CLWTEXP);
double VC = TVVC*pow(WT/70, VCWTEXP)*pow(EGFR*RMF/90, VCEGFREXP)*exp(EVC);
double VP = TVVP*pow(WT/70, VCWTEXP);
double VMAX = TVVMAX*exp(EVMAX);
double KM = TVKM;
// PH1 disease status increases ka1 relative to other populations
double KA1 = TVKA1*pow(WT/70, KAWTEXP)*(PH1 == 1 ? PH1KA1 : 1.0)*exp(EKA1);
double KA2 = TVKA2*pow(WT/70, KAWTEXP)*exp(EKA2);
// Fraction absorbed via slow pathway (additive BSV), bounded to [0,1]
double FR1 = TVFR1 + EFR1;
if(FR1 < 0) FR1 = 0;
if(FR1 > 1) FR1 = 1;
F_GUTSLOW1 = FR1;
F_GUTFAST1 = 1 - FR1;
// PD parameters
double UOX0 = TVUOX0*exp(EUOX0);
double KOUT = TVKOUT/168; // convert 1/week to 1/h
double KIN  = KOUT*UOX0;  // zero-order production rate to match baseline steady state (Eff=0)
double IC50 = TVIC50*exp(EIC50);
double IMAX = TVIMAX*exp(EIMAX);
// Equilibration half-life in effect compartment converted from weeks to hours
double LAMBDAH = TVLAMBDA*168;
double KE0 = log(2.0)/LAMBDAH;
UOX_0 = UOX0;
 
$ODE
// Slow pathway: two sequential transit compartments feeding into central
dxdt_GUTSLOW1 = -KA1*GUTSLOW1;
dxdt_GUTSLOW2 = KA1*GUTSLOW1 - KA1*GUTSLOW2;
// Fast pathway: four sequential transit compartments feeding into central
dxdt_GUTFAST1 = -KA2*GUTFAST1;
dxdt_GUTFAST2 = KA2*GUTFAST1 - KA2*GUTFAST2;
dxdt_GUTFAST3 = KA2*GUTFAST2 - KA2*GUTFAST3;
dxdt_GUTFAST4 = KA2*GUTFAST3 - KA2*GUTFAST4;
// Convert central amount (mg) / volume (L) to concentration in ng/mL
double CP = CENT/VC*1000;
// Parallel linear (CL) and nonlinear Michaelis-Menten elimination
dxdt_CENT = KA1*GUTSLOW2 + KA2*GUTFAST4
            - (CL/VC)*CENT - (Q/VC)*CENT + (Q/VP)*PERIPH
            - (VMAX*CP)/(KM + CP);
dxdt_PERIPH = (Q/VC)*CENT - (Q/VP)*PERIPH;
// First-order equilibration of effect compartment with plasma concentration
dxdt_EFF = KE0*(CP - EFF);
// Inhibitory (Imax) effect of nedosiran on zero-order Uox production
double EFFI = IMAX*pow(EFF, HILL)/(pow(EFF, HILL) + pow(IC50, HILL));
dxdt_UOX = KIN*(1 - EFFI) - KOUT*UOX;
 
$TABLE
double IPRED = CP;
double DV = IPRED*(1 + EPS(1));
double UOXIPRED = UOX;
double UOXDV = UOXIPRED + EPS(2);
 
$CAPTURE
IPRED
DV
UOXIPRED
UOXDV

