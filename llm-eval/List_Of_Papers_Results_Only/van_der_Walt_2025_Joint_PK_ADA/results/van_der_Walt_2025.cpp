$PROB
Bidirectional joint PK-ADA (CTMM) model for avelumab; van der Walt et al., J Pharmacokinet Pharmacodyn 2025;52:33

# Model Annotations: 

block   name        descr                                                            unit                                         
------  ----------  ---------------------------------------------------------------  ---------------------------------------------
CMT     CENT        Central compartment                                              .                                            
CMT     PERIPH      Peripheral compartment                                           .                                            
CMT     A0          Probability of ADA negative status                               .                                            
CMT     A1          Probability of ADA positive status                               .                                            
PARAM   TVCL        Typical avelumab clearance                                       L/h                                          
PARAM   TVV1        Typical central volume of distribution                           L                                            
PARAM   TVV2        Typical peripheral volume of distribution                        L                                            
PARAM   TVQ         Intercompartmental clearance                                     L/h                                          
PARAM   TVIMAX      Typical maximum fractional change in CL over time                .                                            
PARAM   TVT50       Time for 50 percent of Imax                                      h, converted from 51.9 days                  
PARAM   GAMMA       Shape parameter for time effect on CL                            .                                            
PARAM   TVLAM01     Typical transition rate ADA-negative to ADA-positive             per h                                        
PARAM   TVLAM10     Typical transition rate ADA-positive to ADA-negative             per h                                        
PARAM   TUMGCGEJC   Change in log for GC/GEJC tumor type vs NSCLC                    lambda01                                     
PARAM   TUMUC       Change in log for UC tumor type vs NSCLC                         lambda01                                     
PARAM   TUMMCC      Change in log for MCC tumor type vs NSCLC                        lambda01                                     
PARAM   TUMOTHER    Change in log for other solid tumors vs NSCLC                    lambda01                                     
PARAM   BASEEFF     Change in log if baseline ADA positive                           lambda01                                     
PARAM   SL          Slope of linear effect of ADA-positive probability on CL         p1                                           
PARAM   EMAXC1      Maximum fractional change in lambda01 due to avelumab exposure   .                                            
PARAM   EC50        Concentration for half-maximal effect of exposure on lambda01    ug/mL                                        
PARAM   GAMMAC1     Shape parameter for exposure effect on lambda01                  .                                            
PARAM   WT          Body weight                                                      kg                                           
PARAM   TUMOR       Tumor type category                                              1 NSCLC, 2 GC/GEJC, 3 UC, 4 MCC, 5 Other     
PARAM   BASEADA     Baseline ADA status                                              0 negative, 1 positive                       
OMEGA   ECL         IIV on CL                                                        .                                            
OMEGA   EV1         IIV on V1                                                        .                                            
OMEGA   ELAM01      IIV on lambda01                                                  transition rate ADA-negative to ADA-positive 
OMEGA   EIMAX       IIV on Imax                                                      additive                                     
SIGMA   PROP        Proportional residual error variance                             .                                            
SIGMA   ADD         Additive residual error variance^2                               ug/mL                                        

$PARAM
TVCL = 0.0275
TVV1 = 3.52
TVV2 = 0.582
TVQ = 0.0128
TVIMAX = -0.015
TVT50 = 1245.6
GAMMA = 2.59
TVLAM01 = 0.00406
TVLAM10 = 0.146
TUMGCGEJC = 1.17
TUMUC = -0.404
TUMMCC = -0.373
TUMOTHER = -0.99
BASEEFF = 4.38
SL = 0.15
EMAXC1 = -0.374
EC50 = 349
GAMMAC1 = 2.58
WT = 70
TUMOR = 1
BASEADA = 0

$INIT
CENT = 0
PERIPH = 0
A0 = 1
A1 = 0

$OMEGA
@block
@labels ECL EV1 ELAM01 EIMAX
// row 1
0.1478
// row 2
0
0.0587
// row 3
0
0
2.4699
// row 4
0
0
0
0.0475

$SIGMA
@block
@labels PROP ADD
// row 1
0.0445
// row 2
0
5.658

$MAIN
// Standard allometric scaling of CL (0.75) and V1 (1) by body weight
double CLPOP = TVCL * pow(WT/70.0, 0.75);
double V1POP = TVV1 * (WT/70.0);
double CLi = CLPOP * exp(ECL);
double V1  = V1POP * exp(EV1);
double V2  = TVV2;
double Q   = TVQ;
// Additive IIV on Imax
double IMAXi = TVIMAX + EIMAX;
double LAM10 = TVLAM10;
// Tumor type effect on log(lambda01), NSCLC is reference
double TUMEFF;
if(TUMOR==2)      TUMEFF = TUMGCGEJC;
else if(TUMOR==3) TUMEFF = TUMUC;
else if(TUMOR==4) TUMEFF = TUMMCC;
else if(TUMOR==5) TUMEFF = TUMOTHER;
else              TUMEFF = 0;
// Covariate-adjusted log(theta_lambda01), baseline ADA status effect included
double LOGLAM01COV = log(TVLAM01) + TUMEFF + BASEEFF*BASEADA;
// Pass-through of eta for lambda01 (kept as MAIN-derived variable for use in $ODE)
double ETALAM01 = ELAM01;
// Baseline ADA status sets initial probability compartments
A0_0 = 1 - BASEADA;
A1_0 = BASEADA;
 
$ODE
double C1 = CENT / V1;
// Sigmoidal Emax effect of avelumab exposure on lambda01
double C1EFF = 1 + EMAXC1 * pow(C1, GAMMAC1) / (pow(EC50, GAMMAC1) + pow(C1, GAMMAC1));
double LAM01 = exp(C1EFF * LOGLAM01COV + ETALAM01);
// Linear effect of probability of ADA-positive status (A1) on CL
double P1 = A1;
double P1EFF = 1 + SL * P1;
// Sigmoid Imax time-varying decrease in CL
double TIMEEFF = IMAXi * pow(SOLVERTIME, GAMMA) / (pow(TVT50, GAMMA) + pow(SOLVERTIME, GAMMA));
double CLT = CLi * exp(TIMEEFF) * P1EFF;
double K10 = CLT / V1;
double K12 = Q / V1;
double K21 = Q / V2;
dxdt_CENT   = -K10*CENT - K12*CENT + K21*PERIPH;
dxdt_PERIPH =  K12*CENT - K21*PERIPH;
dxdt_A0     = -LAM01*A0 + LAM10*A1;
dxdt_A1     =  LAM01*A0 - LAM10*A1;
 
$TABLE
double IPRED = CENT / V1;
double DV = IPRED*(1 + EPS(1)) + EPS(2);
 
$CAPTURE
DV
IPRED

