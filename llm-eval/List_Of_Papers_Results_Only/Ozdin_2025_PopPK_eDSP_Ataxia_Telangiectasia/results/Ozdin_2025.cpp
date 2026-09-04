$PROB
Population PK model for dexamethasone released from encapsulated dexamethasone sodium phosphate (eDSP) administered via the EryDex System; two-compartment model with linear elimination, allometric weight scaling, and patient-status effect on clearance (Ozdin et al., CPT Pharmacometrics Syst Pharmacol, 2025)

# Model Annotations: 

block   name        descr                                           unit                                   
------  ----------  ----------------------------------------------  ---------------------------------------
CMT     CENT        Central compartment                             ng or mass units of dexamethasone      
CMT     PERIPH      Peripheral compartment                          .                                      
PARAM   WT          Body weight                                     kg                                     
PARAM   PTNT        Patient status indicator                        0 = healthy volunteer, 1 = AT patient  
PARAM   PHAS        Study phase indicator                           1 = phase 1, 3 = phase 3               
PARAM   TVCL        Typical clearance                               L/h                                    
PARAM   TVV1        Typical central volume of distribution          L                                      
PARAM   TVQ         Typical inter-compartmental clearance           L/h                                    
PARAM   TVV2        Typical peripheral volume of distribution       L                                      
PARAM   THETAPTNT   Multiplicative effect of patient status on CL   .                                      
PARAM   PROP1       Proportional residual error                     phase 1                                
PARAM   ADD1        Additive residual error                         phase 1, ng/mL                         
PARAM   PROP2       Proportional residual error                     phase 3                                
PARAM   ADD2        Additive residual error                         phase 3, ng/mL                         
OMEGA   ECL         ETA on CL                                       .                                      
OMEGA   EV1         ETA on V1                                       .                                      
SIGMA   EPS1        Proportional error term                         scaled by phase-specific PROP in TABLE 
SIGMA   EPS2        Additive error term                             scaled by phase-specific ADD in TABLE  

$PARAM
WT = 70
PTNT = 0
PHAS = 1
TVCL = 20.8
TVV1 = 122
TVQ = 0.358
TVV2 = 16.8
THETAPTNT = 0.899
PROP1 = 0.181
ADD1 = 0.0013
PROP2 = 0.295
ADD2 = 24.3

$INIT
CENT = 0
PERIPH = 0

$OMEGA
@block
@labels ECL EV1
// row 1
0.15818
// row 2
0
0.40542

$SIGMA
@block
@labels EPS1 EPS2
// row 1
1
// row 2
0
1

$MAIN
// Allometric scaling of weight (centered to 70 kg): 0.75 power for CL/Q, 1 for V1/V2
double CL = TVCL * pow(WT/70.0, 0.75) * pow(THETAPTNT, PTNT) * exp(ECL);
double V1 = TVV1 * pow(WT/70.0, 1.0) * exp(EV1);
double Q  = TVQ  * pow(WT/70.0, 0.75);
double V2 = TVV2 * pow(WT/70.0, 1.0);
 
$ODE
dxdt_CENT   = -(CL/V1)*CENT - (Q/V1)*CENT + (Q/V2)*PERIPH;
dxdt_PERIPH =  (Q/V1)*CENT - (Q/V2)*PERIPH;
 
$TABLE
double IPRED = CENT/V1;
// Phase-specific combined proportional and additive residual error
double PROP = (PHAS==1) ? PROP1 : PROP2;
double ADD  = (PHAS==1) ? ADD1  : ADD2;
double DV = IPRED*(1 + PROP*EPS(1)) + ADD*EPS(2);
 
$CAPTURE
IPRED
DV

