$PROB
Amlitelimab PopPK and PopPK/PD-EASI model in atopic dermatitis (Tiraboschi et al., CPT Pharmacometrics Syst Pharmacol, 2025)

# Model Annotations: 

block   name       descr                                                         unit                            
------  ---------  ------------------------------------------------------------  --------------------------------
CMT     DEPOT      Depot compartment for subcutaneous absorption                 .                               
CMT     CENT       Central compartment                                           .                               
CMT     PERIPH     Peripheral compartment                                        .                               
CMT     EASI       EASI score compartment                                        .                               
PARAM   TVV1       Typical central volume of distribution                        L                               
PARAM   TVV2       Typical peripheral volume of distribution                     L                               
PARAM   TVCLL      Typical linear clearance                                      L/day                           
PARAM   TVQ2       Typical intercompartmental clearance                          L/day                           
PARAM   TVFsc      Typical subcutaneous bioavailability                          .                               
PARAM   TVVM       Typical maximum elimination rate for nonlinear clearance      ug/day                          
PARAM   TVKM       Typical Michaelis-Menten constant                             ug/mL                           
PARAM   TVALAG     Typical absorption lag time                                   day                             
PARAM   TVKA       Typical first-order absorption rate constant                  1/day                           
PARAM   dBWTV1     Body weight exponent on V1                                    .                               
PARAM   dBWTV2     Body weight exponent on V2                                    .                               
PARAM   dBWTCL     Body weight exponent on CL                                    .                               
PARAM   dBEASICL   Baseline EASI effect on CL                                    .                               
PARAM   dBALBFsc   Baseline albumin effect on Fsc                                .                               
PARAM   BWT        Baseline body weight                                          kg                              
PARAM   BEASI      Baseline EASI score                                           .                               
PARAM   BALB       Baseline albumin                                              g/L                             
PARAM   RESP       Responder status                                              1 = responder, 0 = nonresponder 
PARAM   IMAXR      Maximum inhibitory effect in responders                       Imax                            
PARAM   IMAXNR     Maximum inhibitory effect in nonresponders                    Imax                            
PARAM   KOUTR      First-order EASI dissipation rate constant in responders      1/day                           
PARAM   KOUTNR     First-order EASI dissipation rate constant in nonresponders   1/day                           
PARAM   IC50       Free amlitelimab concentration at 50% of Imax                 ug/mL                           
OMEGA   EV1        ETA on central volume V1                                      .                               
OMEGA   ECL        ETA on clearance CL                                           correlated with V1              
OMEGA   EV2        ETA on peripheral volume V2                                   .                               
OMEGA   EFSC       ETA on bioavailability Fsc                                    .                               
OMEGA   EALAG      ETA on absorption lag time                                    .                               
OMEGA   EKA        ETA on absorption rate constant Ka                            .                               
OMEGA   EKOUT      ETA on Kout                                                   .                               
OMEGA   EIMAX      ETA on Imax                                                   .                               
SIGMA   PROP       Proportional residual error for PK concentration              .                               
SIGMA   EPROP      Proportional residual error for EASI                          .                               
SIGMA   EADD       Additive residual error for EASI                              .                               

$PARAM
TVV1 = 3.46
TVV2 = 2.48
TVCLL = 0.115
TVQ2 = 0.569
TVFsc = 0.888
TVVM = 0.0362
TVKM = 0.0783
TVALAG = 0.0351
TVKA = 0.233
dBWTV1 = 0.901
dBWTV2 = 0.35
dBWTCL = 1.2
dBEASICL = 0.00111
dBALBFsc = 0.598
BWT = 75
BEASI = 27.5
BALB = 47
RESP = 1
IMAXR = 0.968
IMAXNR = 0.537
KOUTR = 0.0224
KOUTNR = 0.0396
IC50 = 0.0000166

$INIT
DEPOT = 0
CENT = 0
PERIPH = 0
EASI = 27.5

$OMEGA
@block
@labels EV1 ECL
// row 1
0.0491
// row 2
0.024
0.0482

$OMEGA
@block
@labels EV2 EFSC EALAG EKA EKOUT EIMAX
// row 1
0.0693
// row 2
0
1.18
// row 3
0
0
0.151
// row 4
0
0
0
0.135
// row 5
0
0
0
0
0.222
// row 6
0
0
0
0
0
1.65

$SIGMA
@block
@labels PROP EPROP EADD
// row 1
0.0248
// row 2
0
0.201
// row 3
0
0
0.218

$GLOBAL
double V1, V2, CL, Q2, VM, KM, KA, ALAG, FSC, KOUT, IMAX, KIN;
 
$MAIN
V1 = TVV1 * pow(BWT/75, dBWTV1) * exp(EV1);
V2 = TVV2 * pow(BWT/75, dBWTV2) * exp(EV2);
CL = (TVCLL * pow(BWT/75, dBWTCL) + dBEASICL*BEASI) * exp(ECL);
Q2 = TVQ2;
VM = TVVM;
KM = TVKM;
KA = TVKA * exp(EKA);
ALAG = TVALAG * exp(EALAG);
// logit-normal transformation for bioavailability (bounded 0-1)
double FSCCOV = TVFsc + dBALBFsc * ((BALB/47) - 1);
double PHIF = log(FSCCOV/(1-FSCCOV));
FSC = exp(PHIF+EFSC) / (1+exp(PHIF+EFSC));
F_DEPOT = FSC;
ALAG_DEPOT = ALAG;
// responder status drives typical Imax and Kout
double IMAXTV = RESP==1 ? IMAXR : IMAXNR;
double KOUTTV = RESP==1 ? KOUTR : KOUTNR;
KOUT = KOUTTV * exp(EKOUT);
// logit-normal transformation for Imax (bounded 0-1)
double PHII = log(IMAXTV/(1-IMAXTV));
IMAX = exp(PHII+EIMAX) / (1+exp(PHII+EIMAX));
// steady-state production constant based on baseline EASI
KIN = KOUT * BEASI;
EASI_0 = BEASI;
 
$ODE
double K20 = CL/V1;
double K23 = Q2/V1;
double K32 = Q2/V2;
double C2 = CENT/V1;
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT = KA*DEPOT - K23*CENT + K32*PERIPH - K20*CENT - (C2*VM)/(KM+C2);
dxdt_PERIPH = K23*CENT - K32*PERIPH;
double EFF = 1 - (IMAX*C2)/(IC50+C2);
dxdt_EASI = KIN*EFF - KOUT*EASI;
 
$TABLE
double IPRED = CENT/V1;
double DV = IPRED*(1+EPS(1));
double EASIIPRED = EASI;
double EASIDV = EASIIPRED*(1+EPS(2)) + EPS(3);
 
$CAPTURE
IPRED
DV
EASIIPRED
EASIDV

