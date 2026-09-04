$PROB
Population PK/PD TMDD model of lampalizumab administered intravitreally in geographic atrophy patients (Le et al., CPT Pharmacometrics Syst Pharmacol 2015, doi:10.1002/psp4.12031)

# Model Annotations: 

block   name       descr                                                                 unit                 
------  ---------  --------------------------------------------------------------------  ---------------------
CMT     AVITR      Total lampalizumab amount in vitreous humor                           mg                   
CMT     RVITR      Total CFD (target) concentration in vitreous humor                    mg/mL                
CMT     ASER       Total lampalizumab amount in serum                                    mg                   
PARAM   TVKOUT     Typical ocular elimination rate constant of drug                      1/day, 80yo male     
PARAM   TVKOUTC    Ocular elimination rate constant of complex                           1/day                
PARAM   TVKINT     Ocular influx/synthesis rate constant of target                       mg/mL/day            
PARAM   TVKA       Correction factor for drug:target molar ratio and assay differences   .                    
PARAM   TVVVITR    Vitreous volume of distribution                                       mL                   
PARAM   TVKOUTT    Ocular degradation/elimination rate constant of target                1/day                
PARAM   TVKAQ      Vitreous-aqueous partition coefficient of drug                        .                    
PARAM   TVVC       Systemic volume of distribution                                       mL                   
PARAM   TVK        Typical systemic elimination rate constant of drug                    1/day, 80yo male     
PARAM   KSS        Quasi-steady-state constant                                           mg/mL, fixed         
PARAM   HAGEKOUT   Power for age effect on kout                                          .                    
PARAM   HAGEK      Power for age effect on k                                             .                    
PARAM   HSEXK      Multiplier for sex effect on k                                        female               
PARAM   AGE        Patient age                                                           years                
PARAM   SEX        Sex                                                                   0 = male, 1 = female 
OMEGA   ETA_KOUT   IIV on ocular elimination rate constant                               kout                 
OMEGA   ETA_K      IIV on systemic elimination rate constant                             k                    
OMEGA   ETA_KA     IIV on correction factor                                              kA                   
SIGMA   EPS_AQ     Proportional residual error variance for aqueous humor measurements   .                    
SIGMA   EPS_SER    Proportional residual error variance for serum measurements           .                    

$PARAM
TVKOUT = 0.117
TVKOUTC = 0.135
TVKINT = 0.000364
TVKA = 2.23
TVVVITR = 3.09
TVKOUTT = 0.27
TVKAQ = 13
TVVC = 2410
TVK = 1.89
KSS = 0.00096
HAGEKOUT = -0.77
HAGEK = -1.63
HSEXK = 0.739
AGE = 80
SEX = 0

$INIT
AVITR = 0
RVITR = 0.00134814814814815
ASER = 0

$OMEGA
@block
@labels ETA_KOUT ETA_K ETA_KA
// row 1
0.273
// row 2
0
0.275
// row 3
0
0
0.591

$SIGMA
@block
@labels EPS_AQ EPS_SER
// row 1
0.258
// row 2
0
0.329

$MAIN
double kout = TVKOUT * pow(AGE/80.0, HAGEKOUT) * exp(ETA_KOUT);
double koutC = TVKOUTC;
double kinT = TVKINT;
double kA = TVKA * exp(ETA_KA);
double VVITR = TVVVITR;
double koutT = TVKOUTT;
double kAQ = TVKAQ;
double kTAQ = kAQ * kA;
double VC = TVVC;
double k = TVK * pow(AGE/80.0, HAGEK) * pow(HSEXK, SEX) * exp(ETA_K);
// Initialize target compartment at pre-dose steady-state (production = degradation)
RVITR_0 = kinT/koutT;
 
$ODE
// Quasi-steady-state approximation for unbound drug concentration in vitreous humor
double CVITR = AVITR/VVITR;
double RCONC = RVITR;
double DIFF = CVITR - RCONC - KSS;
double CUNBOUND = 0.5*(DIFF + sqrt(DIFF*DIFF + 4.0*KSS*CVITR));
dxdt_AVITR = -kout*CUNBOUND*VVITR - koutC*RCONC*CUNBOUND*VVITR/(KSS+CUNBOUND);
dxdt_RVITR = kinT - koutT*RCONC - (koutC - koutT)*RCONC*CUNBOUND/(KSS+CUNBOUND);
dxdt_ASER = kout*CUNBOUND*VVITR + koutC*RCONC*CUNBOUND*VVITR/(KSS+CUNBOUND) - k*ASER;
 
$TABLE
double CSER = ASER/VC;
double CVITRF = AVITR/VVITR;
double CAQ = CVITRF/kAQ;
double RAQ = RVITR/kTAQ;
double IPREDSER = CSER;
double IPREDAQ = CAQ;
double IPREDRAQ = RAQ;
double DVSER = IPREDSER * (1 + EPS_SER);
double DVAQ = IPREDAQ * (1 + EPS_AQ);
double DVRAQ = IPREDRAQ * (1 + EPS_AQ);
 
$CAPTURE
IPREDSER
IPREDAQ
IPREDRAQ
DVSER
DVAQ
DVRAQ

