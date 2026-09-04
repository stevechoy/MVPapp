$PROB
Faricimab ocular popPKPD model for VEGF-A and Ang-2 suppression in nAMD and DME (Diack et al., Transl Vis Sci Technol. 2024;13(11):13)

# Model Annotations: 

block   name       descr                                                                   unit                          
------  ---------  ----------------------------------------------------------------------  ------------------------------
CMT     VH         Faricimab amount in vitreous humor                                      ug                            
CMT     VEGF       Free VEGF-A concentration in aqueous humor                              pg/mL                         
CMT     ANG2       Free Ang-2 concentration in aqueous humor                               pg/mL                         
PARAM   DISEASE    Disease indicator, 0 = nAMD, 1 = DME                                    .                             
PARAM   PHASE      Trial phase indicator, 2 = phase 2, 3 = phase 3                         .                             
PARAM   KVH        Elimination rate constant of faricimab from vitreous humor              1/day                         
PARAM   VVH        Volume of vitreous humor                                                mL                            
PARAM   KOUTV      Elimination rate constant for VEGF-A turnover                           1/day                         
PARAM   BASEDME    Baseline VEGF-A concentration in DME                                    pg/mL                         
PARAM   BASENAMD   Baseline VEGF-A concentration in nAMD                                   pg/mL                         
PARAM   EC50V      Faricimab VH concentration for 50% VEGF-A production inhibition         ug/mL                         
PARAM   HILLVP2    Hill coefficient for VEGF-A model, phase 2 studies                      .                             
PARAM   HILLVP3    Hill coefficient for VEGF-A model, phase 3 studies                      .                             
PARAM   EMAXV      Maximum inhibitory effect of faricimab on VEGF-A production             .                             
PARAM   HILLBASE   Exponent for effect of individual baseline VEGF-A on Hill coefficient   .                             
PARAM   KOUTA      Elimination rate constant for Ang-2 turnover                            1/day                         
PARAM   BASEA      Baseline Ang-2 concentration                                            pg/mL                         
PARAM   EC50A      Faricimab VH concentration for 50% Ang-2 production inhibition          ug/mL                         
PARAM   HILLA      Hill coefficient for Ang-2 model                                        .                             
PARAM   EMAXA      Maximum inhibitory effect of faricimab on Ang-2 production              fixed                         
OMEGA   EBASE      ETA on VEGF-A baseline                                                  observed baseline variability 
OMEGA   EEC50V     ETA on VEGF-A EC50                                                      .                             
OMEGA   EKOUTA     ETA on Ang-2 kout                                                       .                             
OMEGA   EEC50A     ETA on Ang-2 EC50                                                       .                             
OMEGA   EHILLA     ETA on Ang-2 Hill coefficient                                           .                             
SIGMA   EPROPV     Proportional residual variance for VEGF-A                               .                             
SIGMA   EADDA2     Additive residual variance for Ang-2, phase 2 studies                   .                             
SIGMA   EADDA3     Additive residual variance for Ang-2, phase 3 studies                   .                             

$PARAM
DISEASE = 0
PHASE = 3
KVH = 0.07
VVH = 4
KOUTV = 4.15
BASEDME = 114
BASENAMD = 52.8
EC50V = 2.59
HILLVP2 = 1.11
HILLVP3 = 0.655
EMAXV = 0.987
HILLBASE = 0.235
KOUTA = 0.708
BASEA = 12.1
EC50A = 0.0835
HILLA = 0.653
EMAXA = 1

$INIT
VH = 0
VEGF = 52.8
ANG2 = 12.1

$OMEGA
@block
@labels EBASE EEC50V
// row 1
0.401
// row 2
0
1.1

$OMEGA
@block
@labels EKOUTA EEC50A EHILLA
// row 1
0.807
// row 2
0
5.76
// row 3
0
0
0.0845

$SIGMA
@block
@labels EPROPV EADDA2 EADDA3
// row 1
0.401
// row 2
0
2.24
// row 3
0
0
3.9

$MAIN
// VH PK is a simplified single-compartment driver (full plasma/VH/AH PK model reported elsewhere; not detailed in this article)
double TVBASEV = DISEASE==1 ? BASEDME : BASENAMD;
double BASEV = TVBASEV * exp(EBASE);
double EC50Vi = EC50V * exp(EEC50V);
double HILLtyp = PHASE==2 ? HILLVP2 : HILLVP3;
// Covariate effect of individual baseline VEGF-A on Hill coefficient (power model)
double HILLVi = HILLtyp * pow(BASEV/TVBASEV, HILLBASE);
double KOUTVi = KOUTV;
double EMAXVi = EMAXV;
double KinV = BASEV * KOUTVi;
double KOUTAi = KOUTA * exp(EKOUTA);
double EC50Ai = EC50A * exp(EEC50A);
double HILLAi = HILLA * exp(EHILLA);
double BASEAi = BASEA;
double EMAXAi = EMAXA;
double KinA = BASEAi * KOUTAi;
VH_0 = 0;
VEGF_0 = BASEV;
ANG2_0 = BASEAi;
 
$ODE
double CVH = VH/VVH;
double EFFV = EMAXVi * pow(CVH, HILLVi) / (pow(EC50Vi, HILLVi) + pow(CVH, HILLVi));
double EFFA = EMAXAi * pow(CVH, HILLAi) / (pow(EC50Ai, HILLAi) + pow(CVH, HILLAi));
dxdt_VH = -KVH * VH;
dxdt_VEGF = KinV * (1 - EFFV) - KOUTVi * VEGF;
dxdt_ANG2 = KinA * (1 - EFFA) - KOUTAi * ANG2;
 
$TABLE
double CVHOUT = VH/VVH;
double IPREDV = VEGF;
double IPREDA = ANG2;
double DVVEGF = IPREDV * (1 + EPROPV);
double sigmaA = PHASE==2 ? EADDA2 : EADDA3;
double DVANG2 = IPREDA + (PHASE==2 ? EADDA2 : EADDA3);
 
$CAPTURE
CVHOUT
IPREDV
IPREDA
DVVEGF
DVANG2

