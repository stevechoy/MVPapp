$PROB
Population PK/PD model of donidalorsen (antisense oligonucleotide) for prophylaxis of hereditary angioedema; Diep et al., CPT Pharmacometrics Syst Pharmacol 2026;15:e70206

# Model Annotations: 

block   name         descr                                                                       unit                                        
------  -----------  --------------------------------------------------------------------------  --------------------------------------------
CMT     GUT          Subcutaneous absorption depot compartment                                   .                                           
CMT     CENT         Central compartment                                                         donidalorsen                                
CMT     PERIPH       Peripheral compartment                                                      donidalorsen                                
CMT     PKK          Prekallikrein response compartment                                          .                                           
PARAM   TVCL         Typical apparent clearance                                                  L/h                                         
PARAM   dCLWT        Weight exponent on CL/F                                                     .                                           
PARAM   TVVC         Typical apparent central volume                                             L                                           
PARAM   dVCWT        Weight exponent on Vc/F                                                     .                                           
PARAM   VCHAE        Fractional effect of HAE disease status on Vc/F                             .                                           
PARAM   TVQ          Typical apparent intercompartmental clearance                               L/h                                         
PARAM   dQWT         Weight exponent on Q/F                                                      .                                           
PARAM   QHAE         Fractional effect of HAE disease status on Q/F                              .                                           
PARAM   TVVP         Typical apparent peripheral volume                                          L                                           
PARAM   dVPWT        Weight exponent on Vp/F                                                     .                                           
PARAM   TVKA         Typical first-order absorption rate constant                                1/h                                         
PARAM   KAARM        Fractional effect of arm injection site on ka                               .                                           
PARAM   KADEV        Fractional effect of autoinjector presentation on ka                        .                                           
PARAM   TVBL         Typical baseline prekallikrein                                              mg/L                                        
PARAM   BLHAE        Fractional effect of HAE disease status on baseline PKK                     .                                           
PARAM   TVKOUT       Typical first-order loss rate constant for PKK                              1/h                                         
PARAM   TVIMAX       Maximal fractional inhibitory capacity                                      .                                           
PARAM   TVIC50       Typical concentration for half-maximal inhibition                           ng/mL                                       
PARAM   IC50HAE      Fractional effect of HAE disease status on IC50                             .                                           
PARAM   WT           Body weight                                                                 kg                                          
PARAM   HAE          HAE disease status indicator                                                0 = healthy volunteer, 1 = patient with HAE 
PARAM   ARM          Site of administration indicator                                            0 = abdomen/thigh, 1 = arm                  
PARAM   DEVICE       Drug presentation indicator                                                 0 = vial and syringe, 1 = autoinjector      
OMEGA   ECL          IIV on CL/F                                                                 .                                           
OMEGA   EVC          IIV on Vc/F                                                                 .                                           
OMEGA   EQ           IIV on Q/F                                                                  .                                           
OMEGA   EVP          IIV on Vp/F                                                                 .                                           
OMEGA   EKA          IIV on ka                                                                   .                                           
OMEGA   EBL          IIV on baseline PKK                                                         .                                           
OMEGA   EKOUT        IIV on kout                                                                 .                                           
OMEGA   EIC50        IIV on IC50                                                                 .                                           
SIGMA   SIGLOGPK     Additive residual variance for log-transformed donidalorsen concentration   .                                           
SIGMA   SIGPROPPKK   Proportional residual variance for PKK concentration                        .                                           

$PARAM
TVCL = 12.8
dCLWT = 1.52
TVVC = 69.8
dVCWT = 2.34
VCHAE = 0.426
TVQ = 2.58
dQWT = 1.79
QHAE = -0.261
TVVP = 1840
dVPWT = 1.6
TVKA = 0.952
KAARM = -0.338
KADEV = 0.262
TVBL = 139
BLHAE = -0.132
TVKOUT = 0.00266
TVIMAX = 0.992
TVIC50 = 0.158
IC50HAE = 0.77
WT = 70
HAE = 0
ARM = 0
DEVICE = 0

$INIT
GUT = 0
CENT = 0
PERIPH = 0
PKK = 139

$OMEGA
@block
@labels ECL EVC EQ EVP EKA
// row 1
0.0377
// row 2
0.0799
0.2988
// row 3
0.01347
0.04119
0.01035
// row 4
0.03497
0.12673
0.03851
0.169
// row 5
0.03026
0.16185
0.01831
0.05108
0.1682

$OMEGA
@block
@labels EBL EKOUT EIC50
// row 1
0.0649
// row 2
0
0.1257
// row 3
0
0
0.5251

$SIGMA
@block
@labels SIGLOGPK SIGPROPPKK
// row 1
0.0877
// row 2
0
0.0253

$MAIN
double CL = TVCL * pow(WT/70, dCLWT) * exp(ECL);
double VC = TVVC * pow(WT/70, dVCWT) * (1 + VCHAE*HAE) * exp(EVC);
double Q  = TVQ  * pow(WT/70, dQWT)  * (1 + QHAE*HAE)  * exp(EQ);
double VP = TVVP * pow(WT/70, dVPWT) * exp(EVP);
double KA = TVKA * (1 + KAARM*ARM) * (1 + KADEV*DEVICE) * exp(EKA);
double BL   = TVBL   * (1 + BLHAE*HAE) * exp(EBL);
double KOUT = TVKOUT * exp(EKOUT);
double KIN  = BL * KOUT;
double IMAX = TVIMAX;
double IC50 = TVIC50 * (1 + IC50HAE*HAE) * exp(EIC50);
// Baseline PKK is the initial condition for the response compartment
PKK_0 = BL;
 
$ODE
double CP  = CENT/VC;
double CP2 = PERIPH/VP;
dxdt_GUT    = -KA*GUT;
dxdt_CENT   = KA*GUT - CL*CP - Q*(CP - CP2);
dxdt_PERIPH = Q*(CP - CP2);
// Indirect response model: inhibition of PKK zero-order production by donidalorsen
dxdt_PKK    = KIN*(1 - IMAX*CP/(IC50 + CP)) - KOUT*PKK;
 
$TABLE
double IPRED = CENT/VC;
// Additive error model on log-transformed donidalorsen concentration data
double DV = IPRED * exp(EPS(1));
double IPREDPKK = PKK;
// Proportional error model for PKK concentration
double DVPKK = IPREDPKK * (1 + EPS(2));
 
$CAPTURE
IPRED
DV
IPREDPKK
DVPKK

