$PROB
QSP model of bispecific T cell engager (TCE) - TCE pharmacokinetics and CEA/CD3 ternary complex formation in tumor compartment. Ma H et al., AAPS J. 2020;22:85.

# Model Annotations: 

block   name                descr                                                unit                              
------  ------------------  ---------------------------------------------------  ----------------------------------
CMT     TCEC                TCE concentration in central compartment             M                                 
CMT     TCEP                TCE concentration in peripheral compartment          M                                 
CMT     TCET                TCE concentration in tumor compartment               M                                 
CMT     TCELN               TCE concentration in TDLN compartment                M                                 
CMT     CEATCE              CEA_TCE dimer density in tumor                       molecule/um2                      
CMT     CEACEATCE           CEACEA_TCE dimer density in tumor                    molecule/um2                      
CMT     TEFFCD3TCE          TeffCD3_TCE dimer density in tumor                   molecule/um2                      
CMT     TREGCD3TCE          TregCD3_TCE dimer density in tumor                   molecule/um2                      
CMT     CEATCETEFFCD3       CEA_TCE_TeffCD3 moTTC density in tumor               molecule/um2                      
CMT     CEATCETREGCD3       CEA_TCE_TregCD3 moTTC density in tumor               molecule/um2                      
CMT     CEACEATCETEFFCD3    CEACEA_TCE_TeffCD3 biTTC (trimer) density in tumor   molecule/um2                      
CMT     CEACEATCETREGCD3    CEACEA_TCE_TregCD3 biTTC (trimer) density in tumor   molecule/um2                      
PARAM   VC                  Central compartment volume                           L                                 
PARAM   VP                  Peripheral compartment volume                        L, assumed                        
PARAM   VT                  Tumor compartment volume                             L, assumed, ~3 cm diameter sphere 
PARAM   VLN                 TDLN compartment volume                              L, assumed                        
PARAM   qP                  Central-peripheral flow                              L/s, assumed from 0.5 L/day       
PARAM   qLN                 Central-TDLN flow                                    L/s, assumed from 0.1 L/day       
PARAM   qT                  Central-tumor flow                                   L/s, assumed from 0.1 L/day       
PARAM   qLD                 Lymphatic drainage flow tumor-TDLN                   L/s, assumed from 0.05 L/day      
PARAM   CL                  Systemic clearance of TCE                            L/s, assumed from 0.5 L/day       
PARAM   konCEATCE           On rate constant CEA/TCE binding)                    1/M*s                             
PARAM   koffCEATCE          Off rate constant CEA/TCE binding                    1/s                               
PARAM   konCD3TCE           On rate constant CD3/TCE binding)                    1/M*s                             
PARAM   koffCD3TCE          Off rate constant CD3/TCE binding                    1/s                               
PARAM   lambda              Intrinsic antibody cross-arm binding efficiency      dimensionless                     
PARAM   Dsyn                Immunological synapse gap distance                   um, assumed ~15 nm                
PARAM   NAv                 Avogadro constant                                    1/mol                             
PARAM   CCEAtotal           Total CEA per cancer cell                            sites/cell                        
PARAM   TeffCD3total        Total CD3 per Teff cell                              sites/cell                        
PARAM   TregCD3total        Total CD3 per Treg cell                              sites/cell                        
PARAM   SACcell             Surface area of cancer cell                          um2, assumed sphere d=20um        
PARAM   SATcell             Surface area of T cell                               um2, assumed sphere d=7um         
PARAM   ftum                Porosity in the tumor compartment                    dimensionless, assumed            
PARAM   KCEACEATCETeffCD3   Sensitivity of TKR Hill fn to CEACEA_TCE_TeffCD3     molecule/um2, assumed             
PARAM   KCEACEATCETregCD3   Sensitivity of TDR Hill fn to CEACEA_TCE_TregCD3     molecule/um2, assumed             
PARAM   nCEATCE             Hill coefficient for CEATCE-derived Hill equations   dimensionless                     
OMEGA   ECL                 No inter-individual variability described in text    placeholder                       
SIGMA   PROP                No residual error model described in text            placeholder                       

$PARAM
VC = 3.45
VP = 5
VT = 0.0141
VLN = 0.1
qP = 0.000005787
qLN = 0.000001157
qT = 0.000001157
qLD = 0.0000005787
CL = 0.000005787
konCEATCE = 1000
koffCEATCE = 0.00013
konCD3TCE = 10000
koffCD3TCE = 0.00075
lambda = 1000
Dsyn = 0.0015
NAv = 6.022e+23
CCEAtotal = 20000
TeffCD3total = 61000
TregCD3total = 61000
SACcell = 1256.6
SATcell = 153.9
ftum = 0.2
KCEACEATCETeffCD3 = 10
KCEACEATCETregCD3 = 10
nCEATCE = 3

$INIT
TCEC = 0
TCEP = 0
TCET = 0
TCELN = 0
CEATCE = 0
CEACEATCE = 0
TEFFCD3TCE = 0
TREGCD3TCE = 0
CEATCETEFFCD3 = 0
CEATCETREGCD3 = 0
CEACEATCETEFFCD3 = 0
CEACEATCETREGCD3 = 0

$OMEGA
@block
@labels ECL
// row 1
0

$SIGMA
@block
@labels PROP
// row 1
0

$MAIN
// Receptor surface densities (assumed constant over the simulated timeframe)
double DCEA = CCEAtotal / SACcell;
double DTeffCD3 = TeffCD3total / SATcell;
double DTregCD3 = TregCD3total / SATcell;
 
$ODE
double TCE = TCET / ftum;
// PK: two-compartment model with tumor and TDLN (Eqs 10-13)
dxdt_TCEC = (qP*(TCEP - TCEC) + qLN*(TCELN - TCEC) + qT*(TCET - TCEC) + qLD*TCELN - CL*TCEC) / VC;
dxdt_TCEP = qP*(TCEC - TCEP) / VP;
dxdt_TCET = (qT*(TCEC - TCET) - qLD*TCET) / VT;
dxdt_TCELN = (qLN*(TCEC - TCELN) + qLD*TCET - qLD*TCELN) / VLN;
// TCE-CEA-CD3 ternary complex module (Eqs 4-8-2)
dxdt_CEATCE = 2*konCEATCE*DCEA*TCE - koffCEATCE*CEATCE
  - (lambda/(Dsyn*NAv))*konCEATCE*CEATCE*DCEA + 2*koffCEATCE*CEACEATCE
  - konCD3TCE*CEATCE*DTeffCD3 + koffCD3TCE*CEATCETEFFCD3
  - konCD3TCE*CEATCE*DTregCD3 + koffCD3TCE*CEATCETREGCD3;
dxdt_CEACEATCE = (lambda/(Dsyn*NAv))*konCEATCE*CEATCE*DCEA - 2*koffCEATCE*CEACEATCE
  - konCD3TCE*CEACEATCE*DTeffCD3 + koffCD3TCE*CEACEATCETEFFCD3
  - konCD3TCE*CEACEATCE*DTregCD3 + koffCD3TCE*CEACEATCETREGCD3;
dxdt_TEFFCD3TCE = konCD3TCE*DTeffCD3*TCE - koffCD3TCE*TEFFCD3TCE
  - 2*konCEATCE*TEFFCD3TCE*DCEA + koffCEATCE*CEATCETEFFCD3;
dxdt_TREGCD3TCE = konCD3TCE*DTregCD3*TCE - koffCD3TCE*TREGCD3TCE
  - 2*konCEATCE*TREGCD3TCE*DCEA + koffCEATCE*CEATCETREGCD3;
dxdt_CEATCETEFFCD3 = konCD3TCE*CEATCE*DTeffCD3 - koffCD3TCE*CEATCETEFFCD3
  + 2*konCEATCE*TEFFCD3TCE*DCEA - koffCEATCE*CEATCETEFFCD3
  - (lambda/(Dsyn*NAv))*konCEATCE*CEATCETEFFCD3*DCEA + 2*koffCEATCE*CEACEATCETEFFCD3;
dxdt_CEATCETREGCD3 = konCD3TCE*CEATCE*DTregCD3 - koffCD3TCE*CEATCETREGCD3
  + 2*konCEATCE*TREGCD3TCE*DCEA - koffCEATCE*CEATCETREGCD3
  - (lambda/(Dsyn*NAv))*konCEATCE*CEATCETREGCD3*DCEA + 2*koffCEATCE*CEACEATCETREGCD3;
dxdt_CEACEATCETEFFCD3 = (lambda/(Dsyn*NAv))*konCEATCE*CEATCETEFFCD3*DCEA - 2*koffCEATCE*CEACEATCETEFFCD3
  + konCD3TCE*CEACEATCE*DTeffCD3 - koffCD3TCE*CEACEATCETEFFCD3;
dxdt_CEACEATCETREGCD3 = (lambda/(Dsyn*NAv))*konCEATCE*CEATCETREGCD3*DCEA - 2*koffCEATCE*CEACEATCETREGCD3
  + konCD3TCE*CEACEATCE*DTregCD3 - koffCD3TCE*CEACEATCETREGCD3;
 
$TABLE
// IPRED reflects simulated central compartment TCE concentration (Fig. S1)
double IPRED = TCEC;
double DV = IPRED*(1 + PROP);
// Hill functions translating biTTC density to Teff-mediated killing (Eq.9) and Treg-mediated exhaustion (Eq.11)
double HCEAC1T1 = pow(CEACEATCETEFFCD3, nCEATCE) / (pow(CEACEATCETEFFCD3, nCEATCE) + pow(KCEACEATCETeffCD3, nCEATCE));
double HCEAC1T0 = pow(CEACEATCETREGCD3, nCEATCE) / (pow(CEACEATCETREGCD3, nCEATCE) + pow(KCEACEATCETregCD3, nCEATCE));
 
$CAPTURE
IPRED
DV
HCEAC1T1
HCEAC1T0

