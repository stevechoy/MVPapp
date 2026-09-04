$PROB
Type 2 diabetes disease progression model: weight, insulin sensitivity, FPG and HbA1c turnover with logistic beta-cell function decline (NONMEM run 646/649, S. Choy)

# Model Annotations: 

block   name         descr                                                                   unit                                
------  -----------  ----------------------------------------------------------------------  ------------------------------------
CMT     WEIGHT       Body weight compartment                                                 kg                                  
CMT     INSULIN      Insulin compartment                                                     baseline steady-state, uU/mL        
CMT     FPG          Fasting plasma glucose compartment                                      baseline steady-state, mmol/L       
CMT     HBA1C        HbA1c transit compartment 1                                             %                                   
CMT     HBA2C        HbA1c transit compartment 2                                             %                                   
CMT     HBA3C        HbA1c transit compartment 3                                             %                                   
PARAM   TVBLWT       Typical baseline body weight                                            kg                                  
PARAM   TVKOUTHL     Half-life of weight compartment                                         days                                
PARAM   TVEFDE       Typical effect of diet and exercise counseling                          .                                   
PARAM   TVEFPL       Typical effect of placebo                                               .                                   
PARAM   TVEFUPS      Typical loss of effect of D&E and placebo per year                      %/year                              
PARAM   TVBC0        Typical baseline beta-cell function                                     logit scale                         
PARAM   TVRB         Typical rate of beta-cell function decline per year                     logits                              
PARAM   TVEFBMAX     Typical maximal relative increase on beta-cell function                 .                                   
PARAM   TVSEFBI      Shape of sigmoidal logistic increase function                           .                                   
PARAM   TVEFB50      Typical time for half of EFB logistic decline                           days                                
PARAM   TVIS0        Typical baseline insulin sensitivity                                    logit scale                         
PARAM   TVSCALEIS    Typical scaling factor of weight change effect on insulin sensitivity   .                                   
PARAM   TVRESWT      Typical proportional weight residual error magnitude                    .                                   
PARAM   TVRESFSI     Typical proportional FSI residual error magnitude                       .                                   
PARAM   TVRESFPG     Typical proportional FPG residual error magnitude                       .                                   
PARAM   TVRESHBA     Typical proportional HbA1c residual error magnitude                     .                                   
PARAM   TVMTT        Mean transit time for HbA1c                                             days                                
PARAM   TVKIHB       Kin HbA1c                                                               %/day per L/mmol                    
PARAM   TVINTCPT     Residual amount of HbA1c independent of FPG                             .                                   
PARAM   TVPPGSCALE   Reduction scale of PPG effect after time 0                              .                                   
PARAM   TVSEFBD      Shape of sigmoidal logistic decrease function                           .                                   
PARAM   KIOI         kin/kout insulin ratio corresponding to ss FSI 5uU/mL                   .                                   
PARAM   FPGSS        Healthy reference FPG from HOMA                                         mmol/L                              
PARAM   NC           Number of HbA1c transit compartments                                    .                                   
PARAM   BSL          Baseline/day covariate                                                  from data, day placebo administered 
OMEGA   EEFBMAX      ETA on EFBMAX                                                           .                                   
OMEGA   EBC0         ETA on BC0                                                              .                                   
OMEGA   ERB          ETA on RB                                                               .                                   
OMEGA   EIS0         ETA on IS0                                                              .                                   
OMEGA   EINTCPT      ETA on INTCPT                                                           .                                   
OMEGA   ESCALEIS     ETA on SCALEIS                                                          .                                   
OMEGA   EBLWT        ETA on BLWT                                                             .                                   
OMEGA   EEFDE        ETA on EFDE                                                             .                                   
OMEGA   EEFPL        ETA on EFPL                                                             .                                   
OMEGA   EEFUPS       ETA on EFUPS                                                            .                                   
OMEGA   ERESFSI      ETA on RESFSI                                                           .                                   
OMEGA   ERESFPG      ETA on RESFPG                                                           .                                   
OMEGA   ERESHBA      ETA on RESHBA                                                           .                                   
OMEGA   EEFB50       ETA on EFB50                                                            .                                   
SIGMA   EPS1         Residual error variance component                                       fixed                               

$PARAM
TVBLWT = 104.476
TVKOUTHL = 93.213
TVEFDE = 3.92467
TVEFPL = 2.33014
TVEFUPS = 2.83859
TVBC0 = -0.43933
TVRB = 0.184335
TVEFBMAX = 0.175301
TVSEFBI = -5
TVEFB50 = 180
TVIS0 = 1.11364
TVSCALEIS = 0.0559321
TVRESWT = 0.00920946
TVRESFSI = 0.260662
TVRESFPG = 0.0686797
TVRESHBA = 0.0238722
TVMTT = 36.5424
TVKIHB = 0.0137999
TVINTCPT = 0.074282
TVPPGSCALE = 0.970369
TVSEFBD = 5
KIOI = 7.8
FPGSS = 4.5
NC = 3
BSL = 1

$INIT
WEIGHT = 104.476
INSULIN = 18.9465239095652
FPG = 7.49448123181113
HBA1C = 2.16459017916284
HBA2C = 2.16459017916284
HBA3C = 2.16459017916284

$OMEGA
@block
@labels EEFBMAX EBC0 ERB EIS0 EINTCPT ESCALEIS EBLWT EEFDE EEFPL EEFUPS
// row 1
0.233301
// row 2
-0.284251
1.39504
// row 3
0.070311
-0.218421
0.216724
// row 4
0.212939
-0.358478
0.100076
0.308959
// row 5
0.00278359
-0.0187842
-0.00581572
-0.0110792
0.0244959
// row 6
0.129238
-0.283052
0.109508
0.224599
0.0274123
0.412589
// row 7
0.0387983
-0.0374141
0.0117303
0.0345632
-0.00466022
0.0204856
0.0216277
// row 8
0.0574601
1.89831
-0.807979
-0.857973
0.0128065
-0.804342
-0.168829
31.5631
// row 9
0.46376
-1.36275
-0.46601
0.826763
0.027455
0.415386
0.0686122
-13.5439
37.4116
// row 10
-0.0884547
0.221797
-1.42542
-0.0320875
-0.0299654
-1.12837
-0.137252
10.879
27.4577
69.9295

$OMEGA
@block
@labels ERESFSI ERESFPG
// row 1
0.102629
// row 2
0.0420507
0.0656939

$OMEGA
@block
@labels ERESHBA EEFB50
// row 1
0.0239873
// row 2
0
0.0924312

$SIGMA
@block
@labels EPS1
// row 1
1

$MAIN
// Weight parameters
double BLWT = TVBLWT * exp(EBLWT);
double Kout = log(2.0)/TVKOUTHL;
double Kin  = Kout * BLWT;
// D&E + Placebo and loss of effect
double OCC1 = 0;
double OCC2 = 0;
if(TIME > 1)   OCC1 = 1;
if(TIME > BSL) OCC2 = 1;
double EFDE = TVEFDE + EEFDE;
double EFPL = TVEFPL + EEFPL;
double EFDEPL = EFDE*OCC1 + EFPL*OCC2;
double EFUPS = TVEFUPS + EEFUPS;
double EFUP = (100.0 + EFUPS*TIME/365.0)/100.0;
double EFFW = EFUP * (100.0 - EFDEPL)/100.0;
// Beta-cell parameters
double BC0 = TVBC0 + EBC0;
double BCE0 = 1.0/(1.0 + exp(BC0));
double RB = TVRB + ERB;
double EFBMAX = TVEFBMAX * exp(EEFBMAX);
double SEFBI = TVSEFBI;
double SEFBD = TVSEFBD;
double EFB50 = TVEFB50 * exp(EEFB50);
double BF = 1.0/(1.0 + exp(BC0 + RB*TIME/365.0));
double EFBI = 0;
if(TIME > 0) EFBI = EFBMAX/(1.0 + pow(TIME/BSL, SEFBI));
double EFBD = 0;
if(TIME > 0) EFBD = EFBI/(1.0 + pow(TIME/EFB50, SEFBD));
double EFFB = 1.0 + EFBD;
double BNET = EFFB * BF;
// Insulin parameters
double IS0 = TVIS0 + EIS0;
double ISS0 = 1.0/(1.0 + exp(IS0));
double SCALEIS = TVSCALEIS * exp(ESCALEIS);
// Glucose parameters
double KIOG = FPGSS * KIOI;
double THRESH = FPGSS - 1.0;
// HbA1c parameters
double MTT = TVMTT;
double KIHB = TVKIHB;
double KOHB = NC/MTT;
double INTCPT = TVINTCPT * exp(EINTCPT);
double PPG = INTCPT;
if(TIME > 0) PPG = INTCPT * TVPPGSCALE;
// Baseline functions
double B = THRESH * BCE0 * KIOI;
double C = -BCE0 * KIOI * KIOG / ISS0;
double BLI = (-B + sqrt(B*B - 4.0*C))/2.0;
double BLG = KIOG/(ISS0*BLI);
double BLHB = (INTCPT + KIHB*BLG)/KOHB * NC;
// Residual error parameters
double RESWT  = TVRESWT;
double RESFSI = TVRESFSI * exp(ERESFSI);
double RESFPG = TVRESFPG * exp(ERESFPG);
double RESHBA = TVRESHBA * exp(ERESHBA);
// Initial conditions
WEIGHT_0 = BLWT;
INSULIN_0 = BLI;
FPG_0 = BLG;
HBA1C_0 = BLHB/NC;
HBA2C_0 = BLHB/NC;
HBA3C_0 = BLHB/NC;
 
$ODE
// Weight change from baseline (drives insulin sensitivity effect)
double DWT = BLWT - WEIGHT;
double EFFS = 1.0 + SCALEIS * DWT;
double B1 = THRESH * BF * EFFB * KIOI;
double C1 = -BF * EFFB * KIOI * KIOG / (ISS0 * EFFS);
double FSI = (-B1 + sqrt(B1*B1 - 4.0*C1))/2.0;
if(FSI < 0) FSI = 1;
double FPGX = KIOG/(EFFS * ISS0 * FSI);
dxdt_WEIGHT  = Kin*EFFW - Kout*WEIGHT;
dxdt_INSULIN = 0;
dxdt_FPG     = 0;
dxdt_HBA1C   = PPG + KIHB*FPGX - KOHB*HBA1C;
dxdt_HBA2C   = KOHB*HBA1C - KOHB*HBA2C;
dxdt_HBA3C   = KOHB*HBA2C - KOHB*HBA3C;
 
$TABLE
// Recompute observed states at observation time
double EWT = WEIGHT;
if(EWT <= 0) EWT = 0.00001;
double EHB = HBA1C + HBA2C + HBA3C;
if(EHB <= 0) EHB = 0.00001;
double DWTE = BLWT - WEIGHT;
double DWTPE = WEIGHT/BLWT;
double EEFS = 1.0 + SCALEIS * DWTE;
double B2 = THRESH * BF * EFFB * KIOI;
double C2 = -BF * EFFB * KIOI * KIOG / (ISS0 * EEFS);
double EFSI = (-B2 + sqrt(B2*B2 - 4.0*C2))/2.0;
if(EFSI < 1) EFSI = 1;
double EFPG = KIOG/(EEFS * ISS0 * EFSI);
if(EFPG <= 0) EFPG = 0.00001;
// Log-scale individual predictions for each biomarker type
double WTIPRED  = log(EWT);
double FSIIPRED = log(EFSI);
double FPGIPRED = log(EFPG);
double HBAIPRED = log(EHB);
// Residual error (log-scale additive with type-specific magnitude)
double WTDV  = WTIPRED  + RESWT*EPS1;
double FSIDV = FSIIPRED + RESFSI*EPS1;
double FPGDV = FPGIPRED + RESFPG*EPS1;
double HBADV = HBAIPRED + RESHBA*EPS1;
// Diagnostic outputs
double ISSE = ISS0 * EEFS * 100.0;
double BSSE = BNET * 100.0;
double IGR = EFSI/EFPG;
double ISSEKG = 0;
if(DWTE != 0) ISSEKG = (ISSE - (ISS0*100.0))/DWTE;
 
$CAPTURE
WTIPRED
FSIIPRED
FPGIPRED
HBAIPRED
WTDV
FSIDV
FPGDV
HBADV
ISSE
BSSE
IGR
ISSEKG
EWT
EHB
EFSI
EFPG

