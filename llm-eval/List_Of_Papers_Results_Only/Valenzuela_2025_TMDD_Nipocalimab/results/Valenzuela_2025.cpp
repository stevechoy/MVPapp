$PROB
Nipocalimab PK/FcRn Occupancy/IgG/MG-ADL Model in Generalized Myasthenia Gravis (Valenzuela et al., CPT Pharmacometrics Syst Pharmacol 2025;14:2074-2085)

# Model Annotations: 

block   name           descr                                                        unit                               
------  -------------  -----------------------------------------------------------  -----------------------------------
CMT     CENT           Central compartment total nipocalimab amount                 .                                  
CMT     PERIPH         Peripheral compartment total nipocalimab amount              .                                  
CMT     RTOT           Total FcRn receptor concentration                            .                                  
CMT     IGG            Total serum IgG concentration                                .                                  
CMT     IGGEC          IgG effect compartment                                       .                                  
PARAM   CL             Linear serum clearance at reference body weight              L/d                                
PARAM   VC             Central volume of distribution at reference body weight      L                                  
PARAM   Q              Intercompartmental clearance at reference body weight        L/d                                
PARAM   VP             Peripheral volume of distribution at reference body weight   L                                  
PARAM   WT             Individual body weight                                       kg                                 
PARAM   WTREF          Reference body weight for allometric scaling                 kg                                 
PARAM   FCRN0          Baseline total FcRn concentration                            nmol/L                             
PARAM   FRMAX          Maximal fraction of FcRn available to bind nipocalimab       .                                  
PARAM   KSS            Quasi-steady-state dissociation constant                     ug/mL                              
PARAM   KINT           Internalization rate constant of nipocalimab-FcRn complex    1/d                                
PARAM   KDEG           Degradation rate constant of free FcRn                       1/d                                
PARAM   MW             Molecular weight of nipocalimab, used for unit conversion    g/mol                              
PARAM   IGG0           Baseline total serum IgG                                     g/L                                
PARAM   FRIGG0GMG      Fraction of IgG0 for study MOM-M281-004                      Vivacity-MG, gMG                   
PARAM   GMG            Indicator for gMG study population                           1 = yes                            
PARAM   KDEGIGG        IgG degradation rate constant without FcRn recycling         1/d                                
PARAM   IGK            FcRn-mediated IgG recycling parameter                        .                                  
PARAM   KE0            IgG effect compartment rate constant                         1/d                                
PARAM   SIGG           Slope of IgG effect on MG-ADL                                points/10% IgG reduction, ref BL=7 
PARAM   EIGG           Exponent of MG-ADL baseline effect on SIgG                   .                                  
PARAM   IDECPLACEBO    Initial placebo decrease in MG-ADL after treatment start     points, ref BL=7                   
PARAM   EADL           Exponent of MG-ADL baseline effect on IDecplacebo            .                                  
PARAM   SPLACEBO       Slope of placebo effect on MG-ADL                            points/week                        
PARAM   MGADL0         Baseline MG-ADL score                                        points                             
PARAM   TRTFLAG        Treatment flag                                               1 = nipocalimab, 0 = placebo       
OMEGA   ECL            ETA on CL                                                    .                                  
OMEGA   EVC            ETA on Vc                                                    .                                  
OMEGA   EFCRN0         ETA on FcRn0                                                 .                                  
OMEGA   EIGG0          ETA on IgG0                                                  .                                  
OMEGA   EKDEGIGG       ETA on kdeg_IgG                                              .                                  
OMEGA   EIGK           ETA on IgK                                                   .                                  
OMEGA   EIDECPLACEBO   ETA on IDecplacebo                                           .                                  
OMEGA   ESIGG          ETA on SIgG                                                  .                                  
OMEGA   ESPLACEBO      ETA on Splacebo                                              .                                  
SIGMA   EPSADDPK       Additive residual error, PK                                  ug/mL                              
SIGMA   EPSPROPPK      Proportional residual error, PK                              .                                  
SIGMA   EPSADDRO       Additive residual error, RO                                  %                                  
SIGMA   EPSPROPRO      Proportional residual error, RO                              .                                  
SIGMA   EPSPROPIGG     Proportional residual error, total serum IgG                 .                                  
SIGMA   EPSADDMGADL    Additive residual error, MG-ADL                              points                             

$PARAM
CL = 0.655
VC = 3.23
Q = 0.25
VP = 0.622
WT = 75
WTREF = 75
FCRN0 = 143
FRMAX = 0.947
KSS = 6.05
KINT = 62.4
KDEG = 1.3
MW = 145000
IGG0 = 11.4
FRIGG0GMG = 0.777
GMG = 0
KDEGIGG = 0.217
IGK = 5.08
KE0 = 0.414
SIGG = -0.216
EIGG = 0.871
IDECPLACEBO = -1.08
EADL = 1.23
SPLACEBO = -0.0594
MGADL0 = 7.87
TRTFLAG = 1

$INIT
CENT = 0
PERIPH = 0
RTOT = 135.421
IGG = 11.4
IGGEC = 0

$OMEGA
@block
@labels ECL EVC EFCRN0 EIGG0 EKDEGIGG EIGK
// row 1
0.058081
// row 2
0
0.0225
// row 3
0
0
0.0625
// row 4
0
0
0
0.047961
// row 5
0
0
0
0
0.028561
// row 6
0
0
0
0
0
0.069169

$OMEGA
@block
@labels EIDECPLACEBO ESIGG
// row 1
0.0004167
// row 2
-0.0001217
0.00006596

$OMEGA
@block
@labels ESPLACEBO
// row 1
0.00005513

$SIGMA
@block
@labels EPSADDPK EPSPROPPK EPSADDRO EPSPROPRO EPSPROPIGG EPSADDMGADL
// row 1
0.198025
// row 2
0
0.00695556
// row 3
0
0
8.8804
// row 4
0
0
0
0.051529
// row 5
0
0
0
0
0.00736164
// row 6
0
0
0
0
0
2.25

$MAIN
// Allometrically scaled PK parameters with IIV
double CLi = CL*pow(WT/WTREF,0.75)*exp(ECL);
double VCi = VC*pow(WT/WTREF,1.0)*exp(EVC);
double Qi  = Q*pow(WT/WTREF,0.75);
double VPi = VP*pow(WT/WTREF,1.0);
double FCRN0i = FCRN0*exp(EFCRN0);
double IGG0i  = IGG0*exp(EIGG0)*(GMG==1 ? FRIGG0GMG : 1.0);
double KDEGIGGi = KDEGIGG*exp(EKDEGIGG);
double IGKi   = IGK*exp(EIGK);
double SIGGi = SIGG + ESIGG;
double IDECPLACEBOi = IDECPLACEBO + EIDECPLACEBO;
double SPLACEBOi = SPLACEBO + ESPLACEBO;
// Unit conversion factor (nmol per mg) using nipocalimab molecular weight
double FACTOR = 1e6/MW;
// FcRn synthesis rate (baseline steady-state, no drug present)
double KSYN = KDEG*FCRN0i*FRMAX;
// IgG recycling and synthesis rates
double KRECIGG = KDEGIGGi*IGKi/(1+IGKi);
double KSYNIGG = (KDEGIGGi - KRECIGG)*IGG0i;
// Initial conditions
RTOT_0 = FCRN0i*FRMAX;
IGG_0  = IGG0i;
 
$ODE
// Convert total drug concentration and Kss to molar (nM) units for the QSS solution
double CTOTnM = (CENT/VCi)*FACTOR;
double KSSnM  = KSS*FACTOR;
double DIFF   = CTOTnM - KSSnM - RTOT;
double DISC   = DIFF*DIFF + 4*CTOTnM*KSSnM;
double CFREEnM = (DIFF + sqrt(DISC))/2;
double RCCOMPLEX = RTOT*CFREEnM/(KSSnM+CFREEnM);
double CFREEmg = CFREEnM/FACTOR;
dxdt_CENT   = -CLi*CFREEmg - KINT*VCi*(RCCOMPLEX/FACTOR) - Qi*CFREEmg + (Qi/VPi)*PERIPH;
dxdt_PERIPH = Qi*CFREEmg - (Qi/VPi)*PERIPH;
dxdt_RTOT   = KSYN - KDEG*RTOT - (KINT-KDEG)*RCCOMPLEX;
double RFREE = RTOT - RCCOMPLEX + FCRN0i*(1-FRMAX);
double FFREE = RFREE/FCRN0i;
dxdt_IGG   = KSYNIGG - (KDEGIGGi - KRECIGG*FFREE)*IGG;
dxdt_IGGEC = KE0*((1 - IGG/IGG0i) - IGGEC);
 
$TABLE
// Recompute QSS quantities at observation times for reporting
double CTOTnMt = (CENT/VCi)*FACTOR;
double KSSnMt  = KSS*FACTOR;
double DIFFt   = CTOTnMt - KSSnMt - RTOT;
double DISCt   = DIFFt*DIFFt + 4*CTOTnMt*KSSnMt;
double CFREEnMt = (DIFFt + sqrt(DISCt))/2;
double RCCOMPLEXt = RTOT*CFREEnMt/(KSSnMt+CFREEnMt);
double RFREEt = RTOT - RCCOMPLEXt + FCRN0i*(1-FRMAX);
double FFREEt = RFREEt/FCRN0i;
double IPREDPK = CENT/VCi;
double IPREDRO = 100*(1-FFREEt);
double IGGPCFB = (IGG/IGG0i - 1)*100;
double BL = MGADL0;
double TSFD = TIME;
double IDec = IDECPLACEBOi*pow(BL/7.0, EADL);
double MGADLPLACEBO = (TSFD<=0) ? 0.0 : (IDec + SPLACEBOi*TSFD/7.0);
double MGADLIGGEFF = SIGGi*IGGEC*pow(BL/7.0, EIGG);
double MGADLCFB = MGADLPLACEBO + MGADLIGGEFF*TRTFLAG;
double DVPK = IPREDPK*(1+EPSPROPPK) + EPSADDPK;
double DVRO = IPREDRO*(1+EPSPROPRO) + EPSADDRO;
double DVIGG = IGG*(1+EPSPROPIGG);
double DVMGADL = MGADLCFB + EPSADDMGADL;
 
$CAPTURE
IPREDPK
IPREDRO
IGGPCFB
MGADLCFB
DVPK
DVRO
DVIGG
DVMGADL

