$PROB
Faricimab ocular and systemic three-compartment catenary population PK model following intravitreal administration (Diack et al., Transl Vis Sci Technol. 2024;13(11):14)

# Model Annotations: 

block   name         descr                                                  unit                       
------  -----------  -----------------------------------------------------  ---------------------------
CMT     VH           Vitreous humor compartment                             amount, mg                 
CMT     AH           Aqueous humor compartment                              amount, mg                 
CMT     PLASMA       Plasma compartment                                     amount, mg                 
PARAM   TVVA         Typical volume of AH compartment                       L                          
PARAM   TVVC         Typical volume of plasma compartment                   L                          
PARAM   TVKVH        Typical elimination rate constant, VH to AH            1/day                      
PARAM   TVKAH        Typical elimination rate constant, AH to plasma        1/day                      
PARAM   TVCL         Typical plasma clearance                               L/day                      
PARAM   VHVOL        Fixed volume of VH compartment                         L                          
PARAM   CLWT         Power exponent of body weight on CL                    .                          
PARAM   VCWT         Power exponent of body weight on VC                    .                          
PARAM   CLFEMALE     Multiplicative effect of female sex on CL              .                          
PARAM   CLFORM       Multiplicative effect of phase 1/2 formulation on CL   .                          
PARAM   KVHAGE       Power exponent of age on KVH                           .                          
PARAM   KVHADA       Multiplicative effect of ADA positivity on KVH         .                          
PARAM   SDPROP       Proportional residual error SD                         shared AH/plasma           
PARAM   SDPHASE1     Additive residual error SD, phase 1/2 studies          .                          
PARAM   SDPHASE2     Additive residual error SD, phase 3 studies            .                          
PARAM   WT           Body weight                                            kg                         
PARAM   AGE          Age                                                    years                      
PARAM   SEX          Sex                                                    0 = male, 1 = female       
PARAM   ADA          Anti-drug antibody status                              0 = negative, 1 = positive 
PARAM   STUDYPHASE   Study phase indicator                                  0 = phase 3, 1 = phase 1/2 
OMEGA   EKVH         ETA on KVH                                             .                          
OMEGA   EKAH         ETA on KAH, covariance with KVH                        .                          
OMEGA   ECL          ETA on CL, covariance with KVH and KAH                 .                          
OMEGA   EVC          ETA on VC                                              .                          
OMEGA   EEPS         ETA on residual error magnitude                        .                          
SIGMA   EPSAH        Residual error AH                                      fixed variance             
SIGMA   EPSPLASMA    Residual error plasma                                  fixed variance             

$PARAM
TVVA = 0.000253
TVVC = 1.48
TVKVH = 0.0929
TVKAH = 15.6
TVCL = 2.33
VHVOL = 0.0045
CLWT = 0.773
VCWT = 1
CLFEMALE = 0.863
CLFORM = 0.816
KVHAGE = -0.533
KVHADA = 1.3
SDPROP = 0.414
SDPHASE1 = 0.614
SDPHASE2 = 0.788
WT = 80
AGE = 65
SEX = 0
ADA = 0
STUDYPHASE = 0

$INIT
VH = 0
AH = 0
PLASMA = 0

$OMEGA
@block
@labels EKVH EKAH ECL
// row 1
0.087
// row 2
0.0251
0.24
// row 3
0
0.0311
0.0331

$OMEGA
@block
@labels EVC EEPS
// row 1
1.34
// row 2
0
0.086

$SIGMA
@block
@labels EPSAH EPSPLASMA
// row 1
1
// row 2
0
1

$MAIN
// Covariate models (normalized power/multiplicative relationships)
double TVVCi  = TVVC * pow(WT / 80.0, VCWT);
double TVCLi  = TVCL * pow(WT / 80.0, CLWT) * pow(CLFEMALE, SEX) * pow(CLFORM, STUDYPHASE);
double TVKVHi = TVKVH * pow(AGE / 65.0, KVHAGE) * pow(KVHADA, ADA);
double TVKAHi = TVKAH;
// Individual parameters with IIV
double VC  = TVVCi  * exp(EVC);
double CL  = TVCLi  * exp(ECL);
double KVH = TVKVHi * exp(EKVH);
double KAH = TVKAHi * exp(EKAH);
// Parameters without IIV
double VA  = TVVA;
double VVH = VHVOL;
 
$ODE
dxdt_VH     = -KVH * VH;
dxdt_AH     = KVH * VH - KAH * AH;
dxdt_PLASMA = KAH * AH - (CL / VC) * PLASMA;
 
$TABLE
double IPREDVH      = VH / VVH;
double IPREDAH      = AH / VA;
double IPREDPLASMA  = PLASMA / VC;
// Phase-dependent additive residual error term
double SDADD = (STUDYPHASE == 1) ? SDPHASE1 : SDPHASE2;
// Combined additive + proportional error model, scaled by an interindividual eta on residual error
double DVAH     = IPREDAH     + (SDPROP * IPREDAH     + SDADD) * EPSAH     * exp(EEPS);
double DVPLASMA = IPREDPLASMA + (SDPROP * IPREDPLASMA + SDADD) * EPSPLASMA * exp(EEPS);
 
$CAPTURE
IPREDVH
IPREDAH
IPREDPLASMA
DVAH
DVPLASMA

