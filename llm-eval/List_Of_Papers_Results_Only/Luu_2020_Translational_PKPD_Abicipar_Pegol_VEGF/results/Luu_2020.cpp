$PROB
Mechanistic TMDD PK/PD model of abicipar pegol and VEGF inhibition (human model), Luu et al., J Pharmacol Exp Ther 2020

$CMT @annotated
VEGFAH : Free VEGF concentration in aqueous humor (uM)
VEGFV : Free VEGF concentration in vitreous (uM)
DRAH : Abicipar-VEGF complex concentration in aqueous humor (uM)
DRV : Abicipar-VEGF complex concentration in vitreous (uM)
AAH : Abicipar amount in aqueous humor (nmol)
AV : Abicipar amount in vitreous (nmol)
AR : Abicipar amount in retina (nmol)
AC : Abicipar amount in choroid (nmol)
AS : Abicipar amount in serum (nmol)

$PARAM @annotated
VEGFAH0 : 0.0000167 : Baseline free VEGF concentration in aqueous humor, AMD (uM)
VEGFV0 : 0.0000437 : Baseline free VEGF concentration in vitreous, DME (uM)
KDEG : 6.76 : VEGF degradation rate constant (1/day)
KON : 691200 : Second-order association rate constant (1/uM/day)
KD : 0.000000911 : Equilibrium dissociation constant (uM)
VV : 4.4 : Vitreous volume (mL)
VR : 0.326 : Retina volume (mL)
VC : 0.139 : Choroid volume (mL)
VAH : 0.25 : Aqueous humor volume (mL)
TVKVR : 11.5 : Typical vitreous-to-retina transfer rate constant (1/day)
TVKVA : 0.0256 : Typical vitreous-to-aqueous humor transfer rate constant (1/day)
TVKRV : 609 : Typical retina-to-vitreous transfer rate constant (1/day)
TVKRC : 25700 : Typical retina-to-choroid transfer rate constant (1/day)
TVKRS : 4620 : Typical retina-to-serum transfer rate constant (1/day)
TVKCR : 35500 : Typical choroid-to-retina transfer rate constant (1/day)
TVKCS : 6460 : Typical choroid-to-serum transfer rate constant (1/day)
TVKAS : 0.402 : Typical aqueous humor-to-serum transfer rate constant (1/day)
TVKSC : 18.8 : Typical serum-to-choroid transfer rate constant (1/day)
TVKS : 0.0563 : Typical serum elimination rate constant (1/day)
TVVS : 40495 : Typical serum apparent volume (mL)
TVKINT : 9.25 : Typical drug-target complex elimination rate constant, manually adjusted (1/day)

$MAIN
// Apply IIV (30% CV, i.e. omega = 0.09) to all estimated transfer/elimination parameters
double KVR  = TVKVR  * exp(EKVR);
double KVA  = TVKVA  * exp(EKVA);
double KRV  = TVKRV  * exp(EKRV);
double KRC  = TVKRC  * exp(EKRC);
double KRS  = TVKRS  * exp(EKRS);
double KCR  = TVKCR  * exp(EKCR);
double KCS  = TVKCS  * exp(EKCS);
double KAS  = TVKAS  * exp(EKAS);
double KSC  = TVKSC  * exp(EKSC);
double KS   = TVKS   * exp(EKS);
double VS   = TVVS   * exp(EVS);
double KINT = TVKINT * exp(EKINT);

// Derived binding and synthesis parameters
double KOFF   = KD * KON;
double KSYNAH = KDEG * VEGFAH0;
double KSYNV  = KDEG * VEGFV0;

// Initial conditions: VEGF at steady-state baseline levels
VEGFAH_0 = VEGFAH0;
VEGFV_0  = VEGFV0;

// Dose administered into AV (vitreous) as amount (nmol); convert IVT mg dose
// using abicipar MW ~34000 g/mol: nmol = dose_mg * 1e6 / 34000

$ODE
dxdt_VEGFAH = KSYNAH - KDEG*VEGFAH - KON*VEGFAH*(AAH/VAH) + KOFF*DRAH;
dxdt_VEGFV  = KSYNV  - KDEG*VEGFV  - KON*VEGFV*(AV/VV)   + KOFF*DRV;
dxdt_DRAH   = KON*VEGFAH*(AAH/VAH) - DRAH*(KOFF + KINT);
dxdt_DRV    = KON*VEGFV*(AV/VV)   - DRV*(KOFF + KINT);
dxdt_AAH    = -KON*VEGFAH*AAH + KOFF*DRAH*VAH - KAS*AAH + KVA*AV;
dxdt_AV     = -KON*VEGFV*AV   + KOFF*DRV*VV   - KVA*AV  - KVR*AV + KRV*AR;
dxdt_AR     = KVR*AV - KRV*AR - KRC*AR + KCR*AC - KRS*AR;
dxdt_AC     = KRC*AR - KCR*AC - KCS*AC + KSC*AS;
dxdt_AS     = KAS*AAH + KRS*AR + KCS*AC - KS*AS - KSC*AS;

$OMEGA @annotated
EKVR  : 0.09 : ETA on KVR
EKVA  : 0.09 : ETA on KVA
EKRV  : 0.09 : ETA on KRV
EKRC  : 0.09 : ETA on KRC
EKRS  : 0.09 : ETA on KRS
EKCR  : 0.09 : ETA on KCR
EKCS  : 0.09 : ETA on KCS
EKAS  : 0.09 : ETA on KAS
EKSC  : 0.09 : ETA on KSC
EKS   : 0.09 : ETA on KS
EVS   : 0.09 : ETA on VS
EKINT : 0.09 : ETA on KINT

$SIGMA @annotated
PROP : 0.09 : Proportional residual error on vitreous concentration (30% CV)

$TABLE
double CVIT  = AV/VV;
double CAH   = AAH/VAH;
double CRET  = AR/VR;
double CCHOR = AC/VC;
double CSER  = AS/VS;
double IPRED = CVIT;
double DV = IPRED*(1 + EPS(1));

$CAPTURE
CVIT CAH CRET CCHOR CSER IPRED DV