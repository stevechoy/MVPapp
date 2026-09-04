$PROB
Synthetic delay-differential model of the glucose-insulin (G-I) system during an OGTT (Contreras et al., Front. Bioeng. Biotechnol. 2020, doi:10.3389/fbioe.2020.00195)

$CMT @annotated
STOM : Amount of glucose in the stomach
JEJ  : Amount of glucose in the jejunum
ILE  : Amount of glucose in the ileum
GLU  : Blood glucose concentration
INS  : Blood insulin concentration

$PARAM @annotated
TVKJS    : 0.05   : 1/min : Kinetic constant for stomach emptying
TVKGJ    : 0.05   : 1/min : Kinetic constant for glucose absorption in jejunum
TVKJL    : 0.05   : 1/min : Kinetic constant for glucose delivery from jejunum to ileum
TVTAU    : 20     : min : Time delay between glucose disappearance in jejunum and appearance in ileum
TVKGL    : 0.03   : 1/min : Kinetic constant for glucose absorption in ileum
TVKXG    : 0.01   : 1/min : Kinetic constant for basal (insulin-independent) glucose consumption
TVKXGI   : 0.001  : 1/min per pM : Kinetic constant for insulin-induced glucose consumption (insulin sensitivity)
TVETABIO : 0.02   : 1/min per pM : Bioavailability of the intestinally absorbed glucose
TVKLAM   : 500    : mM2/min : Kinetic constant for hepatic glucose release rate
TVFGI    : 0.0005 : min(dm-1)3 : Incretin action conversion factor
TVKXI    : 0.05   : 1/min : Kinetic constant for insulin degradation
TVBETA   : 1      : unitless : Scale for insulin production saturation
TVGAMMA  : 2      : unitless : Scale for insulin production acceleration
Gb       : 5      : mM : Basal (fasting) blood glucose concentration
Ib       : 50     : pM : Basal (fasting) blood insulin concentration
D        : 416    : mmol : Ingested oral glucose dose (75 g bolus)

$MAIN
KJS    = TVKJS;
KGJ    = TVKGJ;
KJL    = TVKJL;
TAU    = TVTAU;
KGL    = TVKGL;
KXG    = TVKXG;
KXGI   = TVKXGI;
ETABIO = TVETABIO;
KLAM   = TVKLAM;
FGI    = TVFGI;
KXI    = TVKXI;
BETA   = TVBETA;
GAMMA  = TVGAMMA;

// Basal hepatic glucose production rate, obtained by imposing dG/dt = 0 at the
// fasting steady-state (t = 0), when intestinal absorption is negligible.
GPROD0 = (KXG + KXGI*Ib)*Gb;

STOM = D;
JEJ  = 0;
ILE  = 0;
GLU  = Gb;
INS  = Ib;

$ODE
// mrgsolve has no native support for delay differential equations. The
// jejunum-to-ileum transit delay (Salinari et al., 2011) is approximated here
// by locally storing the solver history of JEJ and linearly interpolating the
// delayed value J(t - TAU).
static std::vector<double> htime;
static std::vector<double> hJ;

// Detect the start of a new individual (solver time resets close to zero) and
// clear the stored history.
if(SOLVERTIME <= 1.0E-6 && !htime.empty() && htime.back() > 1.0E-6) {
  htime.clear();
  hJ.clear();
}

if(htime.empty() || SOLVERTIME > htime.back()) {
  htime.push_back(SOLVERTIME);
  hJ.push_back(JEJ);
}

double Jdelay = 0.0;
if(SOLVERTIME >= TAU) {
  double ttar = SOLVERTIME - TAU;
  if(ttar <= htime.front()) {
    Jdelay = hJ.front();
  } else if(ttar >= htime.back()) {
    Jdelay = hJ.back();
  } else {
    int n = (int)htime.size();
    for(int i = 1; i < n; ++i) {
      if(htime[i] >= ttar) {
        double t0 = htime[i-1];
        double t1 = htime[i];
        double j0 = hJ[i-1];
        double j1 = hJ[i];
        Jdelay = (t1 > t0) ? j0 + (j1 - j0)*(ttar - t0)/(t1 - t0) : j1;
        break;
      }
    }
  }
}

// Complementary Michaelis-Menten hepatic glucose production (Equation 7)
double GPROD = KLAM/(KLAM/GPROD0 + (GLU - Gb));
// Apparent glucose sensed by the incretin system (Equation 8)
double GTILDE = GLU + FGI*(KGJ*JEJ + KGL*ILE);

dxdt_STOM = -KJS*STOM;
dxdt_JEJ  = KJS*STOM - KGJ*JEJ - KJL*JEJ;
dxdt_ILE  = KJL*Jdelay - KGL*ILE;
dxdt_GLU  = -(KXG + KXGI*INS)*GLU + GPROD + ETABIO*(KGJ*JEJ + KGL*ILE);
dxdt_INS  = KXI*Ib*((pow(BETA,GAMMA) + 1)/(pow(BETA,GAMMA)*pow(Gb/GTILDE,GAMMA) + 1) - INS/Ib);

$OMEGA @annotated
EKJS    : 0.09 : ETA on kjs
EKGJ    : 0.09 : ETA on kgj
EKJL    : 0.09 : ETA on kjl
ETAU    : 0.09 : ETA on tau
EKGL    : 0.09 : ETA on kgl
EKXG    : 0.09 : ETA on kxg
EKXGI   : 0.09 : ETA on kxgi
EETABIO : 0.09 : ETA on eta (bioavailability)
EKLAM   : 0.09 : ETA on klambda
EFGI    : 0.09 : ETA on fgi
EKXI    : 0.09 : ETA on kxi
EBETA   : 0.09 : ETA on beta
EGAMMA  : 0.09 : ETA on gamma

$SIGMA @annotated
PROPG : 0.01 : Proportional residual error variance for glycemia
PROPI : 0.01 : Proportional residual error variance for insulinemia

$TABLE
double IPREDG = GLU;
double IPREDI = INS;
double DVG = IPREDG*(1 + EPS(1));
double DVI = IPREDI*(1 + EPS(2));

$CAPTURE
IPREDG IPREDI DVG DVI