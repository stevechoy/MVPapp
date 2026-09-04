$PROB
Friberg semi-mechanistic model of chemotherapy-induced myelosuppression (Friberg et al, J Clin Oncol 2002)

# Model Annotations: 

block   name       descr                                       unit               
------  ---------  ------------------------------------------  -------------------
CMT     CENT       Central PK compartment                      drug amount        
CMT     PROL       Proliferating progenitor cell compartment   .                  
CMT     TRANSIT1   Maturation transit compartment 1            .                  
CMT     TRANSIT2   Maturation transit compartment 2            .                  
CMT     TRANSIT3   Maturation transit compartment 3            .                  
CMT     CIRC       Circulating blood cell compartment          observed           
PARAM   CL         Drug clearance                              L/h                
PARAM   V          Central volume of distribution              L                  
PARAM   Circ0      Baseline circulating cell count             10^9/L             
PARAM   MTT        Mean transit time                           hours              
PARAM   gamma      Feedback parameter                          unitless           
PARAM   Slope      Linear drug effect parameter                unbound conc, uM-1 
OMEGA   ECIRC0     ETA on Circ0                                CV 35%             
OMEGA   EMTT       ETA on MTT                                  CV 14%             
OMEGA   ESLOPE     ETA on Slope                                CV 47%             
SIGMA   PROP       Proportional residual error variance        .                  
SIGMA   ADD        Additive residual error variance^2          10^9/L             

$PARAM
CL = 21
V = 74
Circ0 = 7.12
MTT = 90.4
gamma = 0.175
Slope = 6.39

$INIT
CENT = 0
PROL = 7.12
TRANSIT1 = 7.12
TRANSIT2 = 7.12
TRANSIT3 = 7.12
CIRC = 7.12

$OMEGA
@block
@labels ECIRC0 EMTT ESLOPE
// row 1
0.1225
// row 2
0
0.0196
// row 3
0
0
0.2209

$SIGMA
@block
@labels PROP ADD
// row 1
0.0357
// row 2
0
1.7161

$MAIN
double CIRC0i = Circ0*exp(ECIRC0);
double MTTi   = MTT*exp(EMTT);
double SLOPEi = Slope*exp(ESLOPE);
// n = 3 transit compartments; MTT = (n+1)/ktr
double ktr   = 4.0/MTTi;
double kprol = ktr;
double kcirc = ktr;
// Initialize compartments at baseline (steady-state prior to dosing)
PROL_0     = CIRC0i;
TRANSIT1_0 = CIRC0i;
TRANSIT2_0 = CIRC0i;
TRANSIT3_0 = CIRC0i;
CIRC_0     = CIRC0i;
 
$ODE
double CONC  = CENT/V;
double EDRUG = SLOPEi*CONC;
dxdt_CENT     = -CL/V*CENT;
dxdt_PROL     = kprol*PROL*(1-EDRUG)*pow(CIRC0i/CIRC, gamma) - ktr*PROL;
dxdt_TRANSIT1 = ktr*PROL - ktr*TRANSIT1;
dxdt_TRANSIT2 = ktr*TRANSIT1 - ktr*TRANSIT2;
dxdt_TRANSIT3 = ktr*TRANSIT2 - ktr*TRANSIT3;
dxdt_CIRC     = ktr*TRANSIT3 - kcirc*CIRC;
 
$TABLE
double IPRED = CIRC;
double DV = IPRED*(1+EPS(1)) + EPS(2);
 
$CAPTURE
IPRED
DV

