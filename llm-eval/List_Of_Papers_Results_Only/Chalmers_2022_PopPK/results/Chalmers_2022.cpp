$PROB
Brensocatib population PK model with 6 transit absorption compartments, two disposition compartments, and linear clearance (Chalmers et al., Clin Pharmacokinet 2022)

# Model Annotations: 

block   name        descr                                             unit                
------  ----------  ------------------------------------------------  --------------------
CMT     TR1         First transit compartment                         .                   
CMT     TR2         Second transit compartment                        .                   
CMT     TR3         Third transit compartment                         .                   
CMT     TR4         Fourth transit compartment                        .                   
CMT     TR5         Fifth transit compartment                         .                   
CMT     TR6         Sixth transit compartment                         .                   
CMT     CENT        Central compartment                               .                   
CMT     PERIPH      Peripheral compartment                            .                   
PARAM   CL          Apparent oral clearance                           L/h/70kg            
PARAM   VC          Apparent oral central volume of distribution      L/70kg              
PARAM   CLD         Apparent oral distributional clearance            L/h                 
PARAM   VP          Apparent oral peripheral volume of distribution   L/70kg              
PARAM   KTRFASTED   Transit rate constant, fasted state               1/h                 
PARAM   KTRFED      Transit rate constant, fed state                  1/h                 
PARAM   VCPOWAGE    Power exponent for age effect on VC               .                   
PARAM   CLPOWCLCR   Power exponent for CLcr effect on CL              .                   
PARAM   WTKG        Body weight                                       kg                  
PARAM   AGE         Age                                               years               
PARAM   CLCR        Creatinine clearance normalized to BSA            mL/min/1.73m2       
PARAM   FED         Fed state indicator                               0 = fasted, 1 = fed 
OMEGA   ECL         ETA on CL                                         .                   
OMEGA   EVC         ETA on VC                                         .                   
OMEGA   EKTR        ETA on transit rate constant                      .                   
SIGMA   PROP        prop error                                        .                   

$PARAM
CL = 6.23
VC = 196
CLD = 6.26
VP = 80.6
KTRFASTED = 11.7
KTRFED = 9.96
VCPOWAGE = 0.396
CLPOWCLCR = 0.276
WTKG = 70
AGE = 60
CLCR = 75.2
FED = 0

$INIT
TR1 = 0
TR2 = 0
TR3 = 0
TR4 = 0
TR5 = 0
TR6 = 0
CENT = 0
PERIPH = 0

$OMEGA
@block
@labels ECL EVC
// row 1
0.139
// row 2
0.0464
0.193

$OMEGA
@block
@labels EKTR
// row 1
0.0921

$SIGMA
@block
@labels PROP
// row 1
0

$MAIN
// Allometric scaling on body weight (0.75 exponent for clearances, 1 for volumes)
double CLI = CL * pow(WTKG/70, 0.75) * pow(CLCR/75.2, CLPOWCLCR) * exp(ECL);
double VCI = VC * (WTKG/70) * pow(AGE/60, VCPOWAGE) * exp(EVC);
double CLDI = CLD * pow(WTKG/70, 0.75);
double VPI = VP * (WTKG/70);
// Transit rate constant depends on fed/fasted state, with shared IIV
double KTRI = (FED == 1 ? KTRFED : KTRFASTED) * exp(EKTR);
double K10 = CLI/VCI;
double K12 = CLDI/VCI;
double K21 = CLDI/VPI;
 
$ODE
dxdt_TR1 = -KTRI*TR1;
dxdt_TR2 = KTRI*TR1 - KTRI*TR2;
dxdt_TR3 = KTRI*TR2 - KTRI*TR3;
dxdt_TR4 = KTRI*TR3 - KTRI*TR4;
dxdt_TR5 = KTRI*TR4 - KTRI*TR5;
dxdt_TR6 = KTRI*TR5 - KTRI*TR6;
dxdt_CENT = KTRI*TR6 - K10*CENT - K12*CENT + K21*PERIPH;
dxdt_PERIPH = K12*CENT - K21*PERIPH;
 
$TABLE
double CP = CENT/VCI;
 
$CAPTURE
CP

