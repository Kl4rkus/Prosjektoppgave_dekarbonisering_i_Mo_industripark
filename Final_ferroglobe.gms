$Title Generalized Optimization Model for Production Planning

* Sets
SET
    m           'Materials and fuels'          / FeMn, SiMn, manganese_ore, manganese_sinter, Si, off-gas, slag_Si, slag_Mn, quartz, oil, biooil, woodchips, coke, biocarbon, hydrogen/
    t           'Time periods'                 / t1*t30 /
    q           'Investment periods'           / t5, t10, t15, t20, t25 /
    f           'Production machines'          / sinter, SAF, CCS, biocarbon, flare/ 
    p           'Processes'                    / sintering, Si_to_SiMn, SiO_to_SiMn, slag_to_Mn, sinter_to_Mn, flaring /
    l           'Low heat fuels'               / oil, biooil, woodchips /
    h           'High heat fuels/reductors'    / coke, biocarbon, hydrogen /
;

* Subsets for inputs and outputs per process
SET
    M_input(p, m)  'Materials used as input in process p'
    M_output(p, m) 'Materials used as output in process p'
    P_f(f, p)      'Processes p that belong to machine f'
    
;

* Define subsets (example: customize based on your data)
M_input(p, m) = no;
* sintering
M_input('sintering', 'manganese_ore') = yes;  
* Si_to_SiMn
M_input('Si_to_SiMn', 'Si') = yes;
M_input('Si_to_SiMn', 'manganese_sinter') = yes;
* SiO_to_SiMn
M_input('SiO_to_SiMn', 'quartz') = yes; 
M_input('SiO_to_SiMn', 'manganese_sinter') = yes;
* slag_to_Mn
M_input('Slag_to_Mn', 'slag_Mn') = yes;
* sinter_to_Mn
M_input('Sinter_to_Mn', 'manganese_sinter') = yes;
* flare
M_input('flaring', 'off-gas') = yes;
 
* Outputs, the off-gas is handeled further down, in equations, through 
M_output(p, m) = no;
* sintering
M_output('sintering', 'manganese_sinter') = yes;
M_output('sintering', 'off-gas') = yes;  

* Si_to_SiMn
M_output('Si_to_SiMn', 'SiMn') = yes;
M_output('Si_to_SiMn', 'off-gas') = yes;
M_output('Si_to_SiMn', 'slag_Si') = yes;

* SiO_to_SiMn
M_output('SiO_to_SiMn', 'SiMn') = yes;
M_output('SiO_to_SiMn', 'off-gas') = yes;
M_output('SiO_to_SiMn', 'slag_Si') = yes;

* slag_to_Mn
M_output('slag_to_Mn', 'FeMn') = yes;  
M_output('slag_to_Mn', 'off-gas') = yes;
M_output('slag_to_Mn', 'slag_Mn') = yes;  

* sinter_to_Mn
M_output('sinter_to_Mn', 'FeMn') = yes;
M_output('sinter_to_Mn', 'off-gas') = yes;
M_output('sinter_to_Mn', 'slag_Mn') = yes;

* Processes
P_f(f, p) = no;
*sinter
P_f('sinter', 'sintering') = yes;
*SAFs  
P_f('SAF', 'Si_to_SiMn') = yes;        
P_f('SAF', 'SiO_to_SiMn') = yes;        
P_f('SAF', 'slag_to_Mn') = yes;        
P_f('SAF', 'sinter_to_Mn') = yes;
P_f('flare', 'flaring') = yes;

SET
    m_include(m) 'Materials to include in the output equation';
m_include(m) = YES;
m_include('off-gas') = NO;


* Parameters
PARAMETER
    D(m, t)         'Demand (t) of end product m in period t'
    Y(m, p)         'Material output m produced with process p at normal production level'
    LEF(l, p)       'Emission factor of fuel l from process p'
    HEF(h, p)       'Emission factor of fuel h from process p'
    PRODUCE(m, p)         'Material input m needed to run process p at normal production level'
    E(p)            'Electricity needed to run process p at normal production level'
    TH(p)            'Total heat in process p needed to operate normally (kWh)'
    HT(p)           'Percentage of high heat needed for process p to operate normally'
    LTR(l)          'Conversion rate of low heat fuel l to heat'
    HTR(h)          'Conversion rate of high heat fuel h to heat'
    C_Mat(m, t)     'Cost of material m bought (per ton) in period t'
    C_Elec(t)       'Price of electricity at time t'
    C_carbon(t)     'Price of the carbon tax (per ton) in period t'
    C_Invest(f, q)  'Price of an investment f at time q'
    offgas_coef(p) 'percentage of carbon emission that are in offgas form'
    Lifetime(f)            'Lifetime of an investment f'
    Overflow_cost(m) 'cost of getting rid of overflowing materials.'
    R               'Discount rate'                                 /0.4/
    C_CCS        'Cost of capturing a ton of carbon'    /50/
    capture_rate               'Carbon capture rate'       /0.9/
    BIG_M                   'Big M'                                 /99999/
;

* Demands
D(m, t) = 0; 
D("FeMn", "t1") = 70459.8;
D("FeMn", "t3") = 5692.3;
D("FeMn", "t4") = 460;
D("FeMn", t)$(ord(t) >= 7) = 14629.7;
D("SiMn", "t1") = 66607.8;
D("SiMn", "t2") = 112911.1;
D("SiMn", "t3") = 89571.5;
D("SiMn", "t4") = 39481.7;
D("SiMn", "t5") = 83985.7;
D("SiMn", "t6") = 117459.3;
D("SiMn", "t7") = 102297.2;
D("SiMn", "t8") = 103270.4;
D("SiMn", "t9") = 107219.7;
D("SiMn", "t10") = 111168.9;
D("SiMn", "t11") = 115118.2;
D("SiMn", "t12") = 119067.4;
D("SiMn", "t13") = 123016.7;
D("SiMn", "t14") = 126965.9;
D("SiMn", "t15") = 130915.1;
D("SiMn", "t16") = 134864.4;
D("SiMn", "t17") = 138813.6;
D("SiMn", "t18") = 142762.9;
D("SiMn", "t19") = 146712.1;
D("SiMn", "t20") = 150661.4;
D("SiMn", "t21") = 154610.6;
D("SiMn", "t22") = 158559.8;
D("SiMn", "t23") = 162509.1;
D("SiMn", "t24") = 166458.3;
D("SiMn", "t25") = 170407.6;
D("SiMn", "t26") = 174356.8;
D("SiMn", "t27") = 178306.1;
D("SiMn", "t28") = 182255.3;
D("SiMn", "t29") = 186204.5;
D("SiMn", "t30") = 190153.8;

* Yields
* Here, all yields = 1 by definition of w(p, t)   'Usage of process p at time t (unit is in tons of final produce of p)'
Y('manganese_sinter','sintering') = 1;  
* Si_to_SiMn
Y( 'SiMn','Si_to_SiMn') = 1;
Y('slag_Si', 'Si_to_SiMn' ) = 0.33;
* SiO_to_SiMn
Y( 'SiMn','SiO_to_SiMn') = 1;
Y('slag_Si', 'SiO_to_SiMn' ) = 0.33;
* slag_to_Mn
Y( 'FeMn','slag_to_Mn') = 1;
Y('slag_Mn', 'slag_to_Mn') = 0.3;
* sinter_to_Mn
Y( 'FeMn','sinter_to_Mn') = 1;
Y('slag_Mn', 'sinter_to_Mn') = 0.3;



* Low fuel emission factors: / oil, biooil, woodchips /
*LEF(l, p) = 0.5; 
LEF('oil', 'sintering') = 3.185;
LEF('biooil', 'sintering') = 3.7;
LEF('woodchips', 'sintering') = 1.4;

LEF('oil', 'Si_to_SiMn') = 3.185;
LEF('biooil', 'Si_to_SiMn') = 3.7;
LEF('woodchips', 'Si_to_SiMn') = 1.4;

LEF('oil', 'SiO_to_SiMn') = 3.185;
LEF('biooil', 'SiO_to_SiMn') = 3.7;
LEF('woodchips', 'SiO_to_SiMn') = 1.4;

LEF('oil', 'slag_to_Mn') = 3.185;
LEF('biooil', 'slag_to_Mn') = 3.7;
LEF('woodchips', 'slag_to_Mn') = 1.4;

LEF('oil', 'sinter_to_Mn') = 3.185;
LEF('biooil', 'sinter_to_Mn') = 3.7;
LEF('woodchips', 'sinter_to_Mn') = 1.4;

*High fuel emission factors / coke, biocarbon, hydrogen /
*HEF(h, p) = 0.8;
HEF('coke', 'sintering') = 3.19;
HEF('biocarbon', 'sintering') = 2.75;
HEF('hydrogen', 'sintering') = 90;

HEF('coke', 'Si_to_SiMn') = 3.19;
HEF('biocarbon', 'Si_to_SiMn') = 2.75;
HEF('hydrogen', 'Si_to_SiMn') = 90;

HEF('coke', 'SiO_to_SiMn') = 3.19;
HEF('biocarbon', 'SiO_to_SiMn') = 2.75;
HEF('hydrogen', 'SiO_to_SiMn') = 90;

HEF('coke', 'slag_to_Mn') = 3.19;
HEF('biocarbon', 'slag_to_Mn') = 2.75;
HEF('hydrogen', 'slag_to_Mn') = 90;

HEF('coke', 'sinter_to_Mn') = 3.19;
HEF('biocarbon', 'sinter_to_Mn') = 2.75;
HEF('hydrogen', 'sinter_to_Mn') = 90;

*Material input
*P(m, p) = 1.2;  
* sintering
PRODUCE('manganese_ore','sintering') = 1.2;  
* Si_to_SiMn
PRODUCE( 'Si','Si_to_SiMn') = 0.4;
PRODUCE('manganese_sinter','Si_to_SiMn' ) = 0;  
* SiO_to_SiMn
PRODUCE( 'SiMn','SiO_to_SiMn') = 0.4;
PRODUCE('manganese_sinter','SiO_to_SiMn') = 0;  
* slag_to_Mn
PRODUCE( 'slag_Mn','slag_to_Mn') = 0;  
* sinter_to_Mn
PRODUCE( 'manganese_sinter','sinter_to_Mn') = 1.8;
* flare
PRODUCE( 'off-gas', 'flaring') = 1;

*Electricity needs for each process.
*E(p) = 50;
E('sintering') = 90;
E('Si_to_SiMn') = 3000;
E('SiO_to_SiMn') = 6000;
E('slag_to_Mn') = 2850;
E('sinter_to_Mn') =2850;

* Total heat needed for each process
*T(p) = 1000;
TH('sintering') = 1000;
TH('Si_to_SiMn') = 2500;
TH('SiO_to_SiMn') = 4500;
TH('slag_to_Mn') = 2700;
TH('sinter_to_Mn') = 2700;

* Percentage of high heat needed
*HT(p) = 0.3;
HT('sintering') = 0.7;
HT('Si_to_SiMn') = 0.7;
HT('SiO_to_SiMn') = 0.7;
HT('slag_to_Mn') = 0.7;
HT('sinter_to_Mn') = 0.7;

*conversion rate for low heat fuels / oil, biooil, woodchips /
*LTR(l) = 0.8;
LTR('oil') =11500;
LTR('biooil') = 4500;
LTR('woodchips') = 3500;

* conversion rate for high heat fuels / coke, biocarbon, hydrogen /
* HTR(h) = 0.9;
HTR('coke') = 8000;
HTR('biocarbon') = 700;
HTR('hydrogen') = 33000;

* Costs:
* Material costs: / FeMn, SiMn, manganese_ore, manganese_sinter, Si, off-gas, slag, quartz, oil, biooil, woodchips, coke, biocarbon, hydrogen /
C_Mat('FeMn', t) = BIG_M;
C_Mat('SiMn', t) = BIG_M;
C_Mat('manganese_ore', t) = 100;
C_Mat('manganese_sinter', t) = 500;
C_Mat('Si', t) = 250;
C_Mat('off-gas', t) = 50;
C_Mat('slag_Si', t) = BIG_M;
C_Mat('slag_Mn', t) = BIG_M;
C_Mat('quartz', t) = 50;
C_Mat('oil', t) = 11500;
C_Mat('biooil', t) = 4500;
C_Mat('woodchips', t) = 3500;
C_Mat('coke', t) = 300;
C_Mat('biocarbon', t) = 500;
C_Mat('hydrogen', t) = 5000;

* Electricity costs
C_Elec(t) = 0.01;

* Carbon tax !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C_carbon('t1') = 11;
LOOP(t$(ORD(t) > 1),
    C_carbon(t) = C_carbon(t-1) + 0;
);



* Cost of investment f at time q / sinter, SAF, CCS, biocarbon/
*C_Invest(f, q) = 1000;
C_Invest('sinter', q) = 100;
C_Invest('SAF', q) = 100;
*in euros per Tonne of capacity
C_Invest('CCS', q) = 100;
*in euros per Tonne of biocarbon capacity
C_Invest('biocarbon', q) = 100;

*offgas_coef / sintering, Si_to_SiMn, SiO_to_SiMn, slag_to_Mn, sinter_to_Mn, CO_flaring /
offgas_coef('sintering') = 0;
offgas_coef('Si_to_SiMn') = 1;
offgas_coef('SiO_to_SiMn') = 1;
offgas_coef('slag_to_Mn') = 1;
offgas_coef('sinter_to_Mn') = 1;
offgas_coef('flaring') = 0;


*Discount rate
R = 0.04;

*Investment lifetimes
Lifetime('biocarbon') = 20;
Lifetime('CCS') = 20;
Lifetime('SAF') = 99999;
Lifetime('sinter') = 99999;

*Overflow_Costs  / FeMn, SiMn, manganese_ore, manganese_sinter, Si, off-gas, slag, quartz, oil, biooil, woodchips, coke, biocarbon, hydrogen /
Overflow_cost('FeMn') = 100;
Overflow_cost('SiMn') = 100;
Overflow_cost('manganese_ore') = 10;
Overflow_cost('manganese_sinter') = 10;
Overflow_cost('Si') = 10;
*This is the most important one, since we want the off-gas to be flared
Overflow_cost('off-gas') = BIG_M;
Overflow_cost('slag_Si') = 0;
Overflow_cost('slag_Mn') = 0;
Overflow_cost('quartz') = 10;
Overflow_cost('oil') = 0;
Overflow_cost('biooil') = 0;
Overflow_cost('woodchips') = 0;
Overflow_cost('coke') = 0;
Overflow_cost('biocarbon') = 0;
Overflow_cost('hydrogen') = 0;

* Variables
VARIABLE
    z               'Total cost (OPEX + CAPEX)'
    w_max(f, q)     'Max usage of machine f at investment period q'
    w(p, t)         'Usage of process p at time t (unit is in tons of final produce of p)'
    o(m, p, t)      'Output m made with process p at time t'
    c(p,t)            'carbon output from process p'
    tc(t)            'Carbon emissions at time t'
    i(m, p, t)      'Input m consumed by process p at time t'
    g(m, t)         'Input material m purchased at time t'
    b(h, p, t)      'High heat fuel h in process p at time t'
    u(l, p, t)      'Low heat fuel l in process p at time t'
    overflow(m, t)        'Overflow of material m at time t'
    captured_carbon  'Carbon captured by CCS'
;

* Positive variables
POSITIVE VARIABLE
    w_max, w, o, c, i, g, b, u, overflow
;
* Equations
EQUATION
    obj             'Objective function: minimize total cost'
    capex_cost           'CAPEX: sum of all investments'
    cost_mat        'Costs of materials bought'
    cost_elec       'Costs of electricity'
    cost_carbon     'Carbon tax'
    balance(m, t)   'Balance constraint: material balance'
    heat(p, t)      'Heat constraint: total heat required'
    high_heat(p, t) 'High heat constraint: high heat required'
    input(m, p, t)  'Input constraint: material input required'
    output(m, p, t) 'Output constraint: material output produced'
    carbon(p,t)       'Carbon emissions constraint'
    offgas_out(p,t) 'CO offgas coming out of each SAF'
    total_carbon(t) 'Total carbon emissions'
    capacity(f, t) 'Capacity constraint: max usage depends on investments'
    opex_cost            'Total operational cost'
    overflow_costs     'Costs caused by overflows'
    carbon_capture(t)    'calculates carbon captured by CCS'
    max_carbon_capture(t) 'ensures that the carbon captures does not surpass the max capacity'
;

* Variables for cost components
VARIABLE
    costs_mat       'Costs of materials bought'
    costs_fuel      'Costs of fuels (low and high temp)'
    costs_elec      'Costs of electricity'
    costs_carbon    'Carbon tax'
    costs_overflow 'costs of trashing materials'
    OPEX            'Total operational cost'
    CAPEX           'Total capital cost'
;

* Objective function
obj..
    z =E= OPEX + CAPEX;

* CAPEX equation
capex_cost..
    CAPEX =E= sum(q, (1 / power(1 + R, ord(q))) * sum(f, C_Invest(f, q) * w_max(f, q)));

* Cost of materials bought
cost_mat..
    costs_mat =E= sum((m, t), g(m, t) * C_Mat(m, t));


* Cost of electricity
cost_elec..
    costs_elec =E= sum((p, t), E(p) * C_Elec(t) * w(p, t));

* Carbon tax
cost_carbon..
    costs_carbon =E= sum(t, (tc(t)-captured_carbon(t) * C_Carbon(t)));
    
* Costs due to material overflows. 
overflow_costs..
    costs_overflow =E= sum((m,t), overflow_cost(m)*overflow(m,t));

* OPEX equation
opex_cost..
    OPEX =E= costs_mat + costs_elec + costs_carbon + costs_overflow;
    


* Balance constraint
balance(m, t)..
    g(m, t) + sum(p$M_output(p, m), o(m, p, t)) =E= D(m, t) + sum(p$M_input(p, m), i(m, p, t)) + overflow(m, t);
* Heat constraint
heat(p, t)..
    sum(l, u(l, p, t) * LTR(l)) + sum(h, b(h, p, t) * HTR(h)) =E= w(p, t) * TH(p);
* High heat constraint
high_heat(p, t)..
    sum(h, b(h, p, t) * HTR(h)) =G= w(p, t) * TH(p) * HT(p);
* Input constraint
input(m, p, t)$M_input(p, m)..
    i(m, p, t) =E= PRODUCE(m, p) * w(p, t);
* Output constraint
output(m, p, t)$(M_output(p, m) AND m_include(m))..
    o(m, p, t) =E= Y(m, p) * w(p, t);

* Carbon emissions constraint
carbon(p,t)..
    c(p,t) =E= sum(h, b(h, p, t) * HEF(h, p)*(1-offgas_coef(p)))
              + sum(l, u(l, p, t) * LEF(l, p))*(1-offgas_coef(p));


offgas_out(p,t)..
    o('off-gas',p,t) =E=  sum(h, b(h, p, t) * HEF(h, p)*(offgas_coef(p))) + sum(l, u(l, p, t) * LEF(l, p))*(offgas_coef(p));
    
total_carbon(t)..
    tc(t) =E= sum(p, c(p,t)) + w('flaring', t); 
    
* Capacity constraint
capacity(f, t)..
    sum(p$P_f(f, p), w(p, t)) =L= sum(q$(ord(q) <= ord(t)), w_max(f, q));

* Carbon capture:
carbon_capture(t)..
    captured_carbon(t) =L= tc(t) * capture_rate; 

max_carbon_capture(t)..
    captured_carbon(t) =L= sum(q$(ord(q) <= ord(t)), w_max('CCS', q));

*fixing variables:
w_max.FX('sinter', 't5') = 250000;
w_max.FX('flare', 't5') = 9999999999999




* Model definition
MODEL production_model /all/;

* Solve statement
SOLVE production_model USING LP MINIMIZING z;

execute_unload 'Kl4rkus_results.gdx',
    z, w_max, w, o, c, tc, i, g, b, u, overflow, captured_carbon,
    D, Y, LEF, HEF, PRODUCE, E, TH, HT, LTR, HTR, C_Mat, C_Elec, C_carbon, C_Invest, offgas_coef, R,
    costs_mat, costs_elec, costs_carbon, costs_overflow, OPEX, CAPEX;

    