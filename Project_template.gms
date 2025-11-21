$Title Generalized Optimization Model for Production Planning

* Sets
SET
    m           'Materials and fuels'          / FeMn, SiMn, off-gas, slag, quartz, oil, biooil, woodchips, coke, biocarbon, hydrogen /
    t           'Time periods'                 / t1*t5 /
    q           'Investment periods'           / q1*q3 /
    f           'Production machines'          / sinter, SAF1, SAF2, CCS /
    p           'Processes'                    / p1*p6 /
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
M_input(p, m) = yes;
M_input('p1', 'sinter') = no;  * Example: sinter is not an input for p1
M_input('p1', 'quartz') = yes; * Example: quartz is an input for p1

M_output(p, m) = yes;
M_output('p1', 'FeMn') = yes;  * Example: FeMn is an output for p1
M_output('p1', 'off-gas') = yes;

P_f(f, p) = yes;
P_f('sinter', 'p1') = yes;     * Example: process p1 belongs to machine sinter
P_f('SAF1', 'p2') = yes;        * Example: process p2 belongs to machine SAF1

* Parameters
PARAMETER
    D(m, t)         'Demand (t) of end product m in period t'
    Y(m, p)         'Material output m produced with process p at normal production level'
    LEF(l, p)       'Emission factor of fuel l from process p'
    HEF(h, p)       'Emission factor of fuel h from process p'
    P(m, p)         'Material input m needed to run process p at normal production level'
    E(p)            'Electricity needed to run process p at normal production level'
    T(p)            'Total heat in process p needed to operate normally (kWh)'
    HT(p)           'Percentage of high heat needed for process p to operate normally'
    LTR(l)          'Conversion rate of low heat fuel l to heat'
    HTR(h)          'Conversion rate of high heat fuel h to heat'
    C_Mat(m, t)     'Cost of material m bought (per ton) in period t'
    C_HFuel(h, t)   'Cost of high temperature fuel h in period t'
    C_LFuel(l, t)   'Cost of low temperature fuel l in period t'
    C_Elec(t)       'Price of electricity at time t'
    C_Carbon(t)     'Price of the carbon tax (per ton) in period t'
    C_Invest(p, q)  'Price of an investment p at time q'
    R               'Discount rate'
;

* Assign values to parameters (example: customize based on your data)
D(m, t) = 100;  * Example: demand for each material and period
Y(m, p) = 0.9;  * Example: yield for each material and process
LEF(l, p) = 0.5;
HEF(h, p) = 0.8;
P(m, p) = 1.2;  * Example: input requirement for each material and process
E(p) = 50;      * Example: electricity needed for each process
T(p) = 1000;    * Example: total heat needed for each process
HT(p) = 0.3;    * Example: 30% of heat must be high heat
LTR(l) = 0.8;   * Example: conversion rate for low heat fuels
HTR(h) = 0.9;   * Example: conversion rate for high heat fuels
C_Mat(m, t) = 10;
C_HFuel(h, t) = 20;
C_LFuel(l, t) = 15;
C_Elec(t) = 5;
C_Carbon(t) = 30;
C_Invest(p, q) = 1000;
R = 0.05;       * Example: 5% discount rate

* Variables
VARIABLE
    z               'Total cost (OPEX + CAPEX)'
    w_max(f, q)     'Max usage of machine f at investment period q'
    w(p, t)         'Usage of process p at time t'
    o(m, p, t)      'Output m made with process p at time t'
    c(t)            'Carbon emissions at time t'
    i(m, p, t)      'Input m consumed by process p at time t'
    g(m, t)         'Input material m purchased at time t'
    b(h, p, t)      'High heat fuel h in process p at time t'
    u(l, p, t)      'Low heat fuel l in process p at time t'
;

* Positive variables
POSITIVE VARIABLE
    w_max, w, o, c, i, g, b, u
;

* Equations
EQUATION
    obj             'Objective function: minimize total cost'
    capex           'CAPEX: sum of all investments'
    cost_mat        'Costs of materials bought'
    cost_fuel       'Costs of fuels (low and high temp)'
    cost_elec       'Costs of electricity'
    cost_carbon     'Carbon tax'
    balance(m, t)   'Balance constraint: material balance'
    heat(p, t)      'Heat constraint: total heat required'
    high_heat(p, t) 'High heat constraint: high heat required'
    input(m, p, t)  'Input constraint: material input required'
    output(m, p, t) 'Output constraint: material output produced'
    carbon(t)       'Carbon emissions constraint'
    capacity(f, t) 'Capacity constraint: max usage depends on investments'
;

* Objective function
obj..
    z =E= OPEX + CAPEX;

* CAPEX equation
capex..
    CAPEX =E= sum(q, (1 / power(1 + R, ord(q))) * sum(p, C_Invest(p, q) * w_max('f', q)));

* Cost of materials bought
cost_mat..
    costs_mat =E= sum((m, t), g(m, t) * C_Mat(m, t));

* Cost of fuels (low and high temp)
cost_fuel..
    costs_fuel =E= sum(t, (sum((l, p), C_LFuel(l, t) * u(l, p, t)) + sum((h, p), C_HFuel(h, t) * b(h, p, t))));

* Cost of electricity
cost_elec..
    costs_elec =E= sum((p, t), E(p) * C_Elec(t) * w(p, t));

* Carbon tax
cost_carbon..
    costs_carbon =E= sum(t, c(t) * C_Carbon(t));

* OPEX equation
OPEX =E= costs_mat + costs_fuel + costs_elec + costs_carbon;

* Balance constraint
balance(m, t)..
    g(m, t) + sum(p$M_output(p, m), o(m, p, t)) =E= D(m, t) + sum(p$M_input(p, m), i(m, p, t));

* Heat constraint
heat(p, t)..
    sum(l, u(l, p, t) * LTR(l)) + sum(h, b(h, p, t) * HTR(h)) =G= w(p, t) * T(p);

* High heat constraint
high_heat(p, t)..
    sum(h, b(h, p, t) * HTR(h)) =G= w(p, t) * T(p) * HT(p);

* Input constraint
input(m, p, t)$M_input(p, m)..
    i(m, p, t) =E= P(m, p) * w(p, t);

* Output constraint
output(m, p, t)$M_output(p, m)..
    o(m, p, t) =E= Y(m, p) * w(p, t);

* Carbon emissions constraint
carbon(t)..
    c(t) =E= sum(p, (sum(h, b(h, p, t) * HEF(h, p)) + sum(l, u(l, p, t) * LEF(l, p))));

* Capacity constraint
capacity(f, t)..
    sum(p$P_f(f, p), w(p, t)) =L= sum(q$(ord(q) <= ord(t)), w_max(f, q));

* Model definition
MODEL production_model /all/;

* Solve statement
SOLVE production_model USING LP MINIMIZING z;
