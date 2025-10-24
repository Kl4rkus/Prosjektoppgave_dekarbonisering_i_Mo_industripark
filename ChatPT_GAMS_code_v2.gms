$Title FeMn / SiMn production & energy system (fixed domains, ASCII only)

*-----------------------------*
* Sets and indices            *
*-----------------------------*
Set
    t        "time periods" / t1 /
    s        "SAF units"     / s1, s2 /
    fuel_all "all fuels"
/ oil, biooil, woodchips, coke, COgas, hydrogen, natgas, biogas, biochar /;

* fuel_lt and fuel_ht are subsets of fuel_all
Set fuel_lt(fuel_all) "low-temp fuels" / oil, biooil, woodchips /;
Set fuel_ht(fuel_all) "high-temp fuels" / coke, COgas, hydrogen, natgas, biogas, biochar /;

Alias (t,tt);

*-----------------------------*
* Parameters (data inputs)    *
*-----------------------------*

* Demands
Parameter
    D_FeMn(t)    "FeMn demand (t)"
    D_SiMn(t)    "SiMn demand (t)";

* Yields
Scalar
    Y_ore_to_sinter      "ore per t sinter"
    Y_sinter_to_FeMn     "sinter per t FeMn"
    Y_sinter_to_slag     "slag per t sinter used";

* Off-gas yield (t off-gas per unit of fuel use in SAF)
Parameter
    Y_offgas(s,t,fuel_all)  "off-gas yield";

* Emission factors and off-gas sink
Scalar
    EF_coke_sint  "t CO2 per t sinter output"
    EF_flare      "t CO2 per t off-gas flared";
Parameter
    D_offgas(t)   "off-gas demand (t)";

* Electricity intensities
Parameter
    eta_SAF_FeMn(s,t)  "kWh per t FeMn"
    nu_SAF_SiMn(s,t)   "kWh per t SiMn";
Scalar
    eta_sint           "kWh per t sinter";

* Total heat required per tonne of product (kWh/t)
Parameter
    TCF_FeMn(s,t)   "total heat per t FeMn"
    TCF_SiMn(s,t)   "total heat per t SiMn"
    TCF_sint(t)     "total heat per t sinter";

* Minimum HT requirements (kWh per t product)
Parameter
    A_SAF_HT_req(s)       "min HT for FeMn"
    A_SAF_SiMn_HT_req(s)  "min HT for SiMn";
Scalar
    A_sint_HT_req         "min HT for sinter";

* Fuel-to-heat conversion factors
Parameter
    phi_saf_lt(s,t,fuel_lt)       "LT heat from LT fuels in SAF (FeMn)"
    phi_saf_ht(s,t,fuel_ht)       "HT heat from HT fuels in SAF (FeMn)"
    phi_saf_smn_lt(s,t,fuel_all)  "LT heat from fuels in SAF (SiMn)"
    phi_saf_smn_ht(s,t,fuel_all)  "HT heat from fuels in SAF (SiMn)"
    phi_sint_lt(t,fuel_lt)        "LT heat from LT fuels in sinter"
    phi_sint_ht(t,fuel_ht)        "HT heat from HT fuels in sinter";

* Per-fuel minimums or availability: A * LT/HT <= fuel use
Parameter
    A_saf_lt(s,fuel_lt)  "LT fuel availability for SAF"
    A_saf_ht(s,fuel_ht)  "HT fuel availability for SAF"
    A_sint_lt(fuel_lt)   "LT fuel availability for sinter"
    A_sint_ht(fuel_ht)   "HT fuel availability for sinter";

* Capacities
Parameter
    Q_SAF(s,t)  "SAF capacity"
    Q_sint(t)   "sinter capacity";

* Costs
Parameter
    C_ore(t)        "cost per t ore"
    C_buysinter(t)  "cost per t sinter bought"
    C_coke(t)       "cost per t coke"
    C_elec(t)       "cost per kWh electricity"
    C_ets(t)        "cost per t CO2"
    FIXEDCOST(t)    "fixed cost in period t"
    C_fuel(fuel_all,t)   "cost per unit of each fuel (same unit as u-variables)";


*-----------------------------*
* Decision variables          *
*-----------------------------*

Positive Variable
* Production and material flows
    pFeMn(s,t)        "FeMn production (t)"
    pSiMn(s,t)        "SiMn production (t)"
    pSinter(t)        "sinter production (t)"
    bSinter(t)        "sinter bought (t)"
    uOre(t)           "ore use (t)"
    uSinterFeMn(s,t)  "sinter used to produce FeMn (t)"
    uSinterSiMn(s,t)  "sinter used to produce SiMn (t)"

* Electricity
    eSAF(s,t)         "SAF electricity (kWh)"
    eSint(t)          "sinter electricity (kWh)"

* Heat balances
    LT_SAF_FeMn(s,t)  "LT heat in SAF for FeMn (kWh)"
    HT_SAF_FeMn(s,t)  "HT heat in SAF for FeMn (kWh)"
    LT_SAF_SiMn(s,t)  "LT heat in SAF for SiMn (kWh)"
    HT_SAF_SiMn(s,t)  "HT heat in SAF for SiMn (kWh)"
    LT_sint(t)        "LT heat in sinter (kWh)"
    HT_sint(t)        "HT heat in sinter (kWh)"

* Fuel uses
    uSAF(s,t,fuel_all)      "fuel use in SAF for FeMn"
    uSAF_SiMn(s,t,fuel_all) "fuel use in SAF for SiMn"
    uSint_LT(t,fuel_lt)     "LT fuels used in sinter"
    uSint_HT(t,fuel_ht)     "HT fuels used in sinter"

* Slag and off-gas
    pSlag(s,t)        "slag"
    pSlagTot(t)       "total slag"
    gOffgas(s,t)      "off-gas from SAF"

* Coke accounting and emissions
    uTotalCoke_SAF(t) "total coke in SAF (FeMn path)"
    CO2_coke_sint(t)  "CO2 from sinter (coke based)"
    CO2_offgas(t)     "CO2 from off-gas flaring";

Variable
    TotalCost         "objective";

*-----------------------------*
* Equations                   *
*-----------------------------*

Equation
    dem_FeMn(t)
    dem_SiMn(t)

    ore_to_sinter(t)
    sinter_energy_total(t)
    sinter_ht_min(t)
    sinter_power(t)
    sinter_use_balance(t)
    lt_sint_def(t)
    lt_sint_min_lt(fuel_lt,t)
    ht_sint_def(t)
    ht_sint_min_ht(fuel_ht,t)

    FeMn_sinter_link(s,t)
    FeMn_energy_total(s,t)
    FeMn_ht_min(s,t)
    lt_saf_femn_def(s,t)
    lt_saf_femn_min(s,t,fuel_lt)
    ht_saf_femn_def(s,t)
    ht_saf_femn_min(s,t,fuel_ht)

    slag_def(s,t)
    slag_tot_def(t)
    SiMn_material_balance(t)

    lt_saf_simn_def(s,t)
    ht_saf_simn_def(s,t)
    SiMn_energy_total(s,t)
    SiMn_ht_min(s,t)

    e_saf_def(s,t)

    offgas_def(s,t)
    CO2_coke_sint_def(t)
    CO2_offgas_def(t)
    coke_total_def(t)

    capacity_saf(s,t)
    capacity_sinter(t)

    obj;

*-----------------------------*
* Constraints                 *
*-----------------------------*

dem_FeMn(t).. sum(s, pFeMn(s,t)) =g= D_FeMn(t);
dem_SiMn(t).. sum(s, pSiMn(s,t)) =g= D_SiMn(t);

ore_to_sinter(t).. uOre(t) =e= Y_ore_to_sinter * pSinter(t);

sinter_energy_total(t).. LT_sint(t) + HT_sint(t) =e= TCF_sint(t) * pSinter(t);
sinter_ht_min(t)..       A_sint_HT_req * pSinter(t) =l= HT_sint(t);
sinter_power(t)..        eSint(t) =e= eta_sint * pSinter(t);

sinter_use_balance(t)..
    sum(s, uSinterFeMn(s,t)) + sum(s, uSinterSiMn(s,t)) =e= pSinter(t) + bSinter(t);

lt_sint_def(t).. LT_sint(t) =e= sum(fuel_lt, phi_sint_lt(t,fuel_lt) * uSint_LT(t,fuel_lt));
lt_sint_min_lt(fuel_lt,t).. A_sint_lt(fuel_lt) * LT_sint(t) =l= uSint_LT(t,fuel_lt);

ht_sint_def(t).. HT_sint(t) =e= sum(fuel_ht, phi_sint_ht(t,fuel_ht) * uSint_HT(t,fuel_ht));
ht_sint_min_ht(fuel_ht,t).. A_sint_ht(fuel_ht) * HT_sint(t) =l= uSint_HT(t,fuel_ht);

FeMn_sinter_link(s,t).. uSinterFeMn(s,t) =e= Y_sinter_to_FeMn * pFeMn(s,t);

FeMn_energy_total(s,t).. LT_SAF_FeMn(s,t) + HT_SAF_FeMn(s,t) =e= TCF_FeMn(s,t) * pFeMn(s,t);
FeMn_ht_min(s,t)..       A_SAF_HT_req(s) * pFeMn(s,t) =l= HT_SAF_FeMn(s,t);

lt_saf_femn_def(s,t)..
    LT_SAF_FeMn(s,t) =e= sum(fuel_lt, phi_saf_lt(s,t,fuel_lt) * uSAF(s,t,fuel_lt));
lt_saf_femn_min(s,t,fuel_lt)..
    A_saf_lt(s,fuel_lt) * LT_SAF_FeMn(s,t) =l= uSAF(s,t,fuel_lt);

ht_saf_femn_def(s,t)..
    HT_SAF_FeMn(s,t) =e= sum(fuel_ht, phi_saf_ht(s,t,fuel_ht) * uSAF(s,t,fuel_ht));
ht_saf_femn_min(s,t,fuel_ht)..
    A_saf_ht(s,fuel_ht) * HT_SAF_FeMn(s,t) =l= uSAF(s,t,fuel_ht);

slag_def(s,t)..   pSlag(s,t) =e= Y_sinter_to_slag * uSinterFeMn(s,t);
slag_tot_def(t).. pSlagTot(t) =e= sum(s, pSlag(s,t));

SiMn_material_balance(t)..
    sum(s, uSinterSiMn(s,t)) + pSlagTot(t) =e= sum(s, pSiMn(s,t));

lt_saf_simn_def(s,t)..
    LT_SAF_SiMn(s,t) =e= sum(fuel_all, phi_saf_smn_lt(s,t,fuel_all) * uSAF_SiMn(s,t,fuel_all));
ht_saf_simn_def(s,t)..
    HT_SAF_SiMn(s,t) =e= sum(fuel_all, phi_saf_smn_ht(s,t,fuel_all) * uSAF_SiMn(s,t,fuel_all));

SiMn_energy_total(s,t)..
    LT_SAF_SiMn(s,t) + HT_SAF_SiMn(s,t) =e= TCF_SiMn(s,t) * pSiMn(s,t);
SiMn_ht_min(s,t).. A_SAF_SiMn_HT_req(s) * pSiMn(s,t) =l= HT_SAF_SiMn(s,t);

e_saf_def(s,t).. eSAF(s,t) =e= eta_SAF_FeMn(s,t) * pFeMn(s,t) + nu_SAF_SiMn(s,t) * pSiMn(s,t);

offgas_def(s,t).. gOffgas(s,t) =e= sum(fuel_all, Y_offgas(s,t,fuel_all) * uSAF(s,t,fuel_all));

CO2_coke_sint_def(t).. CO2_coke_sint(t) =e= EF_coke_sint * pSinter(t);
CO2_offgas_def(t)..    CO2_offgas(t)    =e= (sum(s, gOffgas(s,t)) - D_offgas(t)) * EF_flare;

coke_total_def(t).. uTotalCoke_SAF(t) =e= sum(s, uSAF(s,t,'coke'));

capacity_saf(s,t)..  pFeMn(s,t) + pSiMn(s,t) =l= Q_SAF(s,t);
capacity_sinter(t).. pSinter(t)              =l= Q_sint(t);

obj..
    TotalCost =e= sum(t,
                 FIXEDCOST(t)
               + C_ore(t)        * uOre(t)
               + C_buysinter(t)  * bSinter(t)
               + C_elec(t)       * ( sum(s, eSAF(s,t)) + eSint(t) )
               + C_ets(t)        * ( CO2_coke_sint(t) + CO2_offgas(t) )
               + sum(s, sum(fuel_all, C_fuel(fuel_all,t) * ( uSAF(s,t,fuel_all) + uSAF_SiMn(s,t,fuel_all) )))
               + sum(fuel_lt, C_fuel(fuel_lt,t) * uSint_LT(t,fuel_lt))
               + sum(fuel_ht, C_fuel(fuel_ht,t) * uSint_HT(t,fuel_ht))
               );



Model FeMn_SiMn_Opt / all /;

*-----------------------------*
* Test data (your numbers)    *
*-----------------------------*

D_FeMn(t)  = 58463;
D_SiMn(t)  = 58463;

Y_ore_to_sinter  = 1;
Y_sinter_to_FeMn = 2;
Y_sinter_to_slag = 0.25;

Y_offgas(s,t,fuel_all) = 1.8;

EF_coke_sint = 1.832;
EF_flare     = 1.34;
D_offgas(t)  = 0;

eta_SAF_FeMn(s,t) = 2850;
nu_SAF_SiMn(s,t)  = 4000;
eta_sint          = 300;

TCF_FeMn(s,t) = 1;
TCF_SiMn(s,t) = 1;
TCF_sint(t)   = 0.1;

* Sinter only uses coke
phi_sint_lt(t,fuel_lt) = 0;
phi_sint_ht(t,fuel_ht) = 0;
phi_sint_ht(t,'coke')  = 2000;

* FeMn can use any fuel; set phi = 1 so LT/HT sums to required heat
phi_saf_lt(s,t,fuel_lt)  = 1000;
phi_saf_ht(s,t,fuel_ht)  = 2000;

* SiMn energy closure
phi_saf_smn_lt(s,t,fuel_all) = 1000;
phi_saf_smn_ht(s,t,fuel_all) = 2000;

* No per-fuel mins unless supplied
A_saf_lt(s,fuel_lt) = 0;
A_saf_ht(s,fuel_ht) = 0;
A_sint_lt(fuel_lt)  = 0;
A_sint_ht(fuel_ht)  = 0;

A_sint_HT_req         = 0;
A_SAF_HT_req(s)       = 0.2;
A_SAF_SiMn_HT_req(s)  = 0;

Q_SAF(s,t) = 120000;
Q_sint(t)  = 120000;

C_ore(t)        = 100;
C_buysinter(t)  = 500;
C_coke(t)       = 300;
C_elec(t)       = 0.01;
C_ets(t)        = 10;
FIXEDCOST(t)    = 10000;
C_fuel('coke',t)   = C_coke(t); 
* (Optional) Set others when you have them:
 C_fuel('oil',t)      = 500;
 C_fuel('biooil',t)   = 450;
 C_fuel('woodchips',t)= 120;
 C_fuel('COgas',t)    = 50;
 C_fuel('hydrogen',t) = 2000;
 C_fuel('natgas',t)   = 150;
 C_fuel('biogas',t)   = 130;
 C_fuel('biochar',t)  = 350;


*-----------------------------*
* Solve                       *
*-----------------------------*
Solve FeMn_SiMn_Opt using LP minimizing TotalCost;

* Display key results
* Display pFeMn, pSiMn, pSinter, eSAF, eSint, CO2_offgas, CO2_coke_sint;
