$Title FeMn / SiMn production & energy system with CCS (u_fuel in tonnes, phi in kWh/t)

* =========================
* Sets and indices
* =========================
Set t          "time periods" / t1*t20 /;
Set s          "SAF units"    / s1, s2 /;
Set fuel_all   "all fuels"
/ oil, biooil, woodchips, coke, COgas, hydrogen, natgas, biogas, biochar /;
Set fuel_lt(fuel_all) "low-temp fuels"  / oil, biooil, woodchips /;
Set fuel_ht(fuel_all) "high-temp fuels" / coke, COgas, hydrogen, natgas, biogas, biochar /;
Alias (t,tt);
Set y(t)  "CCS investment periods (subset of T, e.g. every 5th t)" / t5, t10,t15,t20 /;
Alias (t,ty);


* =========================
* Parameters (data inputs)
* =========================

* Demands
Parameter
    D_FeMn(t)    "FeMn demand (t)"
    D_SiMn(t)    "SiMn demand (t)";

* Yields
Scalar
    Y_ore_to_sinter      "ore per t sinter"
    Y_sinter_to_FeMn     "sinter per t FeMn"
    Y_sinter_to_slag     "slag per t sinter used";

* Off-gas yield (t off-gas per t fuel in SAF)
Parameter
    Y_offgas(s,t,fuel_all)     "off-gas yield (t/t fuel) - FeMn path"
    Y_offgas_smn(s,t,fuel_all) "off-gas yield (t/t fuel) - SiMn path";

* Emission factors and off-gas sink
Scalar
    EF_coke_sint  "t CO2 per t sinter output"
    EF_flare      "t CO2 per t off-gas fully oxidized (stack CO2 factor)";
Parameter
    D_offgas(t)   "off-gas demand (t)";

* Electricity intensities (kWh per t product)
Parameter
    eta_SAF_FeMn(s,t)  "kWh per t FeMn"
    nu_SAF_SiMn(s,t)   "kWh per t SiMn";
Scalar
    eta_sint           "kWh per t sinter";

* Total thermal heat required per tonne of product (kWh/t)
Parameter
    TCF_FeMn(s,t)   "total heat per t FeMn (kWh/t)"
    TCF_SiMn(s,t)   "total heat per t SiMn (kWh/t)"
    TCF_sint(t)     "total heat per t sinter (kWh/t)";

* Minimum HT requirements (kWh per t product)
Parameter
    A_SAF_HT_req(s)       "min HT for FeMn (kWh/t product)"
    A_SAF_SiMn_HT_req(s)  "min HT for SiMn (kWh/t product)";
Scalar
    A_sint_HT_req         "min HT for sinter (kWh/t product)";

* Fuel-to-heat conversion factors (phi = kWh produced per t fuel)
Parameter
    phi_saf_lt(s,t,fuel_lt)       "LT heat from LT fuels in SAF (FeMn) [kWh/t fuel]"
    phi_saf_ht(s,t,fuel_ht)       "HT heat from HT fuels in SAF (FeMn) [kWh/t fuel]"
    phi_saf_smn_lt(s,t,fuel_all)  "LT heat from fuels in SAF (SiMn)   [kWh/t fuel]"
    phi_saf_smn_ht(s,t,fuel_all)  "HT heat from fuels in SAF (SiMn)   [kWh/t fuel]"
    phi_sint_lt(t,fuel_lt)        "LT heat from LT fuels in sinter    [kWh/t fuel]"
    phi_sint_ht(t,fuel_ht)        "HT heat from HT fuels in sinter    [kWh/t fuel]";

* Per-fuel minimum share / availability: A * LT/HT <= fuel use
Parameter
    A_saf_lt(s,fuel_lt)  "LT fuel availability for SAF"
    A_saf_ht(s,fuel_ht)  "HT fuel availability for SAF"
    A_sint_lt(fuel_lt)   "LT fuel availability for sinter"
    A_sint_ht(fuel_ht)   "HT fuel availability for sinter";

* Capacities
Parameter
    Q_SAF(s,t)  "SAF capacity (t product)"
    Q_sint(t)   "sinter capacity (t sinter)";

* Costs
Parameter
    C_ore(t)        "cost per t ore"
    C_buysinter(t)  "cost per t sinter bought"
    C_coke(t)       "cost per t coke (legacy convenience)"
    C_elec(t)       "cost per kWh electricity"
    C_ets(t)        "cost per t CO2"
    FIXEDCOST(t)    "fixed cost in period t"
    C_fuel(fuel_all,t)   "cost per t fuel";

* =========================
* CCS PARAMETERS (LP)
* =========================
Scalar
    omega_ccs      "capture efficiency ω (0..1)"             / 0.9 /
    ;
Parameter
    SEC_CCS_el(t)  "electricity for CCS (kWh per tCO2 captured)"
    SEC_CCS_th(t)  "steam/heat for CCS (kWh_th per tCO2 captured)"
    C_CCS_var(t)   "CCS variable O&M (€/tCO2 captured)"
    C_CCS_TandS(t) "Transport & storage (€/tCO2 captured)"
    C_CCS_steam(t) "Steam price for CCS (€/kWh_th)"
    C_CCS_capex(t) "Capex cost for CCS in investment periods y (€/ (tCO2 per period))";

* Initialize so model runs if no data provided
SEC_CCS_el(t)  = 0;
SEC_CCS_th(t)  = 0;
C_CCS_var(t)   = 50;
C_CCS_TandS(t) = 0;
C_CCS_steam(t) = 0;
C_CCS_capex(t) = 100;


* =========================
* Decision variables
* =========================
Positive Variable
* Production and material flows
    pFeMn(s,t)        "FeMn production (t)"
    pSiMn(s,t)        "SiMn production (t)"
    pSinter(t)        "sinter production (t)"
    bSinter(t)        "sinter bought (t)"
    uOre(t)           "ore use (t)"
    uSinterFeMn(s,t)  "sinter used to produce FeMn (t)"
    uSinterSiMn(s,t)  "sinter used to produce SiMn (t)"

* Electricity (kWh)
    eSAF(s,t)         "SAF electricity (kWh)"
    eSint(t)          "sinter electricity (kWh)"

* Heat balances (kWh)
    LT_SAF_FeMn(s,t)  "LT heat in SAF for FeMn (kWh)"
    HT_SAF_FeMn(s,t)  "HT heat in SAF for FeMn (kWh)"
    LT_SAF_SiMn(s,t)  "LT heat in SAF for SiMn (kWh)"
    HT_SAF_SiMn(s,t)  "HT heat in SAF for SiMn (kWh)"
    LT_sint(t)        "LT heat in sinter (kWh)"
    HT_sint(t)        "HT heat in sinter (kWh)"

* Fuel uses (tonnes fuel)
    uSAF(s,t,fuel_all)      "fuel use in SAF for FeMn (t fuel)"
    uSAF_SiMn(s,t,fuel_all) "fuel use in SAF for SiMn (t fuel)"
    uSint_LT(t,fuel_lt)     "LT fuels used in sinter (t fuel)"
    uSint_HT(t,fuel_ht)     "HT fuels used in sinter (t fuel)"

* Slag and off-gas
    pSlag(s,t)        "slag (t)"
    pSlagTot(t)       "total slag (t)"
    gOffgas(s,t)      "off-gas from SAF (t)"

* Coke accounting and emissions (legacy)
    uTotalCoke_SAF(t) "total coke in SAF (t fuel)"
    CO2_coke_sint(t)  "CO2 from sinter (t)"
    CO2_offgas(t)     "CO2 from off-gas flaring (t)";

* CCS variables
Positive Variable
    qCCS(t)             "CCS capacity additions made in period t (only allowed if y(t)=yes), units: tCO2 per period"
    fCCS(t)             "CO2 stream sent to CCS (t per period) = flow through capture unit"
    CO2_captured(t)     "CO2 captured (t per period)"
    CO2_offgas_pot(t)   "CO2 with potential to be emitted from off-gas before CCS (t per period)"
    CO2_atm_offgas(t)   "Residual off-gas CO2 to atmosphere after CCS (t per period)"
    eCCS_el(t)          "Electricity use for CCS (kWh)"
    qCCS_th(t)          "Steam/thermal use for CCS (kWh_th)"
    CO2_total_net(t)    "Net CO2 to atmosphere (t) (sinter process + residual off-gas)";

* remove CCS_cap(t) and CO2_stack_total/CO2_stack_res from your declarations


Variable
    TotalCost           "objective (€)";

* =========================
* Equations
* =========================
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

* =========================
* Equations (append/replace CCS equations only)
* =========================

Equation
    co2_offgas_pot_def(t)   "CO2_t^{off-gas} = (sum gOffgas - D_offgas)*EF_flare, potential before CCS"
    ccs_stream_upper(t)     "f_t^{CCS} ≤ CO2_t^{off-gas}"
    ccs_capacity_upper(t)   "f_t^{CCS} ≤ sum_{y<=t} q_y^{CCS}"
    ccs_capture_eff(t)      "s_t^{CO2} ≤ ω · f_t^{CCS}"
    co2_atm_offgas_def(t)   "CO2^atm_offgas = CO2^off-gas - captured"
    co2_total_net_def(t)    "Net = sinter process CO2 + residual off-gas CO2"
    ccs_el_def(t)           "CCS electricity"
    ccs_th_def(t)           "CCS steam"
    invest_only_y(t)        "Forbid CCS investment outside y(t)"
    ;

* Off-gas CO2 that could be emitted absent CCS (matches LaTeX)
co2_offgas_pot_def(t)..
    CO2_offgas_pot(t) =e= ( sum(s, gOffgas(s,t)) - D_offgas(t) ) * EF_flare;

* f_t^{CCS} ≤ CO2_t^{off-gas}
ccs_stream_upper(t)..
    fCCS(t) =l= CO2_offgas_pot(t);

* f_t^{CCS} ≤ K_t^{CCS} = sum_{y<=t} q_y^{CCS}
ccs_capacity_upper(t)..
    fCCS(t) =l= sum(ty$( y(ty) and ord(ty) <= ord(t) ), qCCS(ty));

* s_t^{CO2} ≤ ω · f_t^{CCS}
ccs_capture_eff(t)..
    CO2_captured(t) =l= omega_ccs * fCCS(t);

* CO2^atm_offgas = CO2^off-gas - s_t^{CO2}
co2_atm_offgas_def(t)..
    CO2_atm_offgas(t) =e= CO2_offgas_pot(t) - CO2_captured(t);

* Net emissions include sinter process CO2 + residual off-gas
co2_total_net_def(t)..
    CO2_total_net(t) =e= CO2_coke_sint(t) + CO2_atm_offgas(t);

* CCS energy links
ccs_el_def(t)..  eCCS_el(t) =e= SEC_CCS_el(t) * CO2_captured(t);
ccs_th_def(t)..  qCCS_th(t) =e= SEC_CCS_th(t) * CO2_captured(t);

* Forbid investment outside y
invest_only_y(t)$(not y(t))..
    qCCS(t) =e= 0;

* =========================
* Constraints
* =========================
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

* Off-gas production (FeMn + SiMn paths)
offgas_def(s,t)..
    gOffgas(s,t) =e=
        sum(fuel_all,
              Y_offgas(s,t,fuel_all)     * uSAF(s,t,fuel_all)
            + Y_offgas_smn(s,t,fuel_all) * uSAF_SiMn(s,t,fuel_all)
        );

CO2_coke_sint_def(t).. CO2_coke_sint(t) =e= EF_coke_sint * pSinter(t);

* Legacy flaring CO2 (still reported)
CO2_offgas_def(t)..
    CO2_offgas(t) =e= ( sum(s, gOffgas(s,t)) - D_offgas(t) ) * EF_flare;

coke_total_def(t).. uTotalCoke_SAF(t) =e= sum(s, uSAF(s,t,'coke'));

capacity_saf(s,t)..  pFeMn(s,t) + pSiMn(s,t) =l= Q_SAF(s,t);
capacity_sinter(t).. pSinter(t)              =l= Q_sint(t);

obj..
    TotalCost =e=
      sum(t,
           FIXEDCOST(t)
         + C_ore(t)        * uOre(t)
         + C_buysinter(t)  * bSinter(t)
         + C_elec(t)       * ( sum(s, eSAF(s,t)) + eSint(t) + eCCS_el(t) )
         + C_ets(t)        * CO2_total_net(t)
         + sum(s, sum(fuel_all, C_fuel(fuel_all,t) * ( uSAF(s,t,fuel_all) + uSAF_SiMn(s,t,fuel_all) )))
         + sum(fuel_lt, C_fuel(fuel_lt,t) * uSint_LT(t,fuel_lt))
         + sum(fuel_ht, C_fuel(fuel_ht,t) * uSint_HT(t,fuel_ht))
         + ( C_CCS_var(t) + C_CCS_TandS(t) ) * CO2_captured(t)
         + C_CCS_steam(t) * qCCS_th(t)
        )
      + sum(t$y(t), C_CCS_capex(t) * qCCS(t));


* =========================
* Model & Test data
* =========================
Model FeMn_SiMn_Opt / all /;

* --- Test data (example numbers) ---

*FeMn Demand
D_FeMn(t)  = 1;
*SiMn Demand
D_SiMn(t)  = 0;

*Conversion rates (ton needed/ton produced)
Y_ore_to_sinter  = 1;
*Conversion rates (ton needed/ton produced)
Y_sinter_to_FeMn = 1.8;
*Conversion rates (ton produced/ton used)
Y_sinter_to_slag = 0.3;

*Carbon emission factor of FeMn fuels (ton CO2/ton fuel)
Y_offgas(s,t,fuel_all)     = 1.8;
*Carbon emission factor of SiMn fuels (ton CO2/ton fuel)
Y_offgas_smn(s,t,fuel_all) = 1.8;

*coke CO2 emission factor (ton CO2/ton coke)
EF_coke_sint = 1.832;

*Flaring factor
EF_flare     = 1.5;
*Offgas demand of the rest of the industrial park
D_offgas(t)  = 0;

*Demand of electricity per FeMn produced (kWh needed/ton produced)
eta_SAF_FeMn(s,t) = 2850;
*Demand of electricity per SiMn produced (kWh needed/ton produced)
nu_SAF_SiMn(s,t)  = 6000;
*Electricity demand of sinter (kWh needed/ton produced)
eta_sint          = 90;

*quantity of heat needed per ton of FeMn produced (kWh needed/ton produced)
TCF_FeMn(s,t) = 2700;
*quantity of heat needed per ton of SiMn produced (kWh needed/ton produced)
TCF_SiMn(s,t) = 4500;
*quantity of heat needed per ton of sinter produced (kWh needed/ton produced)
TCF_sint(t)   = 1000;

*Percentage of total sinter heat needed as high temperature
A_sint_HT_req         = 0;
*Percentage of total FeMn production heat needed as high temperature
A_SAF_HT_req(s)       = 0.2;
*Percentage of total SiMn production heat needed as high temperature
A_SAF_SiMn_HT_req(s)  = 0;

*Percentage of low temperature needed from a particular fuel in SAF
A_saf_lt(s,fuel_lt) = 0;
*Percentage of high temperature needed from a particular fuel in SAF
A_saf_ht(s,fuel_ht) = 0;
*Percentage of low temperature needed from a particular fuel in sinter
A_sint_lt(fuel_lt)  = 0;
*Percentage of high temperature needed from a particular fuel in sinter
A_sint_ht(fuel_ht)  = 0;

*SAF capacity per period in tons
Q_SAF(s,t) = 120000;
*Sinter capacity per period in tons
Q_sint(t)  = 120000;

*Cost of buying ore (euro/ton)
C_ore(t)        = 100;
*Cost of buying sinter (euro/ton)
C_buysinter(t)  = 500;
*Cost of buying coke (euro/ton)
C_coke(t)       = 300;
*Cost of buying electricity (euro/kWh)
C_elec(t)       = 0.010;
*Cost of CO2 emissions (euro/ton)
C_ets(t)        = 100;
*Fixed costs of production CAN BE TAKEN OUT
FIXEDCOST(t)    = 10000;

C_fuel(fuel_all,t) = 0;
C_fuel('coke',t)      = C_coke(t);
C_fuel('oil',t)       = 500;
C_fuel('biooil',t)    = 450;
C_fuel('woodchips',t) = 120;
C_fuel('COgas',t)     = 500000;
C_fuel('hydrogen',t)  = 2000;
C_fuel('natgas',t)    = 150;
C_fuel('biogas',t)    = 130;
C_fuel('biochar',t)   = 500;

* Heat conversion (kWh per tonne fuel) – example LHVs
phi_sint_lt(t,fuel_lt)        = 0;
phi_sint_ht(t,fuel_ht)        = 0;
phi_saf_lt(s,t,fuel_lt)       = 0;
phi_saf_ht(s,t,fuel_ht)       = 0;
phi_saf_smn_lt(s,t,fuel_all)  = 0;
phi_saf_smn_ht(s,t,fuel_all)  = 0;

Scalar
    LHV_coke       /  8000 /
    LHV_biochar    /  7000 /
    LHV_oil        / 11600 /
    LHV_biooil     /  5000 /
    LHV_woodchips  /  2500 /
    LHV_natgas     / 13900 /
    LHV_biogas     /  6000 /
    LHV_COgas      /  2400 /
    LHV_hydrogen   / 33300 /;

phi_sint_ht(t,'coke') = LHV_coke;

phi_saf_ht(s,t,'coke')     = LHV_coke;
phi_saf_ht(s,t,'biochar')  = LHV_biochar;
phi_saf_ht(s,t,'COgas')    = LHV_COgas;
phi_saf_ht(s,t,'hydrogen') = LHV_hydrogen;
phi_saf_ht(s,t,'natgas')   = LHV_natgas;
phi_saf_ht(s,t,'biogas')   = LHV_biogas;

phi_saf_lt(s,t,'oil')       = LHV_oil;
phi_saf_lt(s,t,'biooil')    = LHV_biooil;
phi_saf_lt(s,t,'woodchips') = LHV_woodchips;

phi_saf_smn_ht(s,t,'coke')     = LHV_coke;
phi_saf_smn_ht(s,t,'biochar')  = LHV_biochar;
phi_saf_smn_ht(s,t,'COgas')    = LHV_COgas;
phi_saf_smn_ht(s,t,'hydrogen') = LHV_hydrogen;
phi_saf_smn_ht(s,t,'natgas')   = LHV_natgas;
phi_saf_smn_ht(s,t,'biogas')   = LHV_biogas;

phi_saf_smn_lt(s,t,'oil')       = LHV_oil;
phi_saf_smn_lt(s,t,'biooil')    = LHV_biooil;
phi_saf_smn_lt(s,t,'woodchips') = LHV_woodchips;

* =========================
* Solve
* =========================
Solve FeMn_SiMn_Opt using LP minimizing TotalCost;

Display CO2_coke_sint.l, CO2_offgas_pot.l, CO2_atm_offgas.l;

* ===== Per-period cost breakdown =====
Set costcat / fixed, ore, buysinter, elec, fuel_SAF, fuel_sint, ccs_var_ts, ccs_steam, ets, capex_ccs, total /;
Parameter CostByT(t,costcat) "Cost breakdown by period (EUR)"
          TotalCost_t(t)      "Total cost per period (EUR)"
          TotalCost_check     "Sum over periods (EUR)";

* Components that appear inside the objective’s sum(t, ...)
CostByT(t,'fixed')     = FIXEDCOST(t);
CostByT(t,'ore')       = C_ore(t)       * uOre.l(t);
CostByT(t,'buysinter') = C_buysinter(t) * bSinter.l(t);

* Electricity: SAF + sinter + CCS
CostByT(t,'elec') = C_elec(t) * ( sum(s, eSAF.l(s,t)) + eSint.l(t) + eCCS_el.l(t) );

* Fuels: SAF (FeMn+SiMn) + sinter
CostByT(t,'fuel_SAF')  = sum(s, sum(fuel_all, C_fuel(fuel_all,t) * ( uSAF.l(s,t,fuel_all) + uSAF_SiMn.l(s,t,fuel_all) )));
CostByT(t,'fuel_sint') =   sum(fuel_lt, C_fuel(fuel_lt,t) * uSint_LT.l(t,fuel_lt))
                         + sum(fuel_ht, C_fuel(fuel_ht,t) * uSint_HT.l(t,fuel_ht));

* CCS variable parts
CostByT(t,'ccs_var_ts') = ( C_CCS_var(t) + C_CCS_TandS(t) ) * CO2_captured.l(t);
CostByT(t,'ccs_steam')  = C_CCS_steam(t) * qCCS_th.l(t);

* ETS
CostByT(t,'ets') = C_ets(t) * CO2_total_net.l(t);

* CCS capex only in investment periods y(t)
CostByT(t,'capex_ccs') = 0;
CostByT(t,'capex_ccs')$y(t) = C_CCS_capex(t) * qCCS.l(t);

* Per-period total and cross-check against the model objective
CostByT(t,'total') = CostByT(t,'fixed') + CostByT(t,'ore') + CostByT(t,'buysinter')
                   + CostByT(t,'elec')  + CostByT(t,'fuel_SAF') + CostByT(t,'fuel_sint')
                   + CostByT(t,'ccs_var_ts') + CostByT(t,'ccs_steam') + CostByT(t,'ets')
                   + CostByT(t,'capex_ccs');

TotalCost_t(t)  = CostByT(t,'total');
TotalCost_check = sum(t, TotalCost_t(t));

* ... after computing CostByT, TotalCost_t, TotalCost_check ...

execute_unload 'output_test_2.gdx'
    CO2_offgas_pot.l, CO2_atm_offgas.l,
    pFeMn.l, pSiMn.l, pSinter.l, bSinter.l, eSAF.l, eSint.l,
    CO2_captured.l, CO2_total_net.l, qCCS.l, eCCS_el.l, qCCS_th.l,
    CostByT, TotalCost_t, TotalCost_check, costcat;


* =========================
* Optional diagnostics
* =========================
option decimals=6;

Display CO2_coke_sint.l, CO2_offgas.l;

Parameter CO2_total(t) "Total CO2 per period (t, net)";
CO2_total(t) = CO2_total_net.l(t);
Scalar CO2_total_all "Total CO2 over all periods (t, net)";
CO2_total_all = sum(t, CO2_total(t));
Display CO2_total, CO2_total_all;

Parameter
    ProdTot(t)         "Total FeMn+SiMn (t)"
    OffgasProd(s,t)    "Off-gas produced per SAF (t)"
    OffgasFlared(t)    "Flared off-gas (t)"
    ETS_cost(t)        "ETS cost per period (eur)"
    CO2_intensity(t)   "CO2 intensity vs total product (t CO2/t product)";

ProdTot(t)      = sum(s, pFeMn.l(s,t) + pSiMn.l(s,t));
OffgasProd(s,t) = gOffgas.l(s,t);
OffgasFlared(t) = sum(s, gOffgas.l(s,t)) - D_offgas(t);
ETS_cost(t)     = C_ets(t) * CO2_total(t);

Scalar ep /1e-9/;
CO2_intensity(t) = CO2_total(t) / max(ProdTot(t), ep);

Display ProdTot, OffgasProd, OffgasFlared, ETS_cost, CO2_intensity;