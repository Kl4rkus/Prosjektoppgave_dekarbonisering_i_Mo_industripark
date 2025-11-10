$Title FeMn / SiMn production & energy system + CCS (u_fuel in tonnes, phi in kWh/t)

*-----------------------------*
* Sets and indices            *
*-----------------------------*
Set
    t        "time periods"             / t1 /
    ti (t)
    s        "SAF units"               / s1, s2 /
    fuel_all "all fuels"
/ oil, biooil, woodchips, coke, COgas, hydrogen, natgas, biogas, biochar /;

* fuel_lt and fuel_ht are subsets of fuel_all
Set fuel_lt(fuel_all) "low-temp fuels"  / oil, biooil, woodchips /;
Set fuel_ht(fuel_all) "high-temp fuels" / coke, COgas, hydrogen, natgas, biogas, biochar /;

Alias (t,tt);

* --- Helper mapping: cumulative "tt <= t" (constant, no dynamic subsets) ---
Parameter cumMap(t,tt) "1 if tt is not after t (ord(tt)<=ord(t))";
cumMap(t,tt) = 0;
cumMap(t,tt)$(ord(tt) <= ord(t)) = 1;

* --- Investment-allowed mask (1 = invest allowed this period; 0 = not) ---
Parameter investAllowed(t) "Mask for CCS invest periods (1 allowed, 0 blocked)";
investAllowed(t) = 1; 
* If you later add more periods (e.g., /t1*t20/), you can enforce 'every 5 periods' with:
* investAllowed(t) = 0;
* investAllowed(t)$(mod(ord(t)-1,5)=0) = 1;

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

* Off-gas yield (t off-gas per t fuel in SAF)
Parameter
    Y_offgas(s,t,fuel_all)     "off-gas yield (t/t fuel) - FeMn path"
    Y_offgas_smn(s,t,fuel_all) "off-gas yield (t/t fuel) - SiMn path";

* Emission factors and off-gas sink
Scalar
    EF_coke_sint  "t CO2 per t sinter output"
    EF_flare      "t CO2 per t off-gas flared";
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

* -------- CCS parameters --------
Scalar
    omega           "capture factor (fraction of CO2 in flared stream captured, 0..1)";
Parameter
    C_CAPEX_CCS(t)  "CAPEX €/ (tCO2/period) for CCS capacity added in period t"
    C_FIX_CCS(t)    "fixed O&M €/ (tCO2/period) on installed CCS capacity"
    C_VAR_CCS(t)    "variable €/tCO2 captured";

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

* Coke accounting and emissions
    uTotalCoke_SAF(t) "total coke in SAF (FeMn path, t fuel)"
    CO2_coke_sint(t)  "CO2 from sinter (t)"
    CO2_offgas(t)     "CO2 from off-gas flaring after CCS (t)"

* -------- CCS variables --------
    sCCS(t)           "CO2 captured by CCS in period t (tCO2)"
    KCCS(t)           "Available CCS capacity in period t (tCO2/period)"
    Fflare(t)         "Flared off-gas in period t (t off-gas)"
    qCCS(t)           "CCS capacity addition in period t (tCO2/period)";

Variable
    TotalCost         "objective (€)";

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
    flare_lower(t)
    flare_upper(t)
    CO2_offgas_def(t)

    coke_total_def(t)

    capacity_saf(s,t)
    capacity_sinter(t)

* ---- CCS equations ----
    invest_mask(t)
    ccs_stock(t)
    ccs_cap_bound(t)
    ccs_phys_bound(t)

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

* --- Off-gas production (FeMn + SiMn paths) ---
offgas_def(s,t)..
    gOffgas(s,t) =e=
        sum(fuel_all,
              Y_offgas(s,t,fuel_all)     * uSAF(s,t,fuel_all)
            + Y_offgas_smn(s,t,fuel_all) * uSAF_SiMn(s,t,fuel_all)
        );

CO2_coke_sint_def(t).. CO2_coke_sint(t) =e= EF_coke_sint * pSinter(t);

* --- Flared off-gas bounds (non-negative) ---
flare_lower(t).. Fflare(t) =g= sum(s, gOffgas(s,t)) - D_offgas(t);
flare_upper(t).. Fflare(t) =l= sum(s, gOffgas(s,t));

* --- Net CO2 from flaring AFTER CCS capture credit ---
CO2_offgas_def(t)..
    CO2_offgas(t) =e= EF_flare * Fflare(t) - sCCS(t);

coke_total_def(t).. uTotalCoke_SAF(t) =e= sum(s, uSAF(s,t,'coke'));

capacity_saf(s,t)..  pFeMn(s,t) + pSiMn(s,t) =l= Q_SAF(s,t);
capacity_sinter(t).. pSinter(t)              =l= Q_sint(t);

* -------- CCS: investment mask, stock, capacity/use, physics --------
invest_mask(t)..  qCCS(t) =e= investAllowed(t) * qCCS(t);
ccs_stock(t)..    KCCS(t) =e= sum(tt, cumMap(t,tt) * qCCS(tt));
ccs_cap_bound(t)..sCCS(t) =l= KCCS(t);
ccs_phys_bound(t)..sCCS(t) =l= omega * EF_flare * Fflare(t);

* Objective (adds CAPEX, fixed and variable CCS costs)
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
               + C_FIX_CCS(t)    * KCCS(t)
               + C_VAR_CCS(t)    * sCCS(t)
               )
               + sum(t, C_CAPEX_CCS(t) * qCCS(t));

Model FeMn_SiMn_Opt / all /;

*-----------------------------*
* Test data (example numbers) *
*-----------------------------*

D_FeMn(t)  = 1000;
D_SiMn(t)  = 1000;

*Y_ore_to_sinter is tonne of ore needed per tonne of sinter produced
Y_ore_to_sinter  = 1;
*Y_sinter_to_FeMn is tonne of sinter needed per tonne of FeMn produced
Y_sinter_to_FeMn = 1.8;
*Y_sinter_to_slag is tonne of slag produced per tonne used
Y_sinter_to_slag = 0.3;

* Off-gas yield in t per t fuel (placeholder uniform)
Y_offgas(s,t,fuel_all)     = 1.8;
Y_offgas_smn(s,t,fuel_all) = 1.8;

EF_coke_sint = 1.832;
EF_flare     = 1.34;
D_offgas(t)  = 0;

eta_SAF_FeMn(s,t) = 2850;
nu_SAF_SiMn(s,t)  = 6000;
eta_sint          = 90;

* Total thermal heat intensities (kWh/t product).
* Adjust to your process; set to 0 if no external thermal fuel is used there.
TCF_FeMn(s,t) = 2700;
TCF_SiMn(s,t) = 4500;
TCF_sint(t)   = 1000;

* Minimum HT requirements (kWh/t product)
A_sint_HT_req         = 0;
A_SAF_HT_req(s)       = 0.2;
A_SAF_SiMn_HT_req(s)  = 0;

* Minimum-per-fuel availability coefficients (set to 0 if not binding)
A_saf_lt(s,fuel_lt) = 0;
A_saf_ht(s,fuel_ht) = 0;
A_sint_lt(fuel_lt)  = 0;
A_sint_ht(fuel_ht)  = 0;

Q_SAF(s,t) = 120000;
Q_sint(t)  = 120000;

C_ore(t)        = 100;
C_buysinter(t)  = 500;
C_coke(t)       = 300;
C_elec(t)       = 0.010;
C_ets(t)        = 100;
FIXEDCOST(t)    = 10000;

* Initialize fuel prices (€/t fuel)
C_fuel(fuel_all,t) = 0;
C_fuel('coke',t)      = C_coke(t);
C_fuel('oil',t)       = 500;
C_fuel('biooil',t)    = 450;
C_fuel('woodchips',t) = 120;
C_fuel('COgas',t)     = 50;
C_fuel('hydrogen',t)  = 2000;
C_fuel('natgas',t)    = 150;
C_fuel('biogas',t)    = 130;
C_fuel('biochar',t)   = 350;

* -------- CCS test data (tweak as needed) --------
omega            = 0.9;
* capture fraction on flared CO2 (0..1)
C_CAPEX_CCS(t)   = 0;
* €/ (tCO2/period) at invest periods
C_FIX_CCS(t)     = 0;
* €/ (tCO2/period) installed capacity
C_VAR_CCS(t)     = 0;
* €/tCO2 captured

* -----------------------------
* phi values: kWh per tonne fuel (LHV-based examples).
* Replace with plant data / efficiency factors as needed.
* -----------------------------

* Zero all phi before setting
phi_sint_lt(t,fuel_lt)        = 0;
phi_sint_ht(t,fuel_ht)        = 0;
phi_saf_lt(s,t,fuel_lt)       = 0;
phi_saf_ht(s,t,fuel_ht)       = 0;
phi_saf_smn_lt(s,t,fuel_all)  = 0;
phi_saf_smn_ht(s,t,fuel_all)  = 0;

* Example LHVs [kWh/t fuel]
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

* Sinter only uses coke for HT
phi_sint_ht(t,'coke') = LHV_coke;

* SAF FeMn: HT fuels
phi_saf_ht(s,t,'coke')     = LHV_coke;
phi_saf_ht(s,t,'biochar')  = LHV_biochar;
phi_saf_ht(s,t,'COgas')    = LHV_COgas;
phi_saf_ht(s,t,'hydrogen') = LHV_hydrogen;
phi_saf_ht(s,t,'natgas')   = LHV_natgas;
phi_saf_ht(s,t,'biogas')   = LHV_biogas;

* SAF FeMn: LT fuels
phi_saf_lt(s,t,'oil')       = LHV_oil;
phi_saf_lt(s,t,'biooil')    = LHV_biooil;
phi_saf_lt(s,t,'woodchips') = LHV_woodchips;

* SAF SiMn: mirror mapping (adjust if different)
phi_saf_smn_ht(s,t,'coke')     = LHV_coke;
phi_saf_smn_ht(s,t,'biochar')  = LHV_biochar;
phi_saf_smn_ht(s,t,'COgas')    = LHV_COgas;
phi_saf_smn_ht(s,t,'hydrogen') = LHV_hydrogen;
phi_saf_smn_ht(s,t,'natgas')   = LHV_natgas;
phi_saf_smn_ht(s,t,'biogas')   = LHV_biogas;

phi_saf_smn_lt(s,t,'oil')       = LHV_oil;
phi_saf_smn_lt(s,t,'biooil')    = LHV_biooil;
phi_saf_smn_lt(s,t,'woodchips') = LHV_woodchips;

*-----------------------------*
* Solve                       *
*-----------------------------*
Solve FeMn_SiMn_Opt using LP minimizing TotalCost;

execute_unload 'output_test_2.gdx'
    CO2_offgas.l, pFeMn.l, pSiMn.l, pSinter.l, bSinter.l, eSAF.l, eSint.l, sCCS.l, qCCS.l, KCCS.l, Fflare.l;

*-----------------------------*
* Optional diagnostics        *
*-----------------------------*
option decimals = 6;

* Show the two CO2 components by period
Display CO2_coke_sint.l, CO2_offgas.l, sCCS.l, KCCS.l, qCCS.l, Fflare.l;

* Total CO2 by period and overall
Parameter CO2_total(t) "Total CO2 per period (t)";
CO2_total(t) = CO2_coke_sint.l(t) + CO2_offgas.l(t);
Scalar CO2_total_all "Total CO2 over all periods (t)";
CO2_total_all = sum(t, CO2_total(t));
Display CO2_total, CO2_total_all;

* Useful breakdowns / diagnostics
Parameter
    ProdTot(t)         "Total FeMn+SiMn (t)"
    OffgasProd(s,t)    "Off-gas produced per SAF (t)"
    OffgasFlared(t)    "Flared off-gas (t)"
    ETS_cost(t)        "ETS cost per period (eur)"
    CO2_intensity(t)   "CO2 intensity vs total product (t CO2/t product)";

ProdTot(t)      = sum(s, pFeMn.l(s,t) + pSiMn.l(s,t));
OffgasProd(s,t) = gOffgas.l(s,t);
OffgasFlared(t) = Fflare.l(t);
ETS_cost(t)     = C_ets(t) * CO2_total(t);

Scalar ep /1e-9/;
CO2_intensity(t) = CO2_total(t) / max(ProdTot(t), ep);

Display ProdTot, OffgasProd, OffgasFlared, ETS_cost, CO2_intensity;
