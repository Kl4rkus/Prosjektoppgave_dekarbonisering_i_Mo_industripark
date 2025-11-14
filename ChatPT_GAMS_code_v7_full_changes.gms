$Title FeMn / SiMn production & energy system with CCS (u_fuel in tonnes, phi in kWh/t)

* =========================
* Sets and indices
* =========================
Set t          "time periods" / t1*t30 /;
Set s          "SAF units"    / s1, s2 /;
Set fuel_all   "all fuels"
/ oil, biooil, woodchips, coke, COgas, hydrogen, natgas, biogas, biochar /;
Set fuel_lt(fuel_all) "low-temp fuels"  / oil, biooil, woodchips /;
Set fuel_ht(fuel_all) "high-temp fuels" / coke, COgas, hydrogen, natgas, biogas, biochar /;
Alias (t,tt);
Set y(t)  "CCS/biocarbon investment periods (subset of T, e.g. every 5th t)" / t5, t10,t15,t20, t25, t30 /;
Alias (t,ty);

* =========================
* Parameters (data inputs)
* =========================

* Demands
Parameter
    D_FeMn(t)    "FeMn demand (t)"
    D_SiMn(t)    "SiMn demand (t)";

* Yields (scalars)
Scalar
    Y_ore_to_sinter      "ore per t sinter"
    Y_sinter_to_FeMn     "sinter per t FeMn"
    Y_sinter_to_slag     "slag per t sinter used"
* For SiMn material balance
    Y_sinter_SiMn        "coeff on sinter in SiMn material balance"
    Y_slag_SiMn          "coeff on slag in SiMn material balance"
* SiMn route material factors
    Y_SiMn_SiO2_quartz   "t quartz per t SiMn via SiO2 route"
    Y_SiMn_Si_silicon    "t Si per t SiMn via Si route";

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
    nu_SAF_SiMn(s,t)   "kWh per t SiMn base (not used directly)"
    nu_SAF_Si(s,t)     "kWh per t SiMn via Si route"
    nu_SAF_SiO2(s,t)   "kWh per t SiMn via SiO2 route";
Scalar
    eta_sint           "kWh per t sinter";

* Total thermal heat required per tonne of product (kWh/t)
Parameter
    TCF_FeMn(s,t)      "total heat per t FeMn (kWh/t)"
    TCF_SiMn(s,t)      "total heat per t SiMn base (kWh/t)"
    TCF_SiMn_Si(s,t)   "total heat per t SiMn via Si (kWh/t)"
    TCF_SiMn_SiO2(s,t) "total heat per t SiMn via SiO2 (kWh/t)"
    TCF_sint(t)        "total heat per t sinter (kWh/t)";

* Minimum HT requirements (fractions of total heat)
Parameter
    A_SAF_HT_req(s)       "min HT share for FeMn"
    A_SAF_SiMn_HT_req(s)  "min HT share for SiMn";
Scalar
    A_sint_HT_req         "min HT share for sinter";

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
    C_coke(t)       "cost per t coke"
    C_elec(t)       "cost per kWh electricity"
    C_ets(t)        "cost per t CO2"
    FIXEDCOST(t)    "fixed cost in period t"
    C_fuel(fuel_all,t)   "cost per t fuel"
    C_quartz(t)     "cost per t quartz"
    C_Si(t)         "cost per t silicon";

* =========================
* CCS & BIOCARBON PARAMETERS (LP)
* =========================
Scalar
    omega_ccs   "capture efficiency (0..1)" /0.9/
    L_CCS       "CCS lifetime (number of periods)"
    L_Biocarbon "Biocarbon lifetime (number of periods)";
Parameter
    SEC_CCS_el(t)      "electricity for CCS (kWh per tCO2 captured)"
    SEC_CCS_th(t)      "steam/heat for CCS (kWh_th per tCO2 captured)"
    C_CCS_var(t)       "CCS variable O&M (€/tCO2 captured)"
    C_CCS_TandS(t)     "Transport & storage (€/tCO2 captured)"
    C_CCS_steam(t)     "Steam price for CCS (€/kWh_th)"
    C_CCS_capex(t)     "Capex cost for CCS in investment periods y (€/ (tCO2 per period))"
    C_Biocarbon_capex(t) "Capex cost for biocarbon capacity (€/ (t fuel per period))";

* Initialize so model runs if no data provided
SEC_CCS_el(t)        = 0;
SEC_CCS_th(t)        = 0;
C_CCS_var(t)         = 50;
C_CCS_TandS(t)       = 0;
C_CCS_steam(t)       = 0;
C_CCS_capex(t)       = 500;
C_Biocarbon_capex(t) = 0;

L_CCS       = 20;
L_Biocarbon = 20;

* =========================
* Decision variables
* =========================
Positive Variable
* Production and material flows
    pFeMn(s,t)        "FeMn production (t)"
    pSiMn(s,t)        "SiMn total production (t)"
    pSinter(t)        "sinter production (t)"
    bSinter(t)        "sinter bought (t)"
    uOre(t)           "ore use (t)"
    uSinterFeMn(s,t)  "sinter used to produce FeMn (t)"
    uSinterSiMn(s,t)  "sinter used to produce SiMn (t)"

* Route-split SiMn production & sinter use
    pSiMn_Si(s,t)         "SiMn via Si route (t)"
    pSiMn_SiO2(s,t)       "SiMn via SiO2 route (t)"
    uSinterSiMn_Si(s,t)   "sinter to SiMn via Si route (t)"
    uSinterSiMn_SiO2(s,t) "sinter to SiMn via SiO2 route (t)"

* Quartz and silicon purchases
    bQuartz(s,t)      "quartz bought for SiMn via SiO2 (t)"
    bSi(s,t)          "silicon bought for SiMn via Si (t)"

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
    uTotalCoke_SAF(t) "total coke in SAF (t fuel)"
    CO2_coke_sint(t)  "CO2 from sinter (t)"
    CO2_offgas(t)     "CO2 from off-gas flaring (t)"

* Biocarbon capacity investments
    qBiocarbon(t)     "biocarbon capacity additions in period t (t fuel per period)"
    biocap(t)         "available biocarbon capacity in period t (t fuel per period)"

* CCS variables
    qCCS(t)           "CCS capacity additions in period t (tCO2 per period)"
    fCCS(t)           "CO2 stream sent to CCS (t per period)"
    CO2_captured(t)   "CO2 captured (t per period)"
    CO2_offgas_pot(t) "CO2 with potential to be emitted from off-gas before CCS (t per period)"
    CO2_atm_offgas(t) "Residual off-gas CO2 to atmosphere after CCS (t per period)"
    eCCS_el(t)        "Electricity use for CCS (kWh)"
    qCCS_th(t)        "Steam/thermal use for CCS (kWh_th)"
    CO2_total_net(t)  "Net CO2 to atmosphere (t) (sinter process + residual off-gas)";

Variable
    TotalCost         "objective (€)";

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
    pSiMn_split_def(s,t)
    uSinterSiMn_split_def(s,t)
    quartz_balance(s,t)
    si_balance(s,t)

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

    biocap_def(t)
    biocarbon_use_limit(t)

    co2_offgas_pot_def(t)
    ccs_stream_upper(t)
    ccs_capacity_upper(t)
    ccs_capture_eff(t)
    co2_atm_offgas_def(t)
    co2_total_net_def(t)
    ccs_el_def(t)
    ccs_th_def(t)
    invest_only_y(t)
    invest_biocarbon_only_y(t)

    obj;

* =========================
* Constraints
* =========================

dem_FeMn(t).. sum(s, pFeMn(s,t)) =g= D_FeMn(t);
dem_SiMn(t).. sum(s, pSiMn(s,t)) =g= D_SiMn(t);

* Split total SiMn into Si and SiO2 routes
pSiMn_split_def(s,t).. pSiMn(s,t) =e= pSiMn_Si(s,t) + pSiMn_SiO2(s,t);
uSinterSiMn_split_def(s,t)..
    uSinterSiMn(s,t) =e= uSinterSiMn_Si(s,t) + uSinterSiMn_SiO2(s,t);

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

* SiMn material balance including slag & sinter
SiMn_material_balance(t)..
    Y_sinter_SiMn * sum(s, uSinterSiMn_Si(s,t) + uSinterSiMn_SiO2(s,t))
  + Y_slag_SiMn   * pSlagTot(t)
    =e= sum(s, pSiMn_Si(s,t) + pSiMn_SiO2(s,t));

* Quartz and Si balances
quartz_balance(s,t).. bQuartz(s,t) =e= Y_SiMn_SiO2_quartz * pSiMn_SiO2(s,t);
si_balance(s,t)..     bSi(s,t)     =e= Y_SiMn_Si_silicon  * pSiMn_Si(s,t);

lt_saf_simn_def(s,t)..
    LT_SAF_SiMn(s,t) =e= sum(fuel_all, phi_saf_smn_lt(s,t,fuel_all) * uSAF_SiMn(s,t,fuel_all));
ht_saf_simn_def(s,t)..
    HT_SAF_SiMn(s,t) =e= sum(fuel_all, phi_saf_smn_ht(s,t,fuel_all) * uSAF_SiMn(s,t,fuel_all));

* Route-specific SiMn heat
SiMn_energy_total(s,t)..
    LT_SAF_SiMn(s,t) + HT_SAF_SiMn(s,t)
    =e= TCF_SiMn_Si(s,t)   * pSiMn_Si(s,t)
       + TCF_SiMn_SiO2(s,t)* pSiMn_SiO2(s,t);

SiMn_ht_min(s,t)..
    A_SAF_SiMn_HT_req(s) * (pSiMn_Si(s,t) + pSiMn_SiO2(s,t)) =l= HT_SAF_SiMn(s,t);

* Route-specific electricity
e_saf_def(s,t)..
    eSAF(s,t) =e= eta_SAF_FeMn(s,t) * pFeMn(s,t)
              + nu_SAF_Si(s,t)      * pSiMn_Si(s,t)
              + nu_SAF_SiO2(s,t)    * pSiMn_SiO2(s,t);

* Off-gas production (FeMn + SiMn paths)
offgas_def(s,t)..
    gOffgas(s,t) =e=
        sum(fuel_all,
              Y_offgas(s,t,fuel_all)     * uSAF(s,t,fuel_all)
            + Y_offgas_smn(s,t,fuel_all) * uSAF_SiMn(s,t,fuel_all)
        );

CO2_coke_sint_def(t).. CO2_coke_sint(t) =e= EF_coke_sint * pSinter(t);

CO2_offgas_def(t)..
    CO2_offgas(t) =e= ( sum(s, gOffgas(s,t)) - D_offgas(t) ) * EF_flare;

coke_total_def(t).. uTotalCoke_SAF(t) =e= sum(s, uSAF(s,t,'coke'));

capacity_saf(s,t)..  pFeMn(s,t) + pSiMn(s,t) =l= Q_SAF(s,t);
capacity_sinter(t).. pSinter(t)              =l= Q_sint(t);

* Biocarbon capacity accumulation and usage limit
biocap_def(t)..
    biocap(t) =e=
       sum(ty$( y(ty) and ord(t) >= ord(ty) and (ord(t) - ord(ty)) < L_Biocarbon ),
           qBiocarbon(ty));

biocarbon_use_limit(t)..
    sum(s, uSAF(s,t,'biochar') + uSAF_SiMn(s,t,'biochar'))
  + uSint_HT(t,'biochar')
    =l= biocap(t);

* =========================
* CCS equations
* =========================

co2_offgas_pot_def(t)..
    CO2_offgas_pot(t) =e= ( sum(s, gOffgas(s,t)) - D_offgas(t) ) * EF_flare;

ccs_stream_upper(t)..      fCCS(t) =l= CO2_offgas_pot(t);

* Lifetime-limited CCS capacity
ccs_capacity_upper(t)..
    fCCS(t) =l=
       sum(ty$( y(ty) and ord(t) >= ord(ty) and (ord(t) - ord(ty)) < L_CCS ),
           qCCS(ty));

ccs_capture_eff(t)..
    CO2_captured(t) =l= omega_ccs * fCCS(t);

co2_atm_offgas_def(t)..
    CO2_atm_offgas(t) =e= CO2_offgas_pot(t) - CO2_captured(t);

co2_total_net_def(t)..
    CO2_total_net(t) =e= CO2_coke_sint(t) + CO2_atm_offgas(t);

ccs_el_def(t)..  eCCS_el(t) =e= SEC_CCS_el(t) * CO2_captured(t);
ccs_th_def(t)..  qCCS_th(t) =e= SEC_CCS_th(t) * CO2_captured(t);

invest_only_y(t)$(not y(t))..
    qCCS(t) =e= 0;

invest_biocarbon_only_y(t)$(not y(t))..
    qBiocarbon(t) =e= 0;

* =========================
* Objective function
* =========================
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
         + sum(s, C_quartz(t) * bQuartz(s,t))
         + sum(s, C_Si(t)     * bSi(s,t))
        )
      + sum(t$y(t), C_CCS_capex(t)        * qCCS(t))
      + sum(t$y(t), C_Biocarbon_capex(t)  * qBiocarbon(t));

* =========================
* Model & Test data
* =========================
Model FeMn_SiMn_Opt / all /;

* --- Test data (example numbers) ---

D_FeMn(t)  = 1000;
D_SiMn(t)  = 1000;

Y_ore_to_sinter  = 1;
Y_sinter_to_FeMn = 1.8;
Y_sinter_to_slag = 0.3;

Y_sinter_SiMn      = 1.8;
Y_slag_SiMn        = 0.6;
Y_SiMn_SiO2_quartz = 0.4;
Y_SiMn_Si_silicon  = 0.4;

Y_offgas(s,t,fuel_all)     = 1.8;
Y_offgas_smn(s,t,fuel_all) = 1.8;

EF_coke_sint = 1.832;
EF_flare     = 1.5;
D_offgas(t)  = 0;

eta_SAF_FeMn(s,t) = 2850;
nu_SAF_SiMn(s,t)  = 6000;
nu_SAF_Si(s,t)    = 2500;
nu_SAF_SiO2(s,t)  = 4500;

eta_sint          = 90;

TCF_FeMn(s,t) = 2700;
TCF_SiMn(s,t) = 4500;
TCF_SiMn_Si(s,t)   = 0.8 * TCF_SiMn(s,t);
TCF_SiMn_SiO2(s,t) =       TCF_SiMn(s,t);
TCF_sint(t)   = 1000;

A_sint_HT_req         = 0;
A_SAF_HT_req(s)       = 0.2;
A_SAF_SiMn_HT_req(s)  = 0;

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

C_quartz(t) = 0;
C_Si(t)     = 0;

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
Set costcat / fixed, ore, buysinter, elec, fuel_SAF, fuel_sint,
              quartz, silicon, ccs_var_ts, ccs_steam, ets,
              capex_ccs, capex_biocarb, total /;
Parameter CostByT(t,costcat) "Cost breakdown by period (EUR)"
          TotalCost_t(t)      "Total cost per period (EUR)"
          TotalCost_check     "Sum over periods (EUR)";

CostByT(t,'fixed')     = FIXEDCOST(t);
CostByT(t,'ore')       = C_ore(t)       * uOre.l(t);
CostByT(t,'buysinter') = C_buysinter(t) * bSinter.l(t);

CostByT(t,'elec') = C_elec(t) * ( sum(s, eSAF.l(s,t)) + eSint.l(t) + eCCS_el.l(t) );

CostByT(t,'fuel_SAF')  = sum(s, sum(fuel_all, C_fuel(fuel_all,t) * ( uSAF.l(s,t,fuel_all) + uSAF_SiMn.l(s,t,fuel_all) )));
CostByT(t,'fuel_sint') =   sum(fuel_lt, C_fuel(fuel_lt,t) * uSint_LT.l(t,fuel_lt))
                         + sum(fuel_ht, C_fuel(fuel_ht,t) * uSint_HT.l(t,fuel_ht));

CostByT(t,'quartz')  = sum(s, C_quartz(t) * bQuartz.l(s,t));
CostByT(t,'silicon') = sum(s, C_Si(t)     * bSi.l(s,t));

CostByT(t,'ccs_var_ts') = ( C_CCS_var(t) + C_CCS_TandS(t) ) * CO2_captured.l(t);
CostByT(t,'ccs_steam')  = C_CCS_steam(t) * qCCS_th.l(t);

CostByT(t,'ets') = C_ets(t) * CO2_total_net.l(t);

CostByT(t,'capex_ccs')     = 0;
CostByT(t,'capex_ccs')$y(t)     = C_CCS_capex(t)       * qCCS.l(t);
CostByT(t,'capex_biocarb') = 0;
CostByT(t,'capex_biocarb')$y(t) = C_Biocarbon_capex(t) * qBiocarbon.l(t);

CostByT(t,'total') = CostByT(t,'fixed') + CostByT(t,'ore') + CostByT(t,'buysinter')
                   + CostByT(t,'elec')  + CostByT(t,'fuel_SAF') + CostByT(t,'fuel_sint')
                   + CostByT(t,'quartz') + CostByT(t,'silicon')
                   + CostByT(t,'ccs_var_ts') + CostByT(t,'ccs_steam') + CostByT(t,'ets')
                   + CostByT(t,'capex_ccs') + CostByT(t,'capex_biocarb');

TotalCost_t(t)  = CostByT(t,'total');
TotalCost_check = sum(t, TotalCost_t(t));

execute_unload 'output_test_2.gdx'
    CO2_offgas_pot.l, CO2_atm_offgas.l,
    pFeMn.l, pSiMn.l, pSiMn_Si.l, pSiMn_SiO2.l,
    pSinter.l, bSinter.l, eSAF.l, eSint.l,
    bQuartz.l, bSi.l,
    CO2_captured.l, CO2_total_net.l, qCCS.l, eCCS_el.l, qCCS_th.l,
    qBiocarbon.l, biocap.l,
    CostByT, TotalCost_t, TotalCost_check, costcat;

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
