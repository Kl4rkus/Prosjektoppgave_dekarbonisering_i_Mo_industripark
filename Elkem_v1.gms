$Title Elkem FeSi Mo i Rana with CCS (u_fuel in tonnes, phi in kWh/t)

* =========================
* Sets and indices
* =========================
Set t          "time periods" / t1*t30 /;
Set s          "FeSi SAF units" / s1, s2 /;

Set fuel_all   "all fuels"
/ oil, biooil, woodchips, coke, COgas, hydrogen, natgas, biogas, biochar /;

Set fuel_bio(fuel_all) "biogenic fuels"
/ biooil, woodchips, biogas, biochar /;

Set fuel_lt(fuel_all) "low-temp fuels"  / oil, biooil, woodchips /;
Set fuel_ht(fuel_all) "high-temp fuels" / coke, COgas, hydrogen, natgas, biogas, biochar /;

Set y(t)  "CCS/biocarbon investment periods" / t5, t10, t15, t20, t25, t30 /;
Alias (t,ty);

* =========================
* Parameters (data inputs)
* =========================

* FeSi demand (t)
Parameter D_FeSi(t) "FeSi demand (t)";

Parameter D_FeSi(t) /
            t1  66607.8
            t2  112911.1
            t3  89571.5
            t4  39481.7
            t5  83985.7
            t6  117459.3
            t7  102297.2
            t8   103270.4
            t9   107219.7
            t10  111168.9
            t11  115118.2
            t12  119067.4
            t13  123016.7
            t14  126965.9
            t15  130915.1
            t16  134864.4
            t17  138813.6
            t18  142762.9
            t19  146712.1
            t20  150661.4
            t21  154610.6
            t22  158559.8
            t23  162509.1
            t24  166458.3
            t25  170407.6
            t26  174356.8
            t27  178306.1
            t28  182255.3
            t29  186204.5
            t30  190153.8
/;

* Route material yields (per tonne FeSi)
Scalar
    Y_FeSi_SiO2_quartz  "t quartz per t FeSi via SiO2 route"
    Y_FeSi_Si_silicon   "t Si per t FeSi via Si route";

* Off-gas yield (t off-gas per t fuel in SAF)
Parameter
    Y_offgas(s,t,fuel_all) "off-gas yield (t off-gas per t fuel)";

* Emission factor for off-gas
Scalar
    EF_flare  "t CO2 per t off-gas fully oxidized (stack CO2 factor)";
Parameter
    D_offgas(t)   "off-gas demand (t)";

* Electricity intensities (kWh per t FeSi)
Parameter
    eta_SAF_FeSi(s,t)  "kWh per t FeSi";

* Total thermal heat required per tonne of FeSi (kWh/t)
Parameter
    TCF_FeSi(s,t)      "total heat per t FeSi (kWh/t)";

* Minimum HT requirements (fraction of total heat)
Parameter
    A_SAF_HT_req(s)    "min HT share for FeSi";

* Fuel-to-heat conversion factors (phi = kWh produced per t fuel)
Parameter
    phi_saf_lt(s,t,fuel_lt)  "LT heat from LT fuels in SAF [kWh/t fuel]"
    phi_saf_ht(s,t,fuel_ht)  "HT heat from HT fuels in SAF [kWh/t fuel]";

* Per-fuel minimum share / availability: A * LT/HT <= fuel use
Parameter
    A_saf_lt(s,fuel_lt)  "LT fuel availability for SAF"
    A_saf_ht(s,fuel_ht)  "HT fuel availability for SAF";

* Capacities
Parameter Q_SAF(s,t)  "SAF capacity (t FeSi)";

* Costs
scalar p0 /115/;

Parameter
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
C_CCS_capex(t)       = 100;
C_Biocarbon_capex(t) = 0;

L_CCS       = 20;
L_Biocarbon = 20;

* Biogenic CO2 emission factors (t CO2 per t fuel; non-bio = 0)
Parameter EF_bio(fuel_all) "t CO2 per t fuel (nonzero only for biogenic)";
EF_bio(fuel_all) = 0;
EF_bio('biochar')   = 3.0;
EF_bio('woodchips') = 1.8;
EF_bio('biooil')    = 3.0;
EF_bio('biogas')    = 2.0;

* =========================
* Decision variables
* =========================
Positive Variable
* Production and material flows
    pFeSi(s,t)        "Total FeSi production (t)"
    pFeSi_Si(s,t)     "FeSi via Si route (t)"
    pFeSi_SiO2(s,t)   "FeSi via SiO2 route (t)"

* Quartz and silicon purchases
    bQuartz(s,t)      "quartz bought for FeSi via SiO2 (t)"
    bSi(s,t)          "silicon bought for FeSi via Si (t)"

* Electricity (kWh)
    eSAF(s,t)         "SAF electricity (kWh)"

* Heat balances (kWh)
    LT_SAF_FeSi(s,t)  "LT heat in SAF for FeSi (kWh)"
    HT_SAF_FeSi(s,t)  "HT heat in SAF for FeSi (kWh)"

* Fuel uses (tonnes fuel)
    uSAF(s,t,fuel_all) "fuel use in SAF for FeSi (t fuel)"

* Off-gas
    gOffgas(s,t)      "off-gas from SAF (t)"

* Biocarbon capacity investments
    qBiocarbon(t)     "biocarbon capacity additions in period t (t fuel per period)"
    biocap(t)         "available biocarbon capacity in period t (t fuel per period)"

* CCS variables
    qCCS(t)           "CCS capacity additions in period t (tCO2 per period)"
    fCCS(t)           "CO2 stream sent to CCS (t per period)"
    CO2_offgas_pot(t) "CO2 with potential to be emitted before CCS (t per period)"
    CO2_captured(t)   "CO2 captured (t per period)"
    CO2_atm_offgas(t) "Residual CO2 to atmosphere after CCS (t per period)"
    eCCS_el(t)        "Electricity use for CCS (kWh)"
    qCCS_th(t)        "Steam/thermal use for CCS (kWh_th)"
    CO2_total_net(t)  "Net CO2 to atmosphere (t)"
    CO2_bio_pre(t)    "Biogenic CO2 that would be emitted without CCS (t)"
    CO2_ets(t)        "ETS-accounted net CO2 (can be negative)";

Variable
    TotalCost         "objective (€)";

* =========================
* Equations
* =========================
Equation
    dem_FeSi(t)

    pFeSi_split_def(s,t)
    quartz_balance(s,t)
    si_balance(s,t)

    FeSi_energy_total(s,t)
    FeSi_ht_min(s,t)
    lt_saf_fesi_def(s,t)
    lt_saf_fesi_min(s,t,fuel_lt)
    ht_saf_fesi_def(s,t)
    ht_saf_fesi_min(s,t,fuel_ht)

    e_saf_def(s,t)

    offgas_def(s,t)

    capacity_saf(s,t)

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

    co2_bio_pre_def(t)   "Biogenic CO2 pre-capture"
    co2_ets_def(t)       "ETS-accounted emissions"

    obj;

* =========================
* Constraints
* =========================

* Demand: total FeSi production must meet demand
dem_FeSi(t).. sum(s, pFeSi(s,t)) =g= D_FeSi(t);

* Split total FeSi into Si and SiO2 routes
pFeSi_split_def(s,t).. pFeSi(s,t) =e= pFeSi_Si(s,t) + pFeSi_SiO2(s,t);

* Quartz and Si balances
quartz_balance(s,t).. bQuartz(s,t) =e= Y_FeSi_SiO2_quartz * pFeSi_SiO2(s,t);
si_balance(s,t)..     bSi(s,t)     =e= Y_FeSi_Si_silicon  * pFeSi_Si(s,t);

* Energy balance: total LT+HT heat equals total heat requirement
FeSi_energy_total(s,t)..
    LT_SAF_FeSi(s,t) + HT_SAF_FeSi(s,t) =e= TCF_FeSi(s,t) * pFeSi(s,t);

* Minimum HT requirement (can be set to 0 if not needed)
FeSi_ht_min(s,t)..
    A_SAF_HT_req(s) * pFeSi(s,t) =l= HT_SAF_FeSi(s,t);

* LT heat from LT fuels
lt_saf_fesi_def(s,t)..
    LT_SAF_FeSi(s,t) =e= sum(fuel_lt, phi_saf_lt(s,t,fuel_lt) * uSAF(s,t,fuel_lt));

lt_saf_fesi_min(s,t,fuel_lt)..
    A_saf_lt(s,fuel_lt) * LT_SAF_FeSi(s,t) =l= uSAF(s,t,fuel_lt);

* HT heat from HT fuels
ht_saf_fesi_def(s,t)..
    HT_SAF_FeSi(s,t) =e= sum(fuel_ht, phi_saf_ht(s,t,fuel_ht) * uSAF(s,t,fuel_ht));

ht_saf_fesi_min(s,t,fuel_ht)..
    A_saf_ht(s,fuel_ht) * HT_SAF_FeSi(s,t) =l= uSAF(s,t,fuel_ht);

* Electricity use
e_saf_def(s,t)..
    eSAF(s,t) =e= eta_SAF_FeSi(s,t) * pFeSi(s,t);

* Off-gas production
offgas_def(s,t)..
    gOffgas(s,t) =e= sum(fuel_all, Y_offgas(s,t,fuel_all) * uSAF(s,t,fuel_all));

* Capacity constraint for SAF units
capacity_saf(s,t)..  pFeSi(s,t) =l= Q_SAF(s,t);

* Biocarbon capacity accumulation and usage limit
biocap_def(t)..
    biocap(t) =e=
       sum(ty$( y(ty) and ord(t) >= ord(ty) and (ord(t) - ord(ty)) < L_Biocarbon ),
           qBiocarbon(ty));

biocarbon_use_limit(t)..
    sum(s, uSAF(s,t,'biochar')) =l= biocap(t);

* =========================
* CCS equations
* =========================

co2_offgas_pot_def(t)..
    CO2_offgas_pot(t) =e= ( sum(s, gOffgas(s,t)) - D_offgas(t) ) * EF_flare;

ccs_stream_upper(t)..
    fCCS(t) =l= CO2_offgas_pot(t);

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
    CO2_total_net(t) =e= CO2_atm_offgas(t);

ccs_el_def(t)..  eCCS_el(t) =e= SEC_CCS_el(t) * CO2_captured(t);
ccs_th_def(t)..  qCCS_th(t) =e= SEC_CCS_th(t) * CO2_captured(t);

invest_only_y(t)$(not y(t))..
    qCCS(t) =e= 0;

invest_biocarbon_only_y(t)$(not y(t))..
    qBiocarbon(t) =e= 0;

* =========================
* Biogenic CO2 & ETS accounting
* =========================

* Total biogenic CO2 that would be emitted if there were no CCS
co2_bio_pre_def(t)..
    CO2_bio_pre(t) =e=
          sum(s, sum(fuel_bio,
                        EF_bio(fuel_bio) * uSAF(s,t,fuel_bio)));

* ETS-accounted emissions:
*   physical net emissions (CO2_total_net)
*   minus all biogenic CO2 (treated as climate-neutral),
*   which allows negative emissions when CCS captures more
*   than fossil emissions.
co2_ets_def(t)..
    CO2_ets(t) =e= CO2_total_net(t) - CO2_bio_pre(t);

* =========================
* Objective function
* =========================
obj..
    TotalCost =e=
      sum(t,
           FIXEDCOST(t)
         + C_elec(t)       * ( sum(s, eSAF(s,t)) + eCCS_el(t) )
         + C_ets(t)        * CO2_ets(t)
         + sum(s, sum(fuel_all, C_fuel(fuel_all,t) * uSAF(s,t,fuel_all)))
         + ( C_CCS_var(t) + C_CCS_TandS(t) ) * CO2_captured(t)
         + C_CCS_steam(t)  * qCCS_th(t)
         + sum(s, C_quartz(t) * bQuartz(s,t))
         + sum(s, C_Si(t)     * bSi(s,t))
        )
      + sum(t$y(t), C_CCS_capex(t)       * qCCS(t))
      + sum(t$y(t), C_Biocarbon_capex(t) * qBiocarbon(t));

* =========================
* Model & Test data
* =========================
Model Elkem_FeSi_Opt / all /;

* --- Test data (example numbers; adjust to Elkem data) ---

Y_FeSi_SiO2_quartz = 0.4;
Y_FeSi_Si_silicon  = 0.4;

Y_offgas(s,t,fuel_all) = 1.8;
EF_flare              = 1.5;
D_offgas(t)           = 0;

* kWh/t FeSi (example)
eta_SAF_FeSi(s,t) = 6000;
* thermal kWh/t FeSi (example) 
TCF_FeSi(s,t)     = 5000;    

A_SAF_HT_req(s) = 0.2;

A_saf_lt(s,fuel_lt) = 0;
A_saf_ht(s,fuel_ht) = 0;

Q_SAF(s,t) = 120000;

C_elec(t)    = 0.010;
C_ets(t)        = p0 + 17*(ord(t)-1);
FIXEDCOST(t) = 000;

C_quartz(t) = 50;
C_Si(t)     = 250;

C_fuel(fuel_all,t) = 0;
C_fuel('coke',t)      = 300;
C_fuel('oil',t)       = 500;
C_fuel('biooil',t)    = 450;
C_fuel('woodchips',t) = 120;
C_fuel('COgas',t)     = 500;
C_fuel('hydrogen',t)  = 2000;
C_fuel('natgas',t)    = 150;
C_fuel('biogas',t)    = 130;
C_fuel('biochar',t)   = 400;

phi_saf_lt(s,t,fuel_lt) = 0;
phi_saf_ht(s,t,fuel_ht) = 0;

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

* Example mapping of fuels to HT/LT heat
phi_saf_ht(s,t,'coke')     = LHV_coke;
phi_saf_ht(s,t,'biochar')  = LHV_biochar;
phi_saf_ht(s,t,'COgas')    = LHV_COgas;
phi_saf_ht(s,t,'hydrogen') = LHV_hydrogen;
phi_saf_ht(s,t,'natgas')   = LHV_natgas;
phi_saf_ht(s,t,'biogas')   = LHV_biogas;

phi_saf_lt(s,t,'oil')       = LHV_oil;
phi_saf_lt(s,t,'biooil')    = LHV_biooil;
phi_saf_lt(s,t,'woodchips') = LHV_woodchips;

* =========================
* Solve
* =========================
Solve Elkem_FeSi_Opt using LP minimizing TotalCost;

* ===== Per-period cost breakdown =====
Set costcat / fixed, elec, fuel_SAF,
              quartz, silicon, ccs_var_ts, ccs_steam, ets,
              capex_ccs, capex_biocarb, total /;

Parameter
    CostByT(t,costcat) "Cost breakdown by period (EUR)"
    TotalCost_t(t)     "Total cost per period (EUR)"
    TotalCost_check    "Sum over periods (EUR)";

CostByT(t,'fixed')     = FIXEDCOST(t);

CostByT(t,'elec') = C_elec(t) * ( sum(s, eSAF.l(s,t)) + eCCS_el.l(t) );

CostByT(t,'fuel_SAF')  = sum(s, sum(fuel_all, C_fuel(fuel_all,t) * uSAF.l(s,t,fuel_all)));

CostByT(t,'quartz')  = sum(s, C_quartz(t) * bQuartz.l(s,t));
CostByT(t,'silicon') = sum(s, C_Si(t)     * bSi.l(s,t));

CostByT(t,'ccs_var_ts') = ( C_CCS_var(t) + C_CCS_TandS(t) ) * CO2_captured.l(t);
CostByT(t,'ccs_steam')  = C_CCS_steam(t) * qCCS_th.l(t);

CostByT(t,'ets') = C_ets(t) * CO2_ets.l(t);

CostByT(t,'capex_ccs')     = 0;
CostByT(t,'capex_ccs')$y(t)     = C_CCS_capex(t)       * qCCS.l(t);
CostByT(t,'capex_biocarb') = 0;
CostByT(t,'capex_biocarb')$y(t) = C_Biocarbon_capex(t) * qBiocarbon.l(t);

CostByT(t,'total') =  CostByT(t,'fixed')
                    + CostByT(t,'elec')
                    + CostByT(t,'fuel_SAF')
                    + CostByT(t,'quartz')
                    + CostByT(t,'silicon')
                    + CostByT(t,'ccs_var_ts')
                    + CostByT(t,'ccs_steam')
                    + CostByT(t,'ets')
                    + CostByT(t,'capex_ccs')
                    + CostByT(t,'capex_biocarb');

TotalCost_t(t)  = CostByT(t,'total');
TotalCost_check = sum(t, TotalCost_t(t));

* Fuel use per period & fuel (t)
Parameter FuelUse_t_f(t,fuel_all) "Fuel use per period and fuel (t)";
FuelUse_t_f(t,fuel_all) = sum(s, uSAF.l(s,t,fuel_all));

execute_unload 'output_Elkem_FeSi.gdx'
    CO2_offgas_pot.l, CO2_atm_offgas.l,
    pFeSi.l, pFeSi_Si.l, pFeSi_SiO2.l,
    eSAF.l,
    bQuartz.l, bSi.l,
    uSAF.l,
    CO2_captured.l, CO2_total_net.l, qCCS.l, eCCS_el.l, qCCS_th.l,
    qBiocarbon.l, biocap.l,
    CostByT, TotalCost_t, TotalCost_check, costcat,
    FuelUse_t_f;

option decimals=6;

Parameter CO2_total(t) "Total CO2 per period (t, net)";
CO2_total(t) = CO2_total_net.l(t);
Scalar CO2_total_all "Total CO2 over all periods (t, net)";
CO2_total_all = sum(t, CO2_total(t));
Display CO2_total, CO2_total_all;

Parameter
    ProdTot(t)         "Total FeSi (t)"
    ETS_cost(t)        "ETS cost per period (eur)"
    CO2_intensity(t)   "CO2 intensity vs total product (t CO2/t FeSi)";

ProdTot(t)      = sum(s, pFeSi.l(s,t));
ETS_cost(t)     = C_ets(t) * CO2_ets.l(t);

Scalar ep /1e-9/;
CO2_intensity(t) = CO2_total(t) / max(ProdTot(t), ep);

Display ProdTot, ETS_cost, CO2_intensity;
Display FuelUse_t_f;
