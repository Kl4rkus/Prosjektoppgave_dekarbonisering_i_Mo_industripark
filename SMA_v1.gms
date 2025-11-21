$Title SMA CaCO3 with CCS (u_fuel in tonnes, phi in kWh/t)

* =========================
* Sets and indices
* =========================
Set t          "time periods" / t1*t30 /;
Set s          "SMA units (e.g. shaft kiln, electric furnace)" / s1, s2 /;

Set fuel_all   "all fuels"
/ oil, biooil, woodchips, coke, COgas, hydrogen, natgas, biogas, biochar /;

Set fuel_bio(fuel_all) "biogenic fuels"
/ biooil, woodchips, biogas, biochar /;

Set fuel_lt(fuel_all) "low-temp fuels"  / oil, biooil, woodchips /;
Set fuel_ht(fuel_all) "high-temp fuels" / coke, COgas, hydrogen, natgas, biogas, biochar /;

Set y(t)  "CCS/biocarbon investment periods" / t5, t10, t15, t20, t25, t30 /;
Alias (t,ty);

* NEW: furnace types
Set s_fuel(s) "shaft kiln using fuels + electricity" / s1 /;
Set s_elec(s) "electric-only furnace"              / s2 /;

* NEW: periods before electric furnace is available
Set t_pre(t)  "pre-start periods for electric furnace" / t1*t5 /;

* =========================
* Parameters (data inputs)
* =========================

* Demands
Parameter D_CaO(t) "CaO demand (t)";

Parameter D_CaO(t) /
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

* CO2 from calcination CaCO3 -> CaO + CO2
Scalar EF_calc_CaO "t CO2 per t CaO from calcination" /0.785/;

* Electricity intensities (kWh per t product)
Parameter eta_SAF_CaO(s,t)  "Electricity per t CaO (kWh/t)";

* Total thermal heat required per tonne of product (kWh/t)
Parameter TCF_CaO(s,t)      "Total heat per t CaO (kWh/t)";

* Minimum HT requirements
Parameter A_SAF_HT_req(s)   "min HT requirement parameter for CaO";

* Fuel-to-heat conversion factors (phi = kWh produced per t fuel)
Parameter
    phi_saf_lt(s,t,fuel_lt)  "LT heat from LT fuels in SMA [kWh/t fuel]"
    phi_saf_ht(s,t,fuel_ht)  "HT heat from HT fuels in SMA [kWh/t fuel]";

* Per-fuel minimum share / availability: A * LT/HT <= fuel use
Parameter
    A_saf_lt(s,fuel_lt)  "LT fuel availability for SMA"
    A_saf_ht(s,fuel_ht)  "HT fuel availability for SMA";

* Capacities
Parameter Q_SAF(s,t)  "SMA capacity (t CaO)";

* Costs
scalar p0 /115/;

Parameter
    C_coke(t)       "helper for coke fuel price"
    C_elec(t)       "cost per kWh electricity"
    C_ets(t)        "cost per t CO2"
    FIXEDCOST(t)    "fixed cost in period t"
    C_fuel(fuel_all,t)   "cost per t fuel";

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
C_CCS_var(t)         = 60;
C_CCS_TandS(t)       = 0;
C_CCS_steam(t)       = 0;
C_CCS_capex(t)       = 1000;
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
* Production
    pCaO(s,t)         "CaO production (t)"

* Electricity (kWh)
    eSAF(s,t)         "SMA electricity (kWh)"

* Heat balances (kWh)
    LT_SAF_CaO(s,t)   "LT heat in SMA for CaO (kWh)"
    HT_SAF_CaO(s,t)   "HT heat in SMA for CaO (kWh)"

* Fuel uses (tonnes fuel)
    uSAF(s,t,fuel_all)      "fuel use in SMA for CaO (t fuel)"

* Biocarbon capacity investments
    qBiocarbon(t)     "biocarbon capacity additions in period t (t fuel per period)"
    biocap(t)         "available biocarbon capacity in period t (t fuel per period)"

* CCS variables
    qCCS(t)           "CCS capacity additions in period t (tCO2 per period)"
    fCCS(t)           "CO2 stream sent to CCS (t per period)"
    CO2_calc(t)       "CO2 from CaCO3->CaO+CO2 (t per period)"
    CO2_captured(t)   "CO2 captured (t per period)"
    CO2_offgas_pot(t) "CO2 available to CCS (t per period)"
    CO2_atm_offgas(t) "Residual CO2 to atmosphere after CCS (t per period)"
    eCCS_el(t)        "Electricity use for CCS (kWh)"
    qCCS_th(t)        "Steam/thermal use for CCS (kWh_th)"
    CO2_total_net(t)  "Net CO2 to atmosphere (t)"
    CO2_bio_pre(t)    "Biogenic CO2 that would be emitted without CCS (t)";

Variable
    TotalCost         "objective (€)"
    CO2_ets(t)        "ETS-accounted net CO2 (can be negative)";

* =========================
* Equations
* =========================
Equation
    dem_CaO(t)

    CaO_energy_total(s,t)
    CaO_ht_min(s,t)
    lt_saf_cao_def(s,t)
    lt_saf_cao_min(s,t,fuel_lt)
    ht_saf_cao_def(s,t)
    ht_saf_cao_min(s,t,fuel_ht)

    e_saf_def(s,t)
    capacity_saf(s,t)

    biocap_def(t)
    biocarbon_use_limit(t)

    CO2_calc_def(t)
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

    
* NEW constraints:
    no_fuel_elec(s,t,fuel_all)  "Electric furnace uses no fuels"
    LT_elec_zero(s,t)           "No LT heat balance for electric furnace"
    HT_elec_zero(s,t)           "No HT heat balance for electric furnace"
    furnace2_off(t_pre)         "Electric furnace s2 off in t1–t5"

    obj;

* =========================
* Constraints
* =========================

* Demand: total CaO production must meet demand
dem_CaO(t).. sum(s, pCaO(s,t)) =g= D_CaO(t);

* Energy balance: total LT+HT heat equals total heat requirement per t CaO
* ONLY for fuel-based shaft kiln s1
CaO_energy_total(s,t)$(s_fuel(s))..
    LT_SAF_CaO(s,t) + HT_SAF_CaO(s,t) =e= TCF_CaO(s,t) * pCaO(s,t);

* Minimum HT requirement (only for s1)
CaO_ht_min(s,t)$(s_fuel(s))..
    A_SAF_HT_req(s) * pCaO(s,t) =l= HT_SAF_CaO(s,t);

* LT heat from LT fuels (only for s1)
lt_saf_cao_def(s,t)$(s_fuel(s))..
    LT_SAF_CaO(s,t) =e= sum(fuel_lt, phi_saf_lt(s,t,fuel_lt) * uSAF(s,t,fuel_lt));

lt_saf_cao_min(s,t,fuel_lt)$(s_fuel(s))..
    A_saf_lt(s,fuel_lt) * LT_SAF_CaO(s,t) =l= uSAF(s,t,fuel_lt);

* HT heat from HT fuels (only for s1)
ht_saf_cao_def(s,t)$(s_fuel(s))..
    HT_SAF_CaO(s,t) =e= sum(fuel_ht, phi_saf_ht(s,t,fuel_ht) * uSAF(s,t,fuel_ht));

ht_saf_cao_min(s,t,fuel_ht)$(s_fuel(s))..
    A_saf_ht(s,fuel_ht) * HT_SAF_CaO(s,t) =l= uSAF(s,t,fuel_ht);

* NEW: Electric furnace s2 – no fuel use at all
no_fuel_elec(s,t,fuel_all)$(s_elec(s))..
    uSAF(s,t,fuel_all) =e= 0;

* NEW: Electric furnace s2 – no LT/HT heat variables used
LT_elec_zero(s,t)$(s_elec(s))..
    LT_SAF_CaO(s,t) =e= 0;

HT_elec_zero(s,t)$(s_elec(s))..
    HT_SAF_CaO(s,t) =e= 0;

* Route-specific electricity: both furnaces use electricity
e_saf_def(s,t).. eSAF(s,t) =e= eta_SAF_CaO(s,t) * pCaO(s,t);

* Capacity constraint for SMA units
capacity_saf(s,t)..  pCaO(s,t) =l= Q_SAF(s,t);

* NEW: Electric furnace only operational from t6 onwards
furnace2_off(t_pre)..
    pCaO('s2',t_pre) =e= 0;

* Biocarbon capacity accumulation and usage limit
biocap_def(t)..
    biocap(t) =e=
       sum(ty$( y(ty) and ord(t) >= ord(ty) and (ord(t) - ord(ty)) < L_Biocarbon ),
           qBiocarbon(ty));

biocarbon_use_limit(t)..
    sum(s, uSAF(s,t,'biochar')) =l= biocap(t);

* =========================
* CO2 from calcination & CCS equations
* =========================

* CO2 from CaCO3 -> CaO + CO2
CO2_calc_def(t)..
    CO2_calc(t) =e= EF_calc_CaO * sum(s, pCaO(s,t));

* All capture-eligible CO2 is calcination CO2
co2_offgas_pot_def(t)..
    CO2_offgas_pot(t) =e= CO2_calc(t);

* CCS stream bounded by available CO2
ccs_stream_upper(t)..
    fCCS(t) =l= CO2_offgas_pot(t);

* Lifetime-limited CCS capacity
ccs_capacity_upper(t)..
    fCCS(t) =l=
       sum(ty$( y(ty) and ord(t) >= ord(ty) and (ord(t) - ord(ty)) < L_CCS ),
           qCCS(ty));

* Capture efficiency
ccs_capture_eff(t)..
    CO2_captured(t) =l= omega_ccs * fCCS(t);

* Residual CO2 to atmosphere
co2_atm_offgas_def(t)..
    CO2_atm_offgas(t) =e= CO2_offgas_pot(t) - CO2_captured(t);

* Net CO2 = residual calcination CO2
co2_total_net_def(t)..
    CO2_total_net(t) =e= CO2_atm_offgas(t);

* CCS energy use
ccs_el_def(t)..  eCCS_el(t) =e= SEC_CCS_el(t) * CO2_captured(t);
ccs_th_def(t)..  qCCS_th(t) =e= SEC_CCS_th(t) * CO2_captured(t);

* Investments only allowed in specific years y
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
*   allowing negative emissions when CCS > fossil emissions.
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
        )
      + sum(t$y(t), C_CCS_capex(t)        * qCCS(t))
      + sum(t$y(t), C_Biocarbon_capex(t)  * qBiocarbon(t));

* =========================
* Model & Test data
* =========================
Model SMA_CaCO3_Opt / all /;

* --- Test data (example numbers) ---

* Heat & electricity intensities
eta_SAF_CaO('s1',t) = 60;
eta_SAF_CaO('s2',t) = 1300;

* kWh/t CaO total thermal heat (only used for s1 via s_fuel)
TCF_CaO(s,t)    = 1200;

* can set to 1 if you want only HT
A_SAF_HT_req(s) = 0.2;

A_saf_lt(s,fuel_lt) = 0;
A_saf_ht(s,fuel_ht) = 0;

* capacity
Q_SAF(s,t) = 120000;

C_coke(t)       = 300;
C_elec(t)       = 0.010;
C_ets(t)        = p0 + 17*(ord(t)-1);
FIXEDCOST(t)    = 000;

C_fuel(fuel_all,t) = 0;
C_fuel('coke',t)      = C_coke(t);
C_fuel('oil',t)       = 500;
C_fuel('biooil',t)    = 450;
C_fuel('woodchips',t) = 1200;
C_fuel('COgas',t)     = 50;
C_fuel('hydrogen',t)  = 2000;
C_fuel('natgas',t)    = 1500;
C_fuel('biogas',t)    = 1300;
C_fuel('biochar',t)   = 4000;

phi_saf_lt(s,t,fuel_lt) = 0;
phi_saf_ht(s,t,fuel_ht) = 0;

Scalar
    LHV_coke       /  8000 /
    LHV_biochar    /  7000 /
    LHV_oil        /  1160 /
    LHV_biooil     /  5000 /
    LHV_woodchips  /  2500 /
    LHV_natgas     /  1390 /
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
Solve SMA_CaCO3_Opt using LP minimizing TotalCost;

* ===== Per-period cost breakdown =====
Set costcat / fixed, elec, fuel_SAF,
              ccs_var_ts, ccs_steam, ets,
              capex_ccs, capex_biocarb, total /;

Parameter
    CostByT(t,costcat) "Cost breakdown by period (EUR)"
    TotalCost_t(t)     "Total cost per period (EUR)"
    TotalCost_check    "Sum over periods (EUR)";

CostByT(t,'fixed')     = FIXEDCOST(t);

CostByT(t,'elec') = C_elec(t) * ( sum(s, eSAF.l(s,t)) + eCCS_el.l(t) );

CostByT(t,'fuel_SAF')  = sum(s, sum(fuel_all, C_fuel(fuel_all,t) * uSAF.l(s,t,fuel_all)));

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
                    + CostByT(t,'ccs_var_ts')
                    + CostByT(t,'ccs_steam')
                    + CostByT(t,'ets')
                    + CostByT(t,'capex_ccs')
                    + CostByT(t,'capex_biocarb');

TotalCost_t(t)  = CostByT(t,'total');
TotalCost_check = sum(t, TotalCost_t(t));

* >>> NEW: Aggregate fuel use per period and fuel
Parameter FuelUse_t_f(t,fuel_all) "Fuel use per period and fuel (t)";
FuelUse_t_f(t,fuel_all) = sum(s, uSAF.l(s,t,fuel_all));

* ---- CO2 & ETS summary ----
Parameter CO2_total(t) "Total CO2 per period (t, net)";
Scalar   CO2_total_all "Total CO2 over all periods (t, net)";

CO2_total(t)    = CO2_total_net.l(t);
CO2_total_all   = sum(t, CO2_total(t));

Parameter
    ProdTot(t)       "Total CaO (t)"
    ETS_cost(t)      "ETS cost per period (eur)"
    CO2_intensity(t) "CO2 intensity vs total product (t CO2/t product)";

ProdTot(t)  = sum(s, pCaO.l(s,t));
ETS_cost(t) = C_ets(t) * CO2_ets.l(t);

Scalar ep /1e-9/;
CO2_intensity(t) = CO2_total(t) / max(ProdTot(t), ep);

* ---- Write to GDX ----
execute_unload 'output_SMA.gdx'
    CO2_calc.l, CO2_offgas_pot.l, CO2_atm_offgas.l,
    pCaO.l, eSAF.l,
    CO2_captured.l, CO2_total_net.l, qCCS.l, eCCS_el.l, qCCS_th.l,
    qBiocarbon.l, biocap.l,
    CostByT, TotalCost_t, TotalCost_check, costcat,
    FuelUse_t_f,
    ETS_cost;

option decimals=6;

Display CO2_total, CO2_total_all;
Display ProdTot, ETS_cost, CO2_intensity;
Display FuelUse_t_f;

