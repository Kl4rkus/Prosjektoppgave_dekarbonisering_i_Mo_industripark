$Title 7-Steel Mo i Rana with CCS (u_fuel in tonnes, phi in kWh/t)

* =========================
* Sets and indices
* =========================
Set t          "time periods" / t1*t30 /;
Set s          "EAF units"    / e1, e2 /;

Set fuel_all   "rolling mill fuels and potential alternatives"
/ COgas, natgas, biogas /;

Set fuel_bio(fuel_all) "biogenic fuels"
/ biogas /;

Set y(t)  "CCS investment periods" / t5, t10, t15, t20, t25, t30 /;
Alias (t,ty);

* =========================
* Parameters (data inputs)
* =========================

* Steel demand (t)
Parameter D_steel(t) "Steel demand (t)";

Parameter D_steel(t) /
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

* CO2 from scrap-related emissions (t CO2 per t steel)
Scalar EF_scrap "t CO2 per t steel from scrap input" /0.4/;

* EAF electricity intensities (kWh per t steel)
Parameter eta_EAF(s,t) "Electricity per t steel in EAF (kWh/t)";

* Rolling mill: total thermal heat per tonne of steel (kWh/t)
Parameter TCF_roll(t) "Rolling mill heat per t steel (kWh/t)";

* Fuel-to-heat conversion factors (kWh produced per t fuel) for rolling mill
Parameter phi_roll(t,fuel_all) "Rolling mill heat from fuels [kWh/t fuel]";

* Capacities
Parameter Q_EAF(s,t)  "EAF capacity (t steel)";

* Costs
scalar p0 /115/;

Parameter
    C_elec(t)       "cost per kWh electricity"
    C_ets(t)        "cost per t CO2"
    FIXEDCOST(t)    "fixed cost in period t"
    C_fuel(fuel_all,t)   "cost per t fuel";

* =========================
* CCS PARAMETERS
* =========================
Scalar
    omega_ccs   "capture efficiency (0..1)" /0.9/
    L_CCS       "CCS lifetime (number of periods)";

Parameter
    SEC_CCS_el(t)      "electricity for CCS (kWh per tCO2 captured)"
    SEC_CCS_th(t)      "steam/heat for CCS (kWh_th per tCO2 captured)"
    C_CCS_var(t)       "CCS variable O&M (€/tCO2 captured)"
    C_CCS_TandS(t)     "Transport & storage (€/tCO2 captured)"
    C_CCS_steam(t)     "Steam price for CCS (€/kWh_th)"
    C_CCS_capex(t)     "Capex cost for CCS in investment periods y (€/ (tCO2 per period))";

* Initialize so model runs if no data provided
SEC_CCS_el(t)        = 0;
SEC_CCS_th(t)        = 0;
C_CCS_var(t)         = 60;
C_CCS_TandS(t)       = 0;
C_CCS_steam(t)       = 0;
C_CCS_capex(t)       = 1000;

L_CCS       = 20;

* Biogenic CO2 emission factors (t CO2 per t fuel; non-bio = 0)
Parameter EF_bio(fuel_all) "t CO2 per t fuel (nonzero only for biogenic)";
EF_bio(fuel_all) = 0;
EF_bio('biogas') = 2.0;

* Fossil CO2 emission factors (t CO2 per t fuel)
Parameter EF_fuel_fossil(fuel_all) "Fossil CO2 per t fuel";
EF_fuel_fossil('COgas')  = 2.0;
EF_fuel_fossil('natgas') = 2.5;
* treated as biogenic only
EF_fuel_fossil('biogas') = 0;

Parameter biogas_availability_limit(t) "Max availability of biogas in time period t, in tonnes";

* =========================
* Decision variables
* =========================
Positive Variable
* Production
    pSteel(s,t)       "Steel production in EAF (t)"

* Electricity (kWh)
    eEAF(s,t)         "EAF electricity (kWh)"
* can be left free; cost drives it to 0
    eRoll(t)          "Rolling mill electricity (kWh)"   

* Rolling mill heat (kWh)
    HT_roll(t)        "Rolling mill heat (kWh)"

* Rolling mill fuel uses (tonnes fuel)
    uRoll(t,fuel_all) "Fuel use in rolling mill (t fuel)"

* CCS variables
    qCCS(t)           "CCS capacity additions in period t (tCO2 per period)"
    fCCS(t)           "CO2 stream sent to CCS (t per period)"
    CO2_scrap(t)      "CO2 from scrap-related emissions (t per period)"
    CO2_combust(t)    "CO2 from rolling mill fuel combustion (t per period)"
    CO2_offgas_pot(t) "CO2 available to CCS (t per period)"
    CO2_captured(t)   "CO2 captured (t per period)"
    CO2_atm(t)        "Residual CO2 to atmosphere after CCS (t per period)"
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
    dem_steel(t)

    eaf_el_def(s,t)
    roll_heat_def(t)
    roll_fuel_def(t)
    capacity_eaf(s,t)

    CO2_scrap_def(t)
    CO2_combust_def(t)
    co2_offgas_pot_def(t)
    ccs_stream_upper(t)
    ccs_capacity_upper(t)
    ccs_capture_eff(t)
    co2_atm_def(t)
    co2_total_net_def(t)
    ccs_el_def(t)
    ccs_th_def(t)
    invest_only_y(t)

    co2_bio_pre_def(t)
    co2_ets_def(t)
    
    biogas_availability(t)

    obj;

* =========================
* Constraints
* =========================

* Demand: total steel production must meet demand
dem_steel(t).. sum(s, pSteel(s,t)) =g= D_steel(t);

* EAF electricity use
eaf_el_def(s,t)..
    eEAF(s,t) =e= eta_EAF(s,t) * pSteel(s,t);

* Rolling mill heat requirement (assume all steel is rolled)
roll_heat_def(t)..
    HT_roll(t) =e= TCF_roll(t) * sum(s, pSteel(s,t));

* Rolling mill heat from fuels
roll_fuel_def(t)..
    HT_roll(t) =e= sum(fuel_all, phi_roll(t,fuel_all) * uRoll(t,fuel_all));

* Capacity constraint for EAF units
capacity_eaf(s,t)..  pSteel(s,t) =l= Q_EAF(s,t);

* =========================
* CO2 & CCS equations
* =========================

* Scrap-related CO2 (per tonne steel)
CO2_scrap_def(t)..
    CO2_scrap(t) =e= EF_scrap * sum(s, pSteel(s,t));

* Combustion CO2 from rolling mill fuels
CO2_combust_def(t)..
    CO2_combust(t) =e=
        sum(fuel_all, EF_fuel_fossil(fuel_all) * uRoll(t,fuel_all))
      + sum(fuel_bio, EF_bio(fuel_bio) * uRoll(t,fuel_bio));

* Total CO2 available to CCS
co2_offgas_pot_def(t)..
    CO2_offgas_pot(t) =e= CO2_scrap(t) + CO2_combust(t);

* Stream to CCS cannot exceed available CO2
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
co2_atm_def(t)..
    CO2_atm(t) =e= CO2_offgas_pot(t) - CO2_captured(t);

* Net CO2 = residual after CCS
co2_total_net_def(t)..
    CO2_total_net(t) =e= CO2_atm(t);

* CCS energy use
ccs_el_def(t)..  eCCS_el(t) =e= SEC_CCS_el(t) * CO2_captured(t);
ccs_th_def(t)..  qCCS_th(t) =e= SEC_CCS_th(t) * CO2_captured(t);

* Investments only allowed in specific years y
invest_only_y(t)$(not y(t))..
    qCCS(t) =e= 0;

* =========================
* Biogenic CO2 & ETS accounting
* =========================

* Total biogenic CO2 that would be emitted if there were no CCS
co2_bio_pre_def(t)..
    CO2_bio_pre(t) =e=
        sum(fuel_bio, EF_bio(fuel_bio) * uRoll(t,fuel_bio));

* ETS-accounted emissions:
*   physical net emissions (CO2_total_net)
*   minus all biogenic CO2 (treated as climate-neutral),
*   allowing negative emissions when CCS > fossil emissions.
co2_ets_def(t)..
    CO2_ets(t) =e= CO2_total_net(t) - CO2_bio_pre(t);
    
biogas_availability(t)..  uRoll(t,'biogas') =l= biogas_availability_limit(t);
    

* =========================
* Objective function
* =========================
obj..
    TotalCost =e=
      sum(t,
           FIXEDCOST(t)
         + C_elec(t)       * ( sum(s, eEAF(s,t)) + eRoll(t) + eCCS_el(t) )
         + C_ets(t)        * CO2_ets(t)
         + sum(fuel_all, C_fuel(fuel_all,t) * uRoll(t,fuel_all))
         + ( C_CCS_var(t) + C_CCS_TandS(t) ) * CO2_captured(t)
         + C_CCS_steam(t)  * qCCS_th(t)
        )
      + sum(t$y(t), C_CCS_capex(t) * qCCS(t));

* =========================
* Model & Test data
* =========================
Model Steel7_Opt / all /;

* --- Test data (example numbers, adjust as needed) ---

* EAF electricity intensity (kWh/t steel)
* typical EAF order of magnitude
eta_EAF(s,t) = 450;       

* Rolling mill heat intensity (kWh/t steel)
* example value
TCF_roll(t)  = 600;

biogas_availability_limit(t) = 10000;

* EAF capacity
Q_EAF(s,t) = 120000;     

* Costs
C_elec(t)    = 0.010;
C_ets(t)        = p0 + 17*(ord(t)-1);
FIXEDCOST(t) = 10000;

C_fuel(fuel_all,t) = 0;
C_fuel('COgas',t)  = 500;
C_fuel('natgas',t) = 150;
C_fuel('biogas',t) = 130;

* Rolling mill fuel -> heat (kWh/t fuel)
Scalar
    LHV_COgas    / 2400 /
    LHV_natgas   / 13900 /
    LHV_biogas   / 6000 /;

phi_roll(t,'COgas')  = LHV_COgas;
phi_roll(t,'natgas') = LHV_natgas;
phi_roll(t,'biogas') = LHV_biogas;

* =========================
* Solve
* =========================
Solve Steel7_Opt using LP minimizing TotalCost;

* ===== Per-period cost breakdown =====
Set costcat / fixed, elec, fuel_roll,
              ccs_var_ts, ccs_steam, ets,
              capex_ccs, total /;

Parameter
    CostByT(t,costcat) "Cost breakdown by period (EUR)"
    TotalCost_t(t)     "Total cost per period (EUR)"
    TotalCost_check    "Sum over periods (EUR)";

CostByT(t,'fixed')     = FIXEDCOST(t);

CostByT(t,'elec') = C_elec(t) * ( sum(s, eEAF.l(s,t)) + eRoll.l(t) + eCCS_el.l(t) );

CostByT(t,'fuel_roll')  = sum(fuel_all, C_fuel(fuel_all,t) * uRoll.l(t,fuel_all));

CostByT(t,'ccs_var_ts') = ( C_CCS_var(t) + C_CCS_TandS(t) ) * CO2_captured.l(t);
CostByT(t,'ccs_steam')  = C_CCS_steam(t) * qCCS_th.l(t);

CostByT(t,'ets') = C_ets(t) * CO2_ets.l(t);

CostByT(t,'capex_ccs')     = 0;
CostByT(t,'capex_ccs')$y(t) = C_CCS_capex(t) * qCCS.l(t);

CostByT(t,'total') =  CostByT(t,'fixed')
                    + CostByT(t,'elec')
                    + CostByT(t,'fuel_roll')
                    + CostByT(t,'ccs_var_ts')
                    + CostByT(t,'ccs_steam')
                    + CostByT(t,'ets')
                    + CostByT(t,'capex_ccs');

TotalCost_t(t)  = CostByT(t,'total');
TotalCost_check = sum(t, TotalCost_t(t));

* Fuel use per period & fuel (t)
Parameter FuelUse_t_f(t,fuel_all) "Fuel use per period and fuel (t)";
FuelUse_t_f(t,fuel_all) = uRoll.l(t,fuel_all);



option decimals=6;

Parameter CO2_total(t) "Total CO2 per period (t, net)";
CO2_total(t) = CO2_total_net.l(t);
Scalar CO2_total_all "Total CO2 over all periods (t, net)";
CO2_total_all = sum(t, CO2_total(t));
Display CO2_total, CO2_total_all;

Parameter
    ProdTot(t)         "Total steel (t)"
    ETS_cost(t)        "ETS cost per period (eur)"
    CO2_intensity(t)   "CO2 intensity vs total product (t CO2/t steel)";

ProdTot(t)      = sum(s, pSteel.l(s,t));
ETS_cost(t)     = C_ets(t) * CO2_ets.l(t);

Scalar ep /1e-9/;
CO2_intensity(t) = CO2_total(t) / max(ProdTot(t), ep);

execute_unload 'output_7Steel.gdx'
    CO2_scrap.l, CO2_combust.l, CO2_offgas_pot.l, CO2_atm.l,
    pSteel.l, eEAF.l,
    uRoll.l,
    CO2_captured.l, CO2_total_net.l, qCCS.l, eCCS_el.l, qCCS_th.l,
    CostByT, TotalCost_t, TotalCost_check, ETS_cost,
    FuelUse_t_f;

Display ProdTot, ETS_cost, CO2_intensity;
Display FuelUse_t_f;
