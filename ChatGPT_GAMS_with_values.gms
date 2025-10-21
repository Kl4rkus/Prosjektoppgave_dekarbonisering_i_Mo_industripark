* ============================================================
* Ferroglobe FeMn / SiMn Planning Model (GAMS translation)
* Mirrors "Optimization model (Zack's equations)"
* ============================================================

*-----------------------------
* SETS
*-----------------------------
SETS
    t              "time periods"                    /1/ 
    saf            "SAF index"                       / s1, s2 /
    lhFuel         "low-heat fuels (LT)"             / oil, biooil, woodchips /
    hhFuel         "high-heat fuels (HT)"            / coke, COgas, hydrogen, natgas, biogas, biochar /
    fSiMn          "fuel techs for SiMn LT/HT terms" / f1 /   
;

ALIAS (t,tt);

*-----------------------------
* PARAMETERS (DATA INPUTS)
*-----------------------------
* Demands
PARAMETERS
    DFeMn(t)               "demand FeMn at time t" /1 58463.46 /
    DSiMn(t)               "demand SiMn at time t" /1 58463.46 /
;

* Yields and coefficients
SCALARS
    Y_ore_sint             "Y_{ore->sinter}^{sint}" /1/
    Y_sin_FeMn             "Y_{sinter->FeMn}^{SAF}" /2/
    Y_sin_slag             "Y_{sinter->slag}^{SAF}" /4/
    eta_sint               "electricity per t sinter (Kw/h)" /90/
;

PARAMETERS
    eta1_SAF            "kWh/t FeMn in SAF1" /2850/
    eta2_SAF            "kWh/t FeMn in SAF2" /2850/
    nu1_SAF             "kWh/t SiMn in SAF1" /4000/
    nu2_SAF             "kWh/t SiMn in SAF2" /4000/
    Y1_offgas(f)           "off-gas t per t FeMn in SAF1" /coke 1.8, COgas 1.8, hydrogen 1.8, natgas 1.8, biogas 1.8, biochar 1.8, oil  1.8, biooil 1.8, woodchips 1.8 /
    Y2_offgas(f)           "off-gas t per t FeMn in SAF2" /coke 1.8, COgas 1.8, hydrogen 1.8, natgas 1.8, biogas 1.8, biochar 1.8, oil  1.8, biooil 1.8, woodchips 1.8 /
    Z1_offgas_SiMn      "off-gas per t SiMn SAF1" /coke 1.8, COgas 1.8, hydrogen 1.8, natgas 1.8, biogas 1.8, biochar 1.8, oil  1.8, biooil 1.8, woodchips 1.8 /
    Z2_offgas_SiMn      "off-gas per t SiMn SAF2" /coke 1.8, COgas 1.8, hydrogen 1.8, natgas 1.8, biogas 1.8, biochar 1.8, oil  1.8, biooil 1.8, woodchips 1.8 /
    D_offgas            "off-gas demand" /0/
;

* Emission factors
SCALARS
    EF_coke_sint           "tCO2 per t coke (sinter)" /1.832/
    EF_flare               "tCO2 per t off-gas flared" /1.34/
;

* Heat proportion coefficients (K) and minimum HT factors (A)
PARAMETERS
    KLT_sint            "K_{t,LT}^{sint}" /1/
    KHT_sint            "K_{t,HT}^{sint}" /1/
    AHT_sint               "A_{HT}^{sint}" /0/

    KLT_SAF1            "K_{1,t,LT}^{SAF}" /1/
    KHT_SAF1            "K_{1,t,HT}^{SAF}" /1/
    AHT_SAF1               "A_{1,HT}^{SAF}" /0.3/

    KLT_SAF2            "K_{2,t,LT}^{SAF}" /1/
    KHT_SAF2            "K_{2,t,HT}^{SAF}" /1/
    AHT_SAF2               "A_{2,HT}^{SAF}" /0.3/

    TCF_SiMn1           "total LT+HT per t SiMn in SAF1" /2700/
    TCF_SiMn2           "total LT+HT per t SiMn in SAF2" /2700/
    TCF_FeMn1          "total LT+HT per t FeMn in SAF1" /2700/
    TCF_FeMn2          "total LT+HT per t FeMn in SAF2" /2700/ 
    AHT_SAF1_SiMn          "A_{1,HT}^{SAF,SiMn}" /0.3/
    AHT_SAF2_SiMn          "A_{2,HT}^{SAF,SiMn}"/0.3/
;

* Fuel-to-heat conversion factors phi (kWh heat per unit fuel)
PARAMETERS
    phi_sint_lh(lhFuel)  "LT sinter"
    phi_SAF1_lh(lhFuel)  "LT SAF1"
    phi_SAF2_lh(lhFuel)  "LT SAF2"

    phi_sint_hh(t,hhFuel)  "HT sinter"
    phi_SAF1_hh(t,hhFuel)  "HT SAF1"
    phi_SAF2_hh(t,hhFuel)  "HT SAF2"

    phi_SAF1_SiMn(t,fSiMn) "for LT/HT SiMn SAF1 (aggregate placeholder)"
    phi_SAF2_SiMn(t,fSiMn) "for LT/HT SiMn SAF2 (aggregate placeholder)"
;

* Minimum fuel availability/style constraints (A_* terms)
PARAMETERS
    A_sint_lh(lhFuel)      "A_{oil,biooil,woodchips}^{sint}"
    A_SAF1_lh(lhFuel)      "A_{1,oil,biooil,woodchips}^{SAF}"
    A_SAF2_lh(lhFuel)      "A_{2,oil,biooil,woodchips}^{SAF}"

    A_sint_hh(hhFuel)      "A_{coke,COgas,hydrogen,natgas,biogas,biochar}^{sint}"
    A_SAF1_hh(hhFuel)      "A_{1,*}^{SAF}"
    A_SAF2_hh(hhFuel)      "A_{2,*}^{SAF}"
;

* Coke requirement coefficients (if you activate those eqs)
SCALARS
    R_coke_sinter          "t coke per t sinter"
    R_coke_SAF             "t coke per t FeMn"
;

* Coke fraction or recipe terms X’s for reductants (placeholders)
SCALARS
    X1_coke_SAF, X1_biochar_SAF, X1_woodchips_SAF
    X2_coke_SAF, X2_biochar_SAF, X2_woodchips_SAF
;

* Costs and ETS
PARAMETERS
    C_ore(t)               "€/t ore"
    C_buysinter(t)         "€/t sinter (buy)"
    C_coke(t)              "€/t coke"
    C_elec(t)              "€/kWh electricity"
    C_ets(t)               "€/t CO2"
    FIXEDCOST(t)           "fixed cost €/period"
;

* Capacities
PARAMETERS
    Q_SAF(t)               "max t product in SAF per period (per furnace)"
    Q_sint(t)              "max t sinter per period"
;

*-----------------------------
* VARIABLES
*-----------------------------
POSITIVE VARIABLES
* Production
    pFeMn1(t)              "p_{1,t}^{FeMn}"
    pFeMn2(t)              "p_{2,t}^{FeMn}"
    pSiMn1(t)              "p_{1,t}^{SiMn}"
    pSiMn2(t)              "p_{2,t}^{SiMn}"
    pSinter(t)             "p_t^{sinter}"
    bSinter(t)             "bought sinter"

* Sinter usage to SAFs
    uSinter1(t)            "u_{1,t}^{sinter} for FeMn"
    uSinter2(t)            "u_{2,t}^{sinter} for FeMn"
    uSinter1_SiMn(t)       "u_{1,t}^{sinter,SiMn}"
    uSinter2_SiMn(t)       "u_{2,t}^{sinter,SiMn}"

* Ore usage
    uOre(t)                "u_t^{ore}"

* Coke usage (aggregate & per SAF if used)
    uCoke_sint(t)          "coke used in sinter"
    uCoke1_SAF(t)          "u_{1,t,coke}^{SAF}"
    uCoke2_SAF(t)          "u_{2,t,coke}^{SAF}"
    uCoke_total_SAF(t)     "u_{total,t,coke}^{SAF}"

* Reductant mixes (as in X* equations; optional)
    uBiochar1_SAF(t), uWood1_SAF(t)
    uBiochar2_SAF(t), uWood2_SAF(t)

* Slag
    pSlag1(t)              "p_{1,t}^{slag}"
    pSlag2(t)              "p_{2,t}^{slag}"
    pSlagTot(t)            "p_{tot,t}^{slag}"

* Electricity
    eSAF1(t)               "e_{1,t}^{SAF}"
    eSAF2(t)               "e_{2,t}^{SAF}"
    eSinter(t)             "e_t^{sint}"

* Low & High heat blocks
    LT_sint(t), HT_sint(t)
    LT_SAF1(t), HT_SAF1(t)
    LT_SAF2(t), HT_SAF2(t)
    LT_SiMn1(t), HT_SiMn1(t)
    LT_SiMn2(t), HT_SiMn2(t)

* Low-heat fuel uses
    u_sint_lh(t,lhFuel)
    u_SAF1_lh(t,lhFuel)
    u_SAF2_lh(t,lhFuel)

* High-heat fuel uses
    u_sint_hh(t,hhFuel)
    u_SAF1_hh(t,hhFuel)
    u_SAF2_hh(t,hhFuel)

* Off-gas & CO2
    gOffgas(t)             "g_t^{off-gas}"
    CO2_coke_sint(t)       "CO2_{t,coke}^{sint}"
    CO2_offgas(t)          "CO2^{off-gas}"

* Objective accumulator
    TAC                    "total cost (objective)"
;

FREE VARIABLE Z;

*-----------------------------
* EQUATIONS
*-----------------------------
EQUATIONS
    Demand_FeMn
    Demand_SiMn

    Sinter_mass
    Sinter_heat_balance
    Sinter_HT_min
    Sinter_elec
    Sinter_use_balance

    SAF1_sinter_link
    SAF1_heat_balance
    SAF1_HT_min
    SAF1_reductant_recipe   * optional, uses X’s

    SAF2_sinter_link
    SAF2_heat_balance
    SAF2_HT_min
    SAF2_reductant_recipe   * optional

    Slag1_def
    Slag2_def
    SlagTot_def

    SiMn_sinter_balance
    SiMn1_LTHT_total
    SiMn2_LTHT_total
    SiMn1_HT_min
    SiMn2_HT_min

    SAF1_elec
    SAF2_elec

    Offgas_balance
    CO2_coke_sint_def
    CO2_offgas_def

    LT_sint_fuels
    LT_sint_A_oil
    LT_sint_A_biooil
    LT_sint_A_wood

    LT_SAF1_fuels
    LT_SAF1_A_oil
    LT_SAF1_A_biooil
    LT_SAF1_A_wood

    LT_SAF2_fuels
    LT_SAF2_A_oil
    LT_SAF2_A_biooil
    LT_SAF2_A_wood

    HT_sint_fuels
    HT_sint_A_all(hhFuel)

    HT_SAF1_fuels
    HT_SAF1_A_all(hhFuel)

    HT_SAF2_fuels
    HT_SAF2_A_all(hhFuel)

    Capacity_SAF1
    Capacity_SAF2
    Capacity_sinter

    Objective_def
;

*-----------------------------
* DEMANDS
*-----------------------------
Demand_FeMn(t)..   pFeMn1(t) + pFeMn2(t) =G= DFeMn(t);
Demand_SiMn(t)..   pSiMn1(t) + pSiMn2(t) =G= DSiMn(t);

*-----------------------------
* SINTER: mass & energy
*-----------------------------
Sinter_mass(t)..           uOre(t) =E= Y_ore_sint * pSinter(t);

Sinter_heat_balance(t)..
    KLT_sint(t)*LT_sint(t) + KHT_sint(t)*HT_sint(t) =E= pSinter(t);

Sinter_HT_min(t)..         AHT_sint * pSinter(t) =L= HT_sint(t);

Sinter_elec(t)..           eSinter(t) =E= eta_sint * pSinter(t);

Sinter_use_balance(t)..
    uSinter1(t) + uSinter2(t) + uSinter1_SiMn(t) + uSinter2_SiMn(t)
    =E= pSinter(t) + bSinter(t);

*-----------------------------
* SAF 1: FeMn
*-----------------------------
SAF1_sinter_link(t)..      uSinter1(t) =E= Y_sin_FeMn * pFeMn1(t);

SAF1_heat_balance(t)..
    KLT_SAF1(t)*LT_SAF1(t) + KHT_SAF1(t)*HT_SAF1(t) =E= pFeMn1(t);

SAF1_HT_min(t)..           AHT_SAF1 * pFeMn1(t) =L= HT_SAF1(t);

* Optional reductant split (placeholder)
SAF1_reductant_recipe(t)..
    X1_coke_SAF*uCoke1_SAF(t) + X1_biochar_SAF*uBiochar1_SAF(t) + X1_woodchips_SAF*uWood1_SAF(t)
    =E= pFeMn1(t);

*-----------------------------
* SAF 2: FeMn
*-----------------------------
SAF2_sinter_link(t)..      uSinter2(t) =E= Y_sin_FeMn * pFeMn2(t);

SAF2_heat_balance(t)..
    KLT_SAF2(t)*LT_SAF2(t) + KHT_SAF2(t)*HT_SAF2(t) =E= pFeMn2(t);

SAF2_HT_min(t)..           AHT_SAF2 * pFeMn2(t) =L= HT_SAF2(t);

SAF2_reductant_recipe(t)..
    X2_coke_SAF*uCoke2_SAF(t) + X2_biochar_SAF*uBiochar2_SAF(t) + X2_woodchips_SAF*uWood2_SAF(t)
    =E= pFeMn2(t);

*-----------------------------
* SLAG
*-----------------------------
Slag1_def(t)..             pSlag1(t)   =E= Y_sin_slag * uSinter1(t);
Slag2_def(t)..             pSlag2(t)   =E= Y_sin_slag * uSinter2(t);
SlagTot_def(t)..           pSlagTot(t) =E= pSlag1(t) + pSlag2(t);

*-----------------------------
* SiMn balance & heat
*-----------------------------
SiMn_sinter_balance(t)..
    uSinter1_SiMn(t) + uSinter2_SiMn(t) + pSlagTot(t) =E= pSiMn1(t) + pSiMn2(t);

SiMn1_LTHT_total(t)..      LT_SiMn1(t) + HT_SiMn1(t) =E= TCF_SiMn1(t)*pSiMn1(t);
SiMn2_LTHT_total(t)..      LT_SiMn2(t) + HT_SiMn2(t) =E= TCF_SiMn2(t)*pSiMn2(t);

SiMn1_HT_min(t)..          AHT_SAF1_SiMn * pSiMn1(t) =L= HT_SiMn1(t);
SiMn2_HT_min(t)..          AHT_SAF2_SiMn * pSiMn2(t) =L= HT_SiMn2(t);

*-----------------------------
* Electricity for SAFs
*-----------------------------
SAF1_elec(t)..             eSAF1(t) =E= eta1_SAF(t)*pFeMn1(t) + nu1_SAF(t)*pSiMn1(t);
SAF2_elec(t)..             eSAF2(t) =E= eta2_SAF(t)*pFeMn2(t) + nu2_SAF(t)*pSiMn2(t);

*-----------------------------
* Off-gas & CO2
*-----------------------------
Offgas_balance(t)..
    gOffgas(t)
    =E= Y1_offgas(t)*pFeMn1(t) + Y2_offgas(t)*pFeMn2(t)
       + Z1_offgas_SiMn(t)*pSiMn1(t) + Z2_offgas_SiMn(t)*pSiMn2(t);

CO2_coke_sint_def(t)..     CO2_coke_sint(t) =E= EF_coke_sint * pSinter(t);

CO2_offgas_def(t)..        CO2_offgas(t) =E= (gOffgas(t) - D_offgas(t)) * EF_flare;

*-----------------------------
* Fuel-to-heat links (LT blocks)
*-----------------------------
LT_sint_fuels(t)..         LT_sint(t)  =E= SUM(lhFuel, phi_sint_lh(t,lhFuel)*u_sint_lh(t,lhFuel));
LT_sint_A_oil(t)..         A_sint_lh("oil")      * LT_sint(t) =L= u_sint_lh(t,"oil");
LT_sint_A_biooil(t)..      A_sint_lh("biooil")   * LT_sint(t) =L= u_sint_lh(t,"biooil");
LT_sint_A_wood(t)..        A_sint_lh("woodchips")* LT_sint(t) =L= u_sint_lh(t,"woodchips");

LT_SAF1_fuels(t)..         LT_SAF1(t)  =E= SUM(lhFuel, phi_SAF1_lh(t,lhFuel)*u_SAF1_lh(t,lhFuel));
LT_SAF1_A_oil(t)..         A_SAF1_lh("oil")      * LT_SAF1(t) =L= u_SAF1_lh(t,"oil");
LT_SAF1_A_biooil(t)..      A_SAF1_lh("biooil")   * LT_SAF1(t) =L= u_SAF1_lh(t,"biooil");
LT_SAF1_A_wood(t)..        A_SAF1_lh("woodchips")* LT_SAF1(t) =L= u_SAF1_lh(t,"woodchips");

LT_SAF2_fuels(t)..         LT_SAF2(t)  =E= SUM(lhFuel, phi_SAF2_lh(t,lhFuel)*u_SAF2_lh(t,lhFuel));
LT_SAF2_A_oil(t)..         A_SAF2_lh("oil")      * LT_SAF2(t) =L= u_SAF2_lh(t,"oil");
LT_SAF2_A_biooil(t)..      A_SAF2_lh("biooil")   * LT_SAF2(t) =L= u_SAF2_lh(t,"biooil");
LT_SAF2_A_wood(t)..        A_SAF2_lh("woodchips")* LT_SAF2(t) =L= u_SAF2_lh(t,"woodchips");

*-----------------------------
* Fuel-to-heat links (HT blocks)
*-----------------------------
HT_sint_fuels(t)..         HT_sint(t)  =E= SUM(hhFuel, phi_sint_hh(t,hhFuel)*u_sint_hh(t,hhFuel));
HT_sint_A_all(hhFuel,t)..  A_sint_hh(hhFuel)*HT_sint(t) =L= u_sint_hh(t,hhFuel);

HT_SAF1_fuels(t)..         HT_SAF1(t)  =E= SUM(hhFuel, phi_SAF1_hh(t,hhFuel)*u_SAF1_hh(t,hhFuel));
HT_SAF1_A_all(hhFuel,t)..  A_SAF1_hh(hhFuel)*HT_SAF1(t) =L= u_SAF1_hh(t,hhFuel);

HT_SAF2_fuels(t)..         HT_SAF2(t)  =E= SUM(hhFuel, phi_SAF2_hh(t,hhFuel)*u_SAF2_hh(t,hhFuel));
HT_SAF2_A_all(hhFuel,t)..  A_SAF2_hh(hhFuel)*HT_SAF2(t) =L= u_SAF2_hh(t,hhFuel);

*-----------------------------
* CAPACITY CONSTRAINTS
*-----------------------------
Capacity_SAF1(t)..         pFeMn1(t) + pSiMn1(t) =L= Q_SAF(t);
Capacity_SAF2(t)..         pFeMn2(t) + pSiMn2(t) =L= Q_SAF(t);
Capacity_sinter(t)..       pSinter(t)            =L= Q_sint(t);

*-----------------------------
* OBJECTIVE
*-----------------------------
Objective_def..
    Z =E= SUM(t,
           FIXEDCOST(t)
         + C_ore(t)*uOre(t)
         + C_buysinter(t)*bSinter(t)
         + C_coke(t)*(uCoke_sint(t) + uCoke_total_SAF(t))
         + C_elec(t)*(eSAF1(t) + eSAF2(t) + eSinter(t))
         + C_ets(t)*(CO2_coke_sint(t) + CO2_offgas(t))
    );

* tie free var TAC to Z (optional)
TAC =E= Z;

MODEL Ferroglobe / ALL /;

SOLVE Ferroglobe USING LP MINIMIZING Z;

* REPORTING (examples)
DISPLAY Z.l, pFeMn1.l, pFeMn2.l, pSiMn1.l, pSiMn2.l, pSinter.l;
DISPLAY eSAF1.l, eSAF2.l, eSinter.l, CO2_coke_sint.l, CO2_offgas.l;
