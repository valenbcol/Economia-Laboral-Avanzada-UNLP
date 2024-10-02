/*******************************************************************************
			Trabajo Práctico 2 – Tópicos de Economía Laboral Avanzada: 
				Tópicos recientes de Diferencias en Diferencias
									
* Fernanda Marquez Ragonesi
* Valentina Brizuela Colombres
*******************************************************************************/

cd "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP2LE\Datos"


*==================================Ejercicio 1======================================*
clear all
use "negw.dta"
describe

tab t D

gen t_treat = t*D
replace t_treat=. if t_treat==0
bys id: egen first_treat = min (t_treat)
tab first_treat, m

* Hay differential timing en el tratamiento

* Generamos una variable que indica el periodo relativo al treatment para cada unidad
	*+n son n periodos luego del tratamiento
	*-n son n periodos antes del tratamiento
gen rel_time =t-first_treat

summ rel_time 
local relmin = abs(r(min))
local relmax = abs(r(max))

*leads: periodos previos al tratamiento
forval x=2/`relmin'{
	gen lead_`x' = rel_time==-`x'
	}
		
*lags: periodos posteriores al tratamiento
forval x=2/`relmax'{
	gen lag_`x' = rel_time==`x'
	}

* Estimación por TWFE: 
	*estimamos un modelo de regresión lineal con efectos fijo por id y t y agrupando los erroes estandar por la variable id
reghdfe Y lead_* lag_*, absorb(id t) cluster(id)

estimates store twfe

event_plot twfe, stub_lag(lag_#) stub_lead(lead_#) ///
    plottype(scatter) ciplottype(rcap) ///
    together perturb(-0.30(0.10)0.30) trimlead(20) trimlag(20) noautolegend ///
    graph_opt(   ///
        xtitle("Relative time to treatment") ///
        ytitle("Effect") ///
        xlabel(-20(5)20) ///
        yline(0, lp(dash) lc(gs0) lw(thin)) ///
        xline(0, lp(dash) lc(gs0) lw(thin)) ///
    ) ///    
    lag_opt(color(navy)) ///
    lead_opt(color(red)) lag_ci_opt(color(navy%45 navy%45)) ///
    lead_ci_opt(color(red%45 red%45))

*==================================Ejercicio 2======================================*
* Metodología propuesta por de Chaisemartin and D’Haultfuille (2020)


did_multiplegt Y id t D, robust_dynamic dynamic(20) placebo (20) breps (5) cluster (id)

matrix didmgt_b = e(estimates)
matrix didmgt_v = e(variances)

event_plot didmgt_b#didmgt_v, stub_lag(Effect_#) stub_lead(Placebo_#) plottype(scatter) ciplottype(rcap)	together perturb(-0.30(0.10)0.30) trimlead(20) trimlag(20) noautolegend ///
    graph_opt(   ///
        xtitle("Relative time to treatment") ///
        ytitle("Effect") ///
        xlabel(-20(5)20) ///
        yline(0, lp(dash) lc(gs0) lw(thin)) ///
        xline(0, lp(dash) lc(gs0) lw(thin)) ///
    ) ///    
    lag_opt(color(navy)) ///
    lead_opt(color(red)) lag_ci_opt(color(navy%45 navy%45)) ///
    lead_ci_opt(color(red%45 red%45))

*==================================Ejercicio 3======================================*
* Metodología propuesta por Callaway and SantAnna (2021)

gen gvar=first_treat
recode gvar(.=0)

csdid Y, ivar(id) time(t) gvar(gvar) notyet

estat event, estore(csdd)

event_plot csdd, stub_lag(Tp#) stub_lead(Tm#)	plottype(scatter) ciplottype(rcap)	together perturb(-0.30(0.10)0.30) trimlead(20) trimlag(20) noautolegend ///
    graph_opt(   ///
        xtitle("Relative time to treatment") ///
        ytitle("Effect") ///
        xlabel(-20(5)20) ///
        yline(0, lp(dash) lc(gs0) lw(thin)) ///
        xline(0, lp(dash) lc(gs0) lw(thin)) ///
    ) ///    
    lag_opt(color(navy)) ///
    lead_opt(color(red)) lag_ci_opt(color(navy%45 navy%45)) ///
    lead_ci_opt(color(red%45 red%45))

*==================================Ejercicio 4======================================*
clear all
use "honestdid.dta"

gen t_treat = t*D

replace t_treat=. if t_treat==0
bys id: egen first_treat = min (t_treat)
tab first_treat, m
* No hay problema de differential timing en el tratamiento

gen treated =(first_treat==6)
char t[omit]5
xi i.treated*i.t

*Estimación mediante TWFE
reghdfe Y _ItreXt_*_*, absorb(id t) cluster(id) noconstant

coefplot, vertical yline(0, lpattern(_) lcolor(gs0)) msymbol(O) ///
	mlcolor(navy) mfcolor(navy) ciopts(recast(rcap) ///
	lcolor(navy%45 navy%45)) legend(off) nooffsets ///
	coeflabels(, labsize(small)) ///
     xlabel(, angle(45))
	
*==================================Ejercicio 5======================================*
* Metodología propuesta por Rambachan y Roth(2023)

* A: Relative magnitudes restrictions
honestdid, pre(1/5) post(6/7) mvec(0.5(0.5)4) coefplot ///
    lag_opt(color(navy)) ///
    lead_opt(color(red)) lag_ci_opt(color(navy%45 navy%45)) ///
    lead_ci_opt(color(red%45 red%45))
	
* B: Smoothness restrictions

honestdid, pre(1/5) post(6/7) mvec(0(0.01)0.05) delta(sd) coefplot ///
    lag_opt(color(navy)) ///
    lead_opt(color(red)) lag_ci_opt(color(navy%45 navy%45)) ///
    lead_ci_opt(color(red%45 red%45))



*==================================================================================*
