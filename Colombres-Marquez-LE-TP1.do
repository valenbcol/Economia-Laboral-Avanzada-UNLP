/*******************************************************************************
                             Trabajo práctico 1
*******************************************************************************/



cd "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1"

clear all 
use "Individual_t115.dta"

set seed 123
set obs 100000

*==================================Punto 1======================================*
* Computo media y desvío estándar de los ingresos basado en la EPH 
* Genero variable de salario horario, dividiendo el salario mensual en la cantidad de horas trabajadas

gen salarios=p21/pp3e_tot

* Pondero la variable de salarios 
sum salarios [w=pondera]

* Establecemos la media y el desvío estandar que tendrá la distribución log normal de los salarios. 

local target_mean = 193.686
local target_sd = 185.0953

do "genlognormal.do"
genlognormal salarioslnormal, mean(193) sd(185)
sum salarioslnormal, d


*==============================================================================*

* Simulamos un efecto de 0.05 desvíos estándar con 2000 obs
* Repito 300 veces y me fijo el porcentaje de veces que rechazo H0

*=================================Punto 2=======================================*
/* Realice 300 veces una simulación simple, utilizando una muestra de 2000 individuos y un effect size de 0.2, 
y determine qué porcentaje de veces durante su simulación ha logrado rechazar la hipótesis nula */

mat R = J(300,1,.) 
forvalues x=1(1)300 {
preserve

sample 2000, count

gen temp = runiform(0,1)
gen T=0
replace T = 1 if temp<0.5

sum salarioslnormal if T==0
replace salarioslnormal = salarioslnormal+0.2*r(sd) if T==1

reg salarioslnormal T, robust
mat results_reg=r(table)

local pvalue1=results_reg[4,1]
mat R[`x',1]=`pvalue1'

restore
}


preserve
clear
svmat R
gen reject = 0
replace reject = 1 if (R1<0.05)
drop if reject==.
sum reject 
restore



*==================================Punto 3=====================================*
* Repito la simulación pero para distintos tamaños de muestra () y para distintos efectos
* Se simulan effect sizes de 0.01, 0.05, 0.1 y 0.15 desvíos estándar
* El tamaño de la muestra varia entre 1000 y 7000 individuos, con intervalos de 1500 individuos


mat resultados = J(20,3,.)

local i=1
foreach efecto in 0.01 0.05 0.1 0.15 {
forvalues size = 1000(1500)7000 {
mat R = J(300,1,.)

forvalues x=1(1)300 {

preserve

sample `size', count

gen temp = runiform(0,1)
gen T=0
replace T = 1 if temp<0.5

sum salarioslnormal if T==0
replace salarioslnormal = salarioslnormal+`efecto'*r(sd) if T==1

reg salarioslnormal T, robust
mat results_reg=r(table)

local pvalue1=results_reg[4,1]
mat R[`x',1]=`pvalue1'

restore
}

preserve
clear
svmat R
gen reject = 0
replace reject = 1 if (R1<0.05)
drop if reject==.
sum reject
scalar media = r(mean)

mat resultados[`i',1] = `size'
mat resultados[`i',2] = media
mat resultados[`i',3] = `efecto'

restore

local i=`i'+1
}
}

mat list resultados

preserve
clear
svmat resultados

rename resultados1 sample_size
rename resultados2 est_power
rename resultados3 efecto

replace est_power=round(est_power,.01)
separate est_power, by(efecto)




* Gráfico
*==============================================================================*
set scheme s1color
twoway (connected est_power1 sample_size) (connected est_power2 sample_size) ///
(connected est_power3 sample_size) (connected est_power4 sample_size) , ytitle("Power") ///
xtitle("Tamaño de la muestra") title("Salario por hora") ///
legend(label(1 "1%") label(2 "5%") label(3 "10%") label(4 "15%")) ///
legend(rows(1) title("Effect Size")) xscale(titlegap(3)) yscale(titlegap(3)) 
*==============================================================================*
restore

save graph Graph "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1\Graficos\Punto 3"


*==================================Punto 4=====================================*
* Utilizamos los mismos tamaños de efecto y tamaños muestrales del punto anterior, esta vez teniendo en cuenta que solo el 60% del grupo de tratamiento observa un aumento efectivo en los salarios
* Creamos una variable C que identifique a los tratados que efectivamente asistieron a las capacitaciones
* Introducimos a la regresión una interacción entre las variables T y C para captar las diferencias entre los tratados que efectivamente asistieron a las capacitaciones y los que no


mat resultados = J(20,3,.)

local i=1
foreach efecto in 0.01 0.05 0.1 0.15 {
forvalues size = 1000(1500)7000 {
mat R = J(300,1,.)

forvalues x=1(1)300 {

preserve

sample `size', count

gen temp = runiform(0,1)
gen T=0
replace T = 1 if temp<0.5

gen C=0
gen temp2 = runiform(0,1) if T==1
replace C=1 if temp2<0.60 & temp2!=.

sum salarioslnormal if T==0
replace salarioslnormal = salarioslnormal+`efecto'*r(sd) if C==1

reg salarioslnormal T#C T, robust
mat results_reg=r(table)

local pvalue1=results_reg[4,3]
mat R[`x',1]=`pvalue1'

restore
}

preserve
clear
svmat R
gen reject = 0
replace reject = 1 if (R1<0.05)
drop if reject==.
sum reject
scalar media = r(mean)

mat resultados[`i',1] = `size'
mat resultados[`i',2] = media
mat resultados[`i',3] = `efecto'

restore

local i=`i'+1
}
}


mat list resultados

preserve
clear
svmat resultados

rename resultados1 sample_size
rename resultados2 est_power
rename resultados3 efecto
mat list resultados

replace est_power=round(est_power,.01)
separate est_power, by(efecto)



*==============================================================================*

* Gráfico
*==============================================================================*
set scheme s1color
twoway (connected est_power1 sample_size) (connected est_power2 sample_size) ///
(connected est_power3 sample_size) (connected est_power4 sample_size) , ytitle("Power") ///
xtitle("Tamaño de la muestra") title("Salario por hora") ///
legend(label(1 "1%") label(2 "5%") label(3 "10%") label(4 "15%")) ///
legend(rows(1) title("Effect Size")) xscale(titlegap(3)) yscale(titlegap(3)) 
*==============================================================================*
restore

save graph Graph "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1\Graficos\Punto 4"




*==================================Punto 5=====================================*


mat resultados = J(20,4,.)

local i=1
foreach efecto in 0.01 0.05 0.1 0.15 {
forvalues size = 1000(1500)7000 {
mat R = J(20,2,.)

forvalues x=1(1)20 {

preserve

sample `size', count

gen temp = runiform(0,1)
gen T=0
replace T = 1 if temp<0.333
replace T = 2 if temp>0.666

sum salarioslnormal if T==0
replace salarioslnormal = salarioslnormal+`efecto'*r(sd) if T==1
replace salarioslnormal = salarioslnormal+2*`efecto'*r(sd) if T==2

tabulate(T), generate(T)

reg salarioslnormal T2 T3, robust
mat results_reg=r(table)

local pvalue1=results_reg[4,1]

local pvalue2=results_reg[4,2]


mat R[`x',1]=`pvalue1'
mat R[`x',2]=`pvalue2'

restore
}

preserve
clear
svmat R
gen reject1 = 0
replace reject1 = 1 if (R1<0.05)
drop if reject==.
sum reject1
scalar media1 = r(mean)

gen reject2 = 0
replace reject2 = 1 if (R2<0.05)
drop if reject2==.
sum reject2
scalar media2 = r(mean)

mat resultados[`i',1] = `size'
mat resultados[`i',2] = `efecto'
mat resultados[`i',3] = media1
mat resultados[`i',4] = media2


restore

local i=`i'+1
}
}

mat list resultados

preserve
clear
svmat resultados

rename resultados1 sample_size
rename resultados2 efecto
rename resultados3 est_powerA
rename resultados4 est_powerB


replace est_powerA=round(est_powerA,.01)
separate est_powerA, by(efecto)

replace est_powerB=round(est_powerB,.01)
separate est_powerB, by(efecto)

*==============================================================================*

* Gráfico
*==============================================================================*
set scheme s1color
twoway (connected est_powerA1 sample_size) (connected est_powerA2 sample_size) ///
(connected est_powerA3 sample_size) (connected est_powerA4 sample_size) , ytitle("Power") ///
xtitle("Tamaño de la muestra") title("Salario por hora") ///
legend(label(1 "1%") label(2 "5%") label(3 "10%") label(4 "15%")) ///
legend(rows(1) title("Effect Size")) xscale(titlegap(3)) yscale(titlegap(3)) 

graph save Graph "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1\Graficos\5_a"
*==============================================================================*



set scheme s1color
twoway (connected est_powerB1 sample_size) (connected est_powerB2 sample_size) ///
(connected est_powerB3 sample_size) (connected est_powerB4 sample_size) , ytitle("Power") ///
xtitle("Tamaño de la muestra") title("Salario por hora") ///
legend(label(1 "1%") label(2 "5%") label(3 "10%") label(4 "15%")) ///
legend(rows(1) title("Effect Size")) xscale(titlegap(3)) yscale(titlegap(3)) 

graph save Graph "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1\Graficos\5_b"
*==============================================================================*
restore



*==================================Punto 6=====================================*
* Se corrige utilizando el test de Bonferroni

mat resultados = J(20,4,.)

local i=1
foreach efecto in 0.01 0.05 0.1 0.15 {
forvalues size = 1000(1500)7000 {
mat R = J(300,2,.)

forvalues x=1(1)300 {

preserve

sample `size', count

gen temp = runiform(0,1)
gen T=0
replace T = 1 if temp<0.333
replace T = 2 if temp>0.666

sum salarioslnormal if T==0
replace salarioslnormal = salarioslnormal+`efecto'*r(sd) if T==1
replace salarioslnormal = salarioslnormal+2*`efecto'*r(sd) if T==2

tabulate(T), generate(T)

reg salarioslnormal T2 T3, robust
mat results_reg=r(table)

local pvalue1b=results_reg[4,1]
local pvalue2b=results_reg[4,2]


mat R[`x',1]=`pvalue1b'
mat R[`x',2]=`pvalue2b'

restore
}

preserve
clear
svmat R
gen reject1 = 0
replace reject1 = 1 if (R1<0.05/300)
drop if reject==.
sum reject1
scalar media1 = r(mean)

gen reject2 = 0
replace reject2 = 1 if (R2<0.05/300)
drop if reject2==.
sum reject2
scalar media2 = r(mean)

mat resultados[`i',1] = `size'
mat resultados[`i',2] = `efecto'
mat resultados[`i',3] = media1
mat resultados[`i',4] = media2

restore

local i=`i'+1
}
}

mat list resultados

preserve
clear
svmat resultados

rename resultados1 sample_size
rename resultados2 efecto
rename resultados3 est_powerA
rename resultados4 est_powerB


replace est_powerA=round(est_powerA,.01)
separate est_powerA, by(efecto)

replace est_powerB=round(est_powerB,.01)
separate est_powerB, by(efecto)

*==============================================================================*

* Gráfico
*==============================================================================*
set scheme s1color
twoway (connected est_powerA1 sample_size) (connected est_powerA2 sample_size) ///
(connected est_powerA3 sample_size) (connected est_powerA4 sample_size) , ytitle("Power") ///
xtitle("Tamaño de la muestra") title("Salario por hora") ///
legend(label(1 "1%") label(2 "5%") label(3 "10%") label(4 "15%")) ///
legend(rows(1) title("Effect Size")) xscale(titlegap(3)) yscale(titlegap(3)) 

graph save Graph "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1\Graficos\6_a"
*==============================================================================*



set scheme s1color
twoway (connected est_powerB1 sample_size) (connected est_powerB2 sample_size) ///
(connected est_powerB3 sample_size) (connected est_powerB4 sample_size) , ytitle("Power") ///
xtitle("Tamaño de la muestra") title("Salario por hora") ///
legend(label(1 "1%") label(2 "5%") label(3 "10%") label(4 "15%")) ///
legend(rows(1) title("Effect Size")) xscale(titlegap(3)) yscale(titlegap(3)) 

graph save Graph "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1\Graficos\6_b"
*==============================================================================*
restore




*==================================Punto 7=====================================*


mat resultados = J(20,3,.)

local i=1
foreach efecto in 0.01 0.05 0.1 0.15 {
forvalues size = 1000(1500)7000 {
mat R = J(300,1,.)

forvalues x=1(1)300 {

preserve

sample `size', count

gen temp = runiform(0,1)
gen T=0
replace T = 1 if temp<0.5

gen C=0
gen temp2 = runiform(0,1) if T==1
replace C=1 if temp2<0.60 & temp2!=.

sum salarioslnormal if T==0
replace salarioslnormal = salarioslnormal+`efecto'*r(sd) if C==1

ivregress 2sls salarioslnormal (C=T), first 
mat results_reg=r(table)

local pvalue1=results_reg[4,1]
mat R[`x',1]=`pvalue1'

restore
}

preserve
clear
svmat R
gen reject = 0
replace reject = 1 if (R1<0.05)
drop if reject==.
sum reject
scalar media = r(mean)

mat resultados[`i',1] = `size'
mat resultados[`i',2] = media
mat resultados[`i',3] = `efecto'

restore

local i=`i'+1
}
}


preserve
clear
svmat resultados

rename resultados1 sample_size
rename resultados2 est_power
rename resultados3 efecto

replace est_power=round(est_power,.01)
separate est_power, by(efecto)



*==============================================================================*

* Gráfico
*==============================================================================*
set scheme s1color
twoway (connected est_power1 sample_size) (connected est_power2 sample_size) ///
(connected est_power3 sample_size) (connected est_power4 sample_size) , ytitle("Power") ///
xtitle("Tamaño de la muestra") title("Salario por hora") ///
legend(label(1 "1%") label(2 "5%") label(3 "10%") label(4 "15%")) ///
legend(rows(1) title("Effect Size")) xscale(titlegap(3)) yscale(titlegap(3)) 
*==============================================================================*
restore

graph save Graph "C:\Documents\ME-UNLP\3° Trimestre\Tópicos Avanzados de Economía Laboral\Práctica\TP1\Graficos\Punto 7"

























