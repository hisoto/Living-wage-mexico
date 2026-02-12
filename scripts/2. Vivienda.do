//////////////////////////////////////////////////////////////////////////////
********************* ENIGH 2024 CANASTA DIGNA ********************************
///////////////////////////////////////////////////////////////////////////////

// Este do file es para el trabajo de la construcción de gastos en vivienda para una canasta digna
// eleborado por la Coordinación de Métodos Estadísticos de la Comisión Nacional de 
// los Salarios Mínimos. 
// Autora: Denisse López Cruz
// Se requiere de una versión de STATA mayor a 15.

// Se pueden descargar las bases de la Encuesta Nacional de Ingresos y
// Gastos de los Hogares (ENIGH) 2024 en formato DTA en: https://www.inegi.org.mx/programas/enigh/nc/2022/#microdatos

clear all
set more off
set type double 

// Se establecen los directorios
gl bases "C:\Users\ivan_\OneDrive - Comision Nacional de los Salarios Minimos\Escritorio\investigaciones\salario_digno_project\Living wage mexico\data"
gl final "C:\Users\ivan_\OneDrive - Comision Nacional de los Salarios Minimos\Escritorio\investigaciones\salario_digno_project\Living wage mexico\finaldata\vivienda"
gl graph "C:\Users\ivan_\OneDrive - Comision Nacional de los Salarios Minimos\Escritorio\investigaciones\salario_digno_project\Living wage mexico\graphs\vivienda"

// Indicadores para la vivienda digna 
use "$bases\enigh2024_ns_viviendas_dta\viviendas.dta", clear

/// Separar por ambitos
gen rural = substr(folioviv,3,1) == "6"
gen urbano = cond(rural==0,1,0)

/// Crear entidades
gen ent = substr(folioviv,1,2) 

/// Se eliminan estas variables para poder hacer el destring y calcular los indicadores
drop if mat_pisos=="&"
drop if combus=="&"

/// Se aplica destring para que todas las variables sean número y puedan operarse:
destring ent mat_pared mat_techos mat_pisos antiguedad antigua_ne cocina cocina_dor cuart_dorm num_cuarto lugar_coc agua_ent ab_agua agua_noe dotac_agua excusado uso_compar sanit_agua biodigest bano_comp bano_excus bano_regad drenaje disp_elect focos focos_ahor combus fogon_chi eli_basura tenencia renta estim_pago pago_viv pago_mesp tipo_adqui viv_usada finan_1 finan_2 finan_3 finan_4 finan_5 finan_6 finan_7 finan_8 num_dueno1 hog_dueno1 num_dueno2 hog_dueno2 escrituras lavadero fregadero regadera tinaco_azo cisterna pileta calent_sol calent_gas calen_lena medid_luz bomba_agua tanque_gas aire_acond calefacc p_grietas p_pandeos p_levanta p_humedad p_fractura p_electric p_tuberias tot_resid, replace

/// La construcción de los indicadores de vivienda digna se contruyen a tráves de lo establecido
/// por ONU hábitat 

/// Tenencia 
* Condiciones que garanticen a sus ocupantes proteccion juridica contra desalojo, hostigamiento o amenazas

gen ten=cond(tenencia==1 | tenencia==3 | tenencia==4,1,0) // Considera solo rentadas, propias que se estan pagando y propias
gen etem=cond((escrituras==1 | escrituras==2) & tenencia==4, 1,0) // Que tenga escrituras
gen ten_f=cond((ten==1 & etem==1) | (ten==1 & etem==0),1,0) // Tenencia

/// Disponibilidad
* Disponibilidad de servicios, materiales, instalaciones e infraestructura

/// Cocina
gen coc=cond(cocina==1,1,0) // Tiene cocina
gen coc_dor=cond(cocina_dor==2,1,0) // No la usa como dormitario
gen coc_f=cond(coc==1 & coc_dor==1,1,0) // Cocina disponible

/// Agua
gen agua_e=cond(agua_ent==1 | agua_ent==2,1,0) // Agua entubada
gen agua_ab=cond(ab_agua>=1 & ab_agua<=4,1,0) // Abastecimiento de agua 
replace agua_ab=1 if (ab_agua==6 & procaptar==1) | (agua_noe==5) | (agua_noe==6 & procaptar==1) // Captación de lluvia y agua no entubada de una pipa
gen agua_d=cond(dotac_agua==1 | dotac_agua==2, 1, 0) // Dotación de agua
gen agua_f=cond(agua_e==1 & agua_ab==1 & agua_d==1,1,0) // Agua disponible y suficiente

/// Baño
gen wc=cond(excusado==1 | excusado==2,1,0) // Hay excusado o instalación sanitaria
gen twc=cond(uso_compar==2,1,0) // No se comparte con otra vivienda
gen awc=cond(sanit_agua==1,1,0) // Tiene conexión al agua
gen red=cond(drenaje==1,1,0) // Tiene drenaje
gen wc_f=cond(wc==1 & twc==1 & awc==1 & red==1,1,0) // Sanitario disponible

/// Luz
gen luz=cond(disp_elect==1 | disp_elect==3,1,0) // Luz 
gen mluz=cond(medid_luz==1,1,0) // Medidor de luz
gen luz_f=cond(luz==1 & mluz==1,1,0) // Servicio de luz disponible

/// Servicios cocina
gen gs=cond(combus==3 | combus==4 | combus==5,1,0) // Combustible para cocinar 
replace gs=1 if (combus==1 | combus==2) & rural==1 // Se agrega leña y carbon a rural
gen chim=cond((combus==1 | combus==2) & rural==1 & fogon_chi==1,1,0) // Si se cocina con leña o carbon, que haya chimenea 
gen infra_coc=cond(gs==1,1,0) // Buena infraestructura y servicios en la cocina 
replace infra_coc=1 if chim==1 & rural==1 

/// Desechos
gen basura=cond(eli_basura==1 | eli_basura==2 | eli_basura==3,1,0) // Forma de desechar basura
replace basura=1 if (eli_basura==4 | eli_basura==5) & rural==1 // En rural se añade "la queman" y "la entierran"

gen disponibilidad=cond(coc_f==1 & agua_f==1 & wc_f==1 & luz_f==1 & infra_coc==1 & basura==1, 1,0) 

/// Habitabilidad 
* Condiciones que garantizan la seguridad fisica y espacios habitables suficientes 

gen paredes=cond((mat_pared==8) | (mat_pared==7 & rural==1),1,0) // Material de paredes
gen techos=cond(mat_techos==10 | mat_techos==9,1,0) // Material de techos
gen pisos=cond((mat_pisos==2 | mat_pisos==3), 1,0) // Material de pisos
gen cuarto=(cuart_dorm>=tot_resid/2.5) // Hay un dormitorio, por cada 2.5 residentes
gen probms=cond((p_pandeos==1 | p_levanta==1 | p_humedad==1 | p_fractura==1),0,1) 

gen habitabilidad=cond(paredes==1 & techos==1 & pisos==1 & cuarto==1 & probms==1,1,0) 

save "$final\prob_viv_final.dta", replace

/// Es necesario unir la base de Concentrado Hogar para medir la asequibilidad (ONU Hábitat) y la cantidad de viviendas dignas. 

use "$final\prob_viv_final.dta", clear

merge 1:m folioviv using "$bases\enigh2024_ns_concentradohogar_dta\concentradohogar.dta"

tab _merge // Se comprueba la unión
drop _merge 

save "$final\prob_canviv_final.dta", replace

/// Final de los indicadores y estadisticas descriptivas
use "$final\prob_canviv_final.dta", clear

/// Asequibilidad 
* El gasto en vivienda debe ser menos del 30% de su ingresos
gen gasto_viv=estim_alqu+vivienda // Incluye el gasto en alquiler, luz, agua y predial
gen aseq=(gasto_viv<(ing_cor*0.30)) // El gasto en vivienda es menor al 30% del ingreso 

gen viv_dig=cond(ten_f==1 & disponibilidad==1 & aseq==1 & habitabilidad==1,1,0)
tab viv_dig [fw=factor] 

// Es necesario revisar la distribución de la base con los gastos en vivienda
// para borrar los outliers

graph box gasto_viv, ///
ytitle("Gasto en vivienda") ylabel(, angle(horizontal) labsize(small) format (%9.0gc)) ///
box(1, lwidth(medthin) lcolor("98 19 51")) ///
marker(1, mcolor("152 152 154")) ///
scheme(s1mono) graphregion(color(white)) plotregion(style(none)) ///
yline(0, lcolor(gs14)) legend(off)
graph export "$graph\prob_bigotes1.png", replace

xtile p_nac = gasto_viv [pw=factor], nq(100)    
drop if p_nac < 1 | p_nac > 99  // Se cortan las colas del 1% nada más

graph box gasto_viv, ///
ytitle("Gasto en vivienda") ylabel(, angle(horizontal) labsize(small) format (%9.0gc)) ///
box(1, lwidth(medthin) lcolor("98 19 51")) ///
marker(1, mcolor("152 152 154")) ///
scheme(s1mono) graphregion(color(white)) plotregion(style(none)) ///
yline(0, lcolor(gs14)) legend(off)
graph export "$graph\prob_bigotes2.png", replace

save "$final\prob_final_canviv.dta", replace

// Se realizan varios merges para la estadistica descriptiva
use "$final\prob_final_canviv.dta", clear

* Rubros ONU Habitat por entidad
preserve 
collapse (sum) ten_f disponibilidad aseq habitabilidad [fw=factor], by(ent)
save "$final\prob_final_viv.dta", replace
export excel "$final/prob_final_viv.xlsx", sheet("Totales") firstrow(varlabels) replace
restore 

* Rubros ONU Habitat por entidad y ámbito
preserve 
collapse (sum) ten_f disponibilidad aseq habitabilidad [fw=factor], by(ent rural)
save "$final\prob_final_viv_uru.dta", replace
export excel "$final/prob_final_viv.xlsx", sheet("Totales Uru") firstrow(varlabels) 
restore 

* Viendas Dignas: Gasto en vivienda y alquileres promedio por entidades
preserve
keep if viv_dig==1
collapse (mean) gasto_viv alq (sum) viv_dig [fw=factor], by(ent)
save "$final\prob_final_viv_d.dta", replace
export excel "$final/prob_final_viv.xlsx", sheet("Dignas") firstrow(varlabels)
restore

* Viendas Dignas: Gasto en vivienda y alquileres promedio por entidades y ámbito
preserve
keep if viv_dig==1
collapse (mean) gasto_viv alq (sum) viv_dig [fw=factor], by(ent rural)
save "$final\prob_final_viv_d_uru.dta", replace
export excel "$final/prob_final_viv.xlsx", sheet("Dignas Uru") firstrow(varlabels)
restore

* Estadisticas Nacionales
preserve
collapse (mean) gasto_viv alq (sum) viv_dig ten_f disponibilidad aseq habitabilidad [fw=factor]
save "$final\prob_final_viv_nac.dta", replace
export excel "$final/prob_final_viv.xlsx", sheet("Nacionales") firstrow(varlabels)
restore

/// Es necesario revisar el comportamiento de las variables para revisar y realizar
/// el calculo del estarto de referencia
use "$final\prob_final_canviv.dta", clear

svyset upm [pw=factor], strata(est_dis) singleunit(centered) // Se establece el diseño de la encuesta

svy: mean ten etem coc_f agua_f wc_f luz_f infra_coc basura aseq paredes techos pisos cuarto probms, over(p_nac)

preserve
collapse (mean) ten etem coc_f agua_f wc_f luz_f infra_coc basura aseq paredes techos pisos cuarto probms [pw=factor], by(p_nac)
rename (ten etem coc_f agua_f wc_f luz_f infra_coc basura aseq paredes techos pisos cuarto probms) (mn_ten mn_etem mn_coc mn_agua mn_wc mn_luz mn_infrac mn_basura mn_aseq mn_paredes mn_techos mn_pisos mn_cuarto mn_prob)
tempfile temp
save `temp'
restore

merge m:1 p_nac using `temp', nogen

foreach var of varlist mn_ten mn_etem mn_coc mn_agua mn_wc mn_luz mn_infrac mn_basura mn_aseq mn_paredes mn_techos mn_pisos mn_cuarto mn_prob {
    gen `var'_pct = `var' * 100
    format `var'_pct %9.1f
}

// Se grafica el comportamiento de las variables

twoway connected mn_ten_pct p_nac, lcolor("156 35 72") msymbol(none) || ///
connected mn_etem_pct p_nac, lcolor("152 152 154")  msymbol(none) ||, ///
ytitle("Porcentaje de Tenencia", size(small)) xtitle("Percentil nacional", size(small) margin(t+4)) ///
graphregion(color(white)) plotregion(color(white)) ///
legend(order(1 "Tenencia" 2 "Escrituras") ///
position(6) ring(1) ///
region(lc(none) fc(none)) row(1) size(small)) ///
ylabel(0(20)100, angle(horizontal) labsize(small) format(%9.0g)) ysize(5.8) xsize(7.9)
graph export "$graph\final_tenencia.png", replace

twoway connected mn_coc_pct p_nac, lcolor("156 35 72") msymbol(none) || ///
connected mn_agua_pct p_nac, lcolor("152 152 154")  msymbol(none) || ///
connected mn_wc_pct p_nac, lcolor("30 91 79")   msymbol(none) || ///
connected mn_luz_pct p_nac, lcolor("166 128 45")   msymbol(none) || ///
connected mn_infrac_pct p_nac, lcolor("98 19 51")   msymbol(none) || ///
connected mn_basura_pct p_nac, lcolor("231 210 149")   msymbol(none) ||, ///
ytitle("Porcentaje de disponibilidad", size(small)) xtitle("Percentil nacional", size(small) margin(t+4)) ///
graphregion(color(white)) plotregion(color(white)) ///
legend(order(1 "Cocina" 2 "Agua" 3 "WC" 4 "Luz" 5 "Infraestructura de Cocina" 6 "Basura") ///
position(6) ring(1) ///
region(lc(none) fc(none)) row(2) size(small)) ///
ylabel(0(20)100, angle(horizontal) labsize(small) format(%9.0g)) ysize(5.8) xsize(7.9)
graph export "$graph\final_disponibilidad.png", replace

twoway connected mn_aseq_pct p_nac, lcolor("156 35 72") msymbol(none) ||, ///
ytitle("Porcentaje de Asequibilidad", size(small)) xtitle("Percentil nacional", size(small) margin(t+4)) ///
graphregion(color(white)) plotregion(color(white)) ///
legend(order(1 "Asequibilidad") ///
region(lc(none) fc(none)) row(1) size(small)) ///
ylabel(0(20)100, angle(horizontal) labsize(small) format(%9.0g)) ysize(5.8) xsize(7.9)
graph export "$graph\final_asequibilidad.png", replace

twoway connected mn_paredes_pct p_nac, lcolor("156 35 72") msymbol(none) || ///
connected mn_techos_pct p_nac, lcolor("152 152 154") msymbol(none) || ///
connected mn_pisos_pct p_nac, lcolor("30 91 79") msymbol(none) || ///
connected mn_cuarto_pct p_nac, lcolor("166 128 45")  msymbol(none) || ///
connected mn_prob_pct p_nac, lcolor("98 19 51")  msymbol(none) ||, ///
ytitle("Porcentaje de Habitabilidad", size(small)) xtitle("Percentil nacional", size(small) margin(t+4)) ///
graphregion(color(white)) plotregion(color(white)) ///
legend(order(1 "Material: Paredes" 2 "Material: Techos" 3 "Material: Pisos" 4 "Cuartos" 5 "Problemas") ///
position(6) ring(1) ///
region(lc(none) fc(none)) row(2) size(small)) ///
ylabel(0(20)100, angle(horizontal) labsize(small) format(%9.0g)) ysize(5.8) xsize(7.9)
graph export "$graph\prob_final_habitabilidad.png", replace

// Se realiza el procedimiento para calcular el estrato de referencia

gen indice=ten_f+disponibilidad+aseq+habitabilidad // Indice de Vivienda Digna 

svy: mean ten_f disponibilidad aseq habitabilidad, over(p_nac)

preserve
collapse (mean) ten_f disponibilidad aseq habitabilidad [pw=factor], by(p_nac)
rename (ten_f disponibilidad aseq habitabilidad) (mean_ten mean_dis mean_aseq mean_hab)
tempfile temp
save `temp'
restore

merge m:1 p_nac using `temp', nogen

foreach var of varlist mean_ten mean_dis mean_aseq mean_hab {
    gen `var'_pct = `var' * 100
    format `var'_pct %9.1f 
}

preserve
keep mn_ten_pct mn_etem_pct mn_coc_pct mn_agua_pct mn_wc_pct mn_luz_pct mn_infrac_pct mn_basura_pct mn_aseq_pct mn_paredes_pct mn_techos_pct mn_pisos_pct mn_cuarto_pct p_nac factor mean_ten_pct mean_dis_pct mean_aseq_pct mean_hab_pct indice
save "$final\prob_graph_viv_nac.dta", replace
restore

/// Gráfica del comportamiento de los cuatro rubros de ONU Habitat 
twoway connected mean_hab_pct p_nac, lcolor("152 152 154") msymbol(none) || ///
connected mean_dis_pct p_nac, lcolor("166 128 45") msymbol(none) || ///
connected mean_ten_pct p_nac, lcolor("98 19 51") msymbol(none) || ///
connected mean_aseq_pct p_nac, lcolor("64 64 64") msymbol(none) ||, ///
ytitle("Porcentaje de caracteristicas", size(small)) xtitle("Percentil nacional", size(small) margin(t+4)) ///
graphregion(color(white)) plotregion(color(white)) ///
legend(order(1 "Habitabilidad" 2 "Disponibilidad" 3 "Tenencia" 4 "Asequibilidad") ///
position(6) ring(1) ///
region(lc(none) fc(none)) row(1) size(small)) ///
ylabel(0(20)100, angle(horizontal) labsize(small) format(%9.0g)) ysize(5.8) xsize(7.9)
graph export "$graph\prob_final_rubros.png", replace

/// Se establecen los criterios para la selección del estrato
gen c_u=cond(indice>=1,1,0) // Unión, los hogares cumplen con al menos un grande rubro de ONU Hábitat
gen c_in=cond(indice>=2,1,0) // Intermedio, los hogares cumplen con al menos dos grandes rubros de ONU Hábitat
gen c_interseccion=cond(indice==4,1,0) // Intersección, los hogares cumplen con todos los rubos de ONU Hábitat

xtile p_naci = gasto_viv [pw=factor], nq(10)

svy: mean c_u c_in c_interseccion, over(p_naci)

preserve
collapse (mean) c_u c_in c_interseccion [pw=factor], by(p_naci)
rename (c_u c_in c_interseccion) (ic_u ic_in ic_interseccion)
tempfile temp_i
save `temp_i'
restore

merge m:1 p_naci using `temp_i', nogen

preserve 
collapse (mean) ic_u ic_in ic_interseccion [fw=factor], by(p_naci)
save "$final\prob_final_estrato.dta", replace 
export excel "$final/prob_final_estratos.xlsx", sheet("Deciles") firstrow(varlabels) replace
restore
*/
/// Calculo de la regresion cuantilica para la estimación del gasto en vivienda digna
use "$final\prob_final_canviv.dta", clear

drop p_nac
xtile p_nac = gasto_viv [pw=factor], nq(100) //Se vuelven a medir los percentiles por el recorte de colas

gen ln_viv = ln(gasto_viv)

// Ámbito Urbano
preserve
bootstrap, reps(100): rifhdreg ln_viv i.ent i.rural i.ten_f i.coc_f i.agua_f i.wc_f i.luz_f i.infra_coc i.basura i.aseq i.paredes i.techos i.pisos i.cuarto i.probms, rif(q(70))

matrix coef=e(b)
matrix list coef  

estimates store modelo_rif 

levelsof ent, local(nv_ent)
clear
set obs `:word count `nv_ent''
gen ent = ""
gen rural = 0
gen ten_f = 1
gen coc_f = 1
gen agua_f = 1
gen wc_f = 1
gen luz_f = 1
gen infra_coc = 1
gen basura = 1
gen aseq = 1
gen paredes = 1
gen techos = 1
gen pisos = 1
gen cuarto = 1
gen probms = 1

local i 1
foreach nivel in `nv_ent' {
    replace ent = "`nivel'" in `i'
    local ++i
}

describe ent
destring ent, replace 
describe ent

estimates restore modelo_rif
predict yhat_urbano 
gen pred_exp_urbano = exp(yhat_urbano)
save "$final/prob_resultados_urbanos.dta", replace
restore

preserve
levelsof ent, local(nv_ent)
clear
set obs `:word count `nv_ent''
gen ent = ""
gen rural = 1
gen ten_f = 1
gen coc_f = 1
gen agua_f = 1
gen wc_f = 1
gen luz_f = 1
gen infra_coc = 1
gen basura = 1
gen aseq = 1
gen paredes = 1
gen techos = 1
gen pisos = 1
gen cuarto = 1
gen probms = 1

local i 1
foreach nivel in `nv_ent' {
    replace ent = "`nivel'" in `i'
    local ++i
}

describe ent
destring ent, replace 
describe ent

estimates restore modelo_rif
predict yhat_rural 
gen pred_exp_rural = exp(yhat_rural)
save "$final/prob_resultados_rural.dta", replace
restore

// Nacional
preserve
bootstrap, reps(100): rifhdreg ln_viv i.rural i.ten_f i.coc_f i.agua_f i.wc_f i.luz_f i.infra_coc i.basura i.aseq i.paredes i.techos i.pisos i.cuarto i.probms, rif(q(70))

matrix coef=e(b)
matrix list coef  

estimates store modelo_rifn 

clear
set obs 1
gen rural = 1
gen ten_f = 1
gen coc_f = 1
gen agua_f = 1
gen wc_f = 1
gen luz_f = 1
gen infra_coc = 1
gen basura = 1
gen aseq = 1
gen paredes = 1
gen techos = 1
gen pisos = 1
gen cuarto = 1
gen probms = 1

estimates restore modelo_rifn
predict yhat_ruraln
gen pred_exp_ruraln = exp(yhat_ruraln)
save "$final/prob_resultados_nacional_rural.dta", replace
restore

preserve
clear
set obs 1
gen rural = 0
gen ten_f = 1
gen coc_f = 1
gen agua_f = 1
gen wc_f = 1
gen luz_f = 1
gen infra_coc = 1
gen basura = 1
gen aseq = 1
gen paredes = 1
gen techos = 1
gen pisos = 1
gen cuarto = 1
gen probms = 1

estimates restore modelo_rifn
predict yhat_urbanon
gen pred_exp_urbanon = exp(yhat_urbanon)
save "$final/prob_resultados_nacional_urbano.dta", replace
restore

/// Para el producto final

use "$final/prob_resultados_urbanos.dta", clear
append using "$final/prob_resultados_rural.dta", force
append using "$final/prob_resultados_nacional_rural.dta", force
append using "$final/prob_resultados_nacional_urbano.dta", force

replace ent=33 if ent==.
rename ent cve_ent
egen pred = rowfirst(pred_exp_rural pred_exp_urbano pred_exp_ruraln pred_exp_urbanon)

gsort -rural +cve_ent
order cve_ent pred rural

export delimited cve_ent pred rural using "$final/canasta_vivienda.csv", replace