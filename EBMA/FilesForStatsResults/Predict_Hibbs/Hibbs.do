log using presvote_q_version_2009-09-03_webpostv2, replace

* Re-estimation of Bread and Peace Model using BEA NIPA revision of 2009-08-27
version 9
set logtype text

cd  C:\myfiles-2008\PresVoteUS-2008\Post2008Restimation\
 use presvote_q_version_2009-09-03_webpostv2.dta

* definitions and sources of variables:
describe, full

sort qdates
tsset qdates, quarterly

sort qdates
*generate real income growth rate variable used in the model
gen r = dpi_pc/(cpi_sa_8284/100)
gen lnr = log(r)
gen dlnr = (lnr-L1.lnr)*400

/* Bread and Peace model, nonlinear ("nl") estimation.
Note that the model assumes Presidents get a full term (4 year) "grace period" for inherited wars,
which mainly affects the 1972 election (Nixon having inherited Vietnam from Johnson). See Hibbs, Public Choice 2000 for demonstration.
*/

nl ( presvote = {b0} + {bdlnr}*((1.0*wtq16*dlnr+{g}*L1.dlnr ///
+{g}^2*L2.dlnr+{g}^3*L3.dlnr+{g}^4*L4.dlnr+{g}^5*L5.dlnr ///
+{g}^6*L6.dlnr +{g}^7*L7.dlnr+{g}^8*L8.dlnr+{g}^9*L9.dlnr ///
+{g}^10*L10.dlnr+{g}^11*L11.dlnr ///
+{g}^12*L12.dlnr+{g}^13*L13.dlnr+{g}^14*L14.dlnr) / (1.0*wtq16+{g} ///
+{g}^2+{g}^3+{g}^4+{g}^5+{g}^6+{g}^7+{g}^8+{g}^9+{g}^10+{g}^11 ///
+{g}^12+{g}^13+{g}^14)) + {bkia}*Fatalities ) ///
if year>1948,variables(presvote dlnr wtq16 Fatalities) ///
initial(b0 45 g 0.95 bdlnr 4 bkia -0.1) iterate(500) nolog

predict vhat if e(sample), yhat
predict error  if e(sample), residuals

display _b[b0]
display _b[bdlnr]
display _b[g]
display _b[bkia]

gen b0 = _b[b0]
gen g = _b[g]
gen bdlnr = _b[bdlnr]
gen bkia = _b[bkia]


/* generate regression input and output variables for graph.
Rbar is the weighted average of per capita real income growth rates using estimate of "g" =0.91 obtained above
*/

gen Rbar = ///
(1.0*wtq16*dlnr+g*L1.dlnr ///
+g^2*L2.dlnr+g^3*L3.dlnr+g^4*L4.dlnr  ///
+g^5*L5.dlnr+g^6*L6.dlnr ///
+g^7*L7.dlnr+g^8*L8.dlnr+g^9*L9.dlnr  ///
+g^10*L10.dlnr+g^11*L11.dlnr ///
+g^12*L12.dlnr+g^13*L13.dlnr  ///
+g^14*L14.dlnr) / (1.0*wtq16+g ///
+g^2+g^3+g^4+g^5+g^6+ ///
g^7+g^8+g^9+g^10+g^11 ///
+g^12+g^13+g^14) if electq==1 &year>1948

label var Rbar "Elect quarter weighted-avg per capita real disp personal income growth at g=.91"
 
* gen vote = presvote

/* Note that OLS estimation of the Bread and Peace Model using the Rbar variable created above and Fatalities variable 
returns the nonlinear estimates of bread and peace effects:
*/
reg presvote Rbar Fatalities

 
gen vote_no526808 = presvote - bkia*Fatalities 
replace vote_no526808 =. if year==1952 | year==1968 | year==2008

gen vhat_no526808 = vhat - bkia*Fatalities 
replace vhat_no526808 =. if year==1952 | year==1968 | year==2008


gen year_no526808 = year 
replace year_no526808 =. if  year==1952 | year==1968 | year==2008

gen vote526808 = presvote if year==1952 | year==1968 | year==2008

* Add back in kia effect to 1952 1968 and 2008 and minor effects for 1964 and 1976

gen vhat_nokia = vhat - bkia*Fatalities
replace vhat_nokia = . if  year==1952 | year==1968 | year==2008

gen pos = 3
replace pos = 2 if year==1956
replace pos = 4 if year==1996
replace pos = 1 if year==2000
replace pos = 11 if year==1988
replace pos = 12 if year==1960
replace pos = 12 if year==2008
replace pos = 12 if year==1980
replace pos = 12 if year==1964


* generating the graph depicting effects of weighted average real income growth and military and fatalities combined

gen RbarKia = bdlnr*Rbar+bkia*Fatalities

graph twoway connected vote_no526808 vhat vote526808 RbarKia, ///
   msymbol(O i O)  /// 
 connect(i l i)  lcolor(white blue) lwidth(none thick)  ///
 mlabel(year_no526808)  xsize(6.2) ysize(4.5) mlabvposition(pos)   ///
 title("Bread and Peace Voting in US Presidential Elections 1952-2008"" " , ///
  margin(medsmall) size(medlarge) justification(center) ) ///
 ytitle("Incumbent share of two-party vote (%)", /// 
  margin(medsmall) size(medium) ) ///
 xtitle("Real income growth and military fatalities combined", /// 
  margin(small) size(medium) ) ///
 xlabel(-2(1)16) msize(large large large) ///
 ylabel(40(5)65 ) ///
 legend(off)   ///
 mfcolor(black black red) mcolor(black black red) msize(3 3 3) ///
 text(44.6 -1.2 " 1952", place(se) size(small) ) ///
 text(49.6 3.4 " 1968    ", place(s) size(small) ) ///
 text(46.3 -0.6 " 2008", place(ne) size(small) ) ///
graphregion(fcolor(white) margin(zero)  )  ///
 note("Combination of real growth and fatalities weights each variable by its estimated coefficient." /// 
"Estimated fatalities effects: -0.7% 2008, -7.6% 1968, -9.9% 1952; negligible in 1964, 1976, 2004." ///
"Source: www.douglas-hibbs.com", size(small) margin(medsmall) justification(left) )

* list of the data graphed
describe presvote Rbar Fatalities
list year qdates presvote Rbar Fatalities if electq==1 & year>1948, clean noobs
