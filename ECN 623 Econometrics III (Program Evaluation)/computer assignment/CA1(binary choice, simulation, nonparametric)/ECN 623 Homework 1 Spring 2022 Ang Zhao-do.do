log using "C:\Users\azhao02\OneDrive - Syracuse University\ECN 623 Homework 1 Spring 2022 Ang Zhao", replace
**********************************part B****************************
use "C:\Users\azhao02\OneDrive - Syracuse University\fl8991pam6090.dta" , clear
 
 *generate the quadratic term
 gen sqage = dmage*dmage

 ***The probit model question
 **regression
 probit dead dmage sqage dmeduc dmar mblack mhispan motherr foreignb tobacco alcohol
 
**prediction question
predict deadhat, pr
gen prec=0
replace prec=1 if deadhat>0.5
count if prec==dead
*Probability of crorrectly prediction: 561153/56943==0.992

**Average partial effect
probit dead dmage sqage dmar dmeduc foreignb mblack motherr mhispan tobacco alcohol
margins, dydx(*)

**Average partial effect at mean
margins, dydx(*) atmeans

**average effect when these two are considered as dummy variables
probit dead dmage sqage dmar dmeduc foreignb mblack motherr mhispan i.tobacco i.alcohol
margins, dydx(*)


***The logit model question
**repeat of question one excluding margins at mean
*logit regression
logit dead dmage sqage dmar dmeduc foreignb mblack motherr mhispan tobacco alcohol
*test how well the coefficients fit
predict deadhat2, pr
gen prec2=0
replace prec2=1 if deadhat2>0.5
count if prec2==dead
*The probability of correct prediction: 561153/565943==0.992

*Average partial effect
margins, dydx(*)
*average effect when these two are considered as dummy variables
logit dead dmage sqage dmar dmeduc foreignb mblack motherr mhispan i.tobacco i.alcohol
margins, dydx(*)

*maximum likelihood estimator
mlogit dead dmage sqage dmar dmeduc foreignb mblack motherr mhispan tobacco alcohol
*find the estimator
logit dead dmage sqage dmar dmeduc foreignb mblack motherr mhispan tobacco alcohol
ereturn list
 vce, corr
nlcom _b[dmage]/-2/_b[sqage]

***prediction of specific conditions
logit dead dmage sqage dmar dmeduc foreignb mblack motherr mhispan tobacco alcohol
*predictnl phat = normal(xb(#1)), se(phat_se) **** doesn't work for this case
predict phat
sum phat if dmage==25 & dmeduc==12 & dmar==1 & mblack==0 & mhispan==0 & motherr==0 & foreignb==0 & tobacco==1 & alcohol==0
*calculate the matrix step by step
matrix input x=(25 625 12 1 0 0 0 0 1 0 1)
matrix v=e(v)
matrix xvx=x*v*x'
sca fphat=exp(phat)/( (1+exp(-phat))^2 )
matrix vphat=(fphat^2)*xvx
matrix list vphat
**the std.err is not shown. Thus the confidence interval is unfined. 


****ML estimates of marginal effects at means
margins, dydx( dmage sqage dmeduc dmar mblack mhispan motherr foreignb tobacco alcohol)  atmeans
*marignal effects with some conditions
margins, dydx( dmage sqage dmeduc dmar mblack mhispan motherr foreignb tobacco alcohol) at (dmage=28 sqage=784  dmeduc=17  dmar=1 mblack=0 mhispan=0 motherr=0 foreignb=0 tobacco=0 alcohol=0)


****LPM model
reg dead dmage sqage dmeduc dmar mblack mhispan motherr foreignb tobacco alcohol, robust
*lincom 25*_b[dmage]+625*_b[sqage]+12*_b[dmeduc]+_b[dmar]+_b[tobacco]
predict phat2
sum phat2 if dmage==25 & dmeduc==12 & dmar==1 & mblack==0 & mhispan==0 & motherr==0 & foreignb==0 & tobacco==1 & alcohol==0
matrix olsb=e(b)
matrix olsV=e(V)
matrix probxbols=x*olsb'
matrix list probxbols
matrix Vprobxbols=x*olsV*x'
matrix list Vprobxbols

**********************************part C****************************
clear
*set obs set
set seed 10101
set obs 20

*generate distribution
gen rn1=rchi2(4)
gen rn2=runiform(1,2)
*generate explantory variables
gen x1=rn1-4
gen x2=rn2+3.5
*generate residuals 
gen rn3=rnormal(0,5)
gen rn4=rnormal(0, sqrt(5))
gen rn5=runiform()
gen u=0
replace u=rn3 if rn5<=0.3
replace u=rn4 if rn5>0.3
*generate the dependent variable
gen y=1.3*x1+0.7*x2+0.5*u

**ols
reg y x1 x2

*delta method
nlcom _b[x1]+_b[x2]^2, post

*bootstrap
bootstrap gamma=(_b[x1]+_b[x2]^2), reps(25) : regress y x1 x2
bootstrap gamma=(_b[x1]+_b[x2]^2), reps(200) : regress y x1 x2

*test a given hypothesis
bootstrap gamma=(_b[x1]+_b[x2]^2), reps(999): regress y x1 x2 
test gamma=1


use "C:\Users\azhao02\OneDrive - Syracuse University\pa-pam6090.dta" , clear

***OLS only on tobacco
reg dbirwt tobacco

***OLS on all explantory variables
reg dbirwt tobacco dmage dmage2 dmeduc dmar mblack mhispan motherr foreignb dfage dfeduc fblack fhispan fotherr alcohol drink tripre0 tripre2 tripre3 nprevist  adequac2 adequac3   first dlivord disllb  pre4000 plural  diabete  anemia  cardiac  chyper


***kernel
help kdensity
* use the default kernel function and the optimal bandwidth chosen by STATA
kdensity dbirwt

* gaussian kernel and bandwidth of 5 (small)
kdensity dbirwt, kernel(gau) bwidth(5)
*gaussian kernel and bandwidth of 100 (large)
kdensity dbirwt, kernel(gau) bwidth(100)
**Thus, the default function with optimal bandwidth is the best estimator. 

**two graphs
twoway kdensity dbirwt if tobacco==1, color(green)  || kdensity dbirwt if tobacco==0


**with interaction terms
reg dbirwt tobacco dmage dmage2 dmeduc dmeduc#dmeduc dmar mblack mblack#dmeduc mhispan mhispan#dmeduc motherr foreignb dfage dfeduc fblack fhispan fotherr alcohol drink alcohol#tripre0 tripre0 tripre2 tripre3 nprevist adequac2 adequac3 first dlivord disllb pre4000 plural diabete anemia cardiac chyper



***kernel regression
kernreg1 tobacco dmeduc,  kercode(3) npoint(45)
tab cigar
kernreg1 cigar dmeduc if cigar>0 & cigar<15,  kercode(3) npoint(45)

log close