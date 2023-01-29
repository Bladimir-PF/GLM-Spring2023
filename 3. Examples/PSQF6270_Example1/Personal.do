global filesave "\\Client\C$\Users\gbpadilla\OneDrive - University of Iowa\PhD portfolio\8. Teaching Assistant\3. TA Spring 2023 (Lesa Hoffman)\PSQF 6270 Section 0001 Generalized Linear Models\3. Examples\PSQF6270_Example1"

use "$filesave\STATA_Chapter2", clear

gen age85 = age-85
gen grip9 = grip-9

gen demnf = .
gen demnc = .

replace demnf = 0 if demgroup==1
replace demnc=0 if demgroup==1

replace demnf = 1 if demgroup==2
replace demnc=0 if demgroup==2

replace demnf = 0 if demgroup==3
replace demnc=1 if demgroup==3

gen demf = 0
gen demc = 0

replace demf = 1 if demgroup == 2
replace demc = 1 if demgroup == 3

tab demnf demf
tab demnc demc

drop demf demc

label variable age85 "age85: Age in Years (0=85)"
 label variable grip9 "grip9: Grip Strength in Pounds (0=9)"
 label variable sexmw "sexmw: Sex (0=Men, 1=Women)"
 label variable demnf "demnf: Dementia Predictor for None=0 vs Future=1"
 label variable demnc "demnc: Dementia Predictor for None=0 vs Current=1"
 label variable cognition "cognition: Cognition Outcome"
 label variable demgroup "demgroup: Dementia Group 1N 2F 3C"
 
 egen nmiss = rowmiss (cognition age grip sexmw demgroup)
 
 drop if nmiss > 0
 
 format cognition age grip sexmw demnf demnc %4.3f
 summarize cognition age grip sexmw demnf demnc, format
 summarize cognition age grip sexmw demnf demnc, detail
 
 tabulate sexmw demgroup
 
 /// Regression
 
 regress cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc, level(95)
 lincom c.demnf*-1 + c.demnc*1
 
 test (c.age85 = 0) (c.grip9=0)(c.sexmw=0)(c.demnf=0)(c.demnc=0) 
 
 regress cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc c.age85#c.grip9, level(95)
 
lincom c.demnf*-1 + c.demnc*1 
 test (c.age85=0)(grip9=0)(c.age85#c.grip9)
 
 lincom c.age85*1 + c.age85#c.grip9*-3
 lincom c.age85*1 + c.age85#c.grip9*0
 lincom c.age85*1 + c.age85#c.grip9*3
 
 margins, at (c.grip9 = (-3(3)3)) dydx (c.age85) vsquish

margins, at (c.age85 = (-5(5)5)) dydx (c.grip9) vsquish


regress cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc c.age85#c.grip9 ///
						c.sexmw#c.demnf c.sexmw#c.demnc, level(95)

test (c.sexmw#c.demnf)(c.sexmw#c.demnc)











































