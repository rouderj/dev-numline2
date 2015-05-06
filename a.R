library('BayesFactor')
load('bf.RData')
ans8=bf[[1]]
ans9=bf[[2]]
ansDif=bf[[3]]

top8=head(ans8,14)
top9=head(ans9,35) 
topDif=head(ansDif,4) 
prior9 = newPriorOdds(top9, type = "equal")
post9 = prior9 * top9
post.prob9 = as.BFprobability(post9)

extractBF(top9/top9[1])[,1:2]             

#best 5
IQ1 + WNL6 + frac_add7 + frac_cpt7 + ef1                  1.0000000 0.000000e+00
IQ1 + WNL6 + frac_cpt7 + ef5 + numset1                    0.8912493 4.900876e-08
IQ1 + WNL6 + frac_add7 + frac_cpt7 + ef5                  0.7964367 4.780000e-08
IQ1 + WNL1 + WNL6 + frac_cpt7 + math7                     0.6637877 4.604925e-08
WNL1 + WNL6 + frac_cpt7 + math7 + ef5                     0.6028195 4.521898e-08
WNL6 + frac_cpt7 + math7 + ef5 + numset1                  0.5716441 4.478772e-08
IQ1 + WNL6 + frac_cpt7 + math7 + numset1                  0.4920271 4.366432e-08
---------------------------



#best 4
WNL6 + frac_cpt7 + ef5 + numset1                          0.4648663 2.938088e-05
IQ1 + WNL6 + frac_cpt7 + numset1                          0.3909489 2.961897e-05
WNL1 + WNL6 + frac_cpt7 + math7                           0.3901336 2.962251e-05


OK, so it seems that frac_cpt7 and WNL6 are needed.  Yi, how bad is the best 5 and the best 4 minus each of these?

frac_cpt7

best5=(names(ans9@numerator)=="IQ1 + WNL6 + frac_add7 + frac_cpt7 + ef1")
best4=(names(ans9@numerator)=="WNL6 + frac_cpt7 + ef5 + numset1")

b5wWNL6=(names(ans9@numerator)=="IQ1 + frac_add7 + frac_cpt7 + ef1")
b5wfrac_cpt7=(names(ans9@numerator)=="IQ1 + WNL6 + frac_add7 + ef1")
b4wWNL6=(names(ans9@numerator)=="frac_cpt7 + ef5 + numset1")
b4wfrac_cpt7=(names(ans9@numerator)=="WNL6 + ef5 + numset1")

#we have a great story!
ans9[b5wWNL6]/ans9[best5]
ans9[b4wWNL6]/ans9[best4]
ans9[b5wfrac_cpt7]/ans9[best5]
ans9[b4wfrac_cpt7]/ans9[best4]

top2=(names(ans9@numerator)=="WNL6 + frac_cpt7")
ans9[top2]/ans9[best5]

--------------------------
Improvements for Richard---
a. add numbers of covariates
b. add prior odds weight by covariates
c. add averaging by covariate (dont ave models less than x from top)
