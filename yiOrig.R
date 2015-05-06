install.packages("BayesFactor")
library('BayesFactor')

dat=read.csv('cleansample_4_27_15.csv',head=T)
dat[dat=='#NULL!']='.'


colnames(dat)=c('id','sex','age8','IQ1','attn3','ef1','ef5','math7','frac8','frac9','WNL1','WNL6','frac_add7','frac_cpt7','numset1','numset6')

set8=data.frame(dat$frac8,dat$sex,dat$age8,dat$IQ1,dat$attn3,dat$WNL1,dat$WNL6,dat$frac_add7,dat$frac_cpt7,dat$math7,dat$ef1,dat$ef5,dat$numset1,dat$numset6)
colnames(set8)=c('frac8','sex','age8','IQ1','attn3','WNL1','WNL6','frac_add7','frac_cpt7','math7','ef1','ef5','numset1','numset6')

set9=data.frame(dat$frac9,dat$sex,dat$age8,dat$IQ1,dat$attn3,dat$WNL1,dat$WNL6,dat$frac_add7,dat$frac_cpt7,dat$math7,dat$ef1,dat$ef5,dat$numset1,dat$numset6)
colnames(set9)=c('frac9','sex','age8','IQ1','attn3','WNL1','WNL6','frac_add7','frac_cpt7','math7','ef1','ef5','numset1','numset6')

dif=dat$frac9-dat$frac8
setDif=data.frame(dif,dat$sex,dat$age8,dat$IQ1,dat$attn3,dat$WNL1,dat$WNL6,dat$frac_add7,dat$frac_cpt7,dat$math7,dat$ef1,dat$ef5,dat$numset1,dat$numset6)
colnames(setDif)=c('dif','sex','age8','IQ1','attn3','WNL1','WNL6','frac_add7','frac_cpt7','math7','ef1','ef5','numset1','numset6')


reg8=summary(lm(frac8~.,data=set8))
ans8=sort(regressionBF(frac8~.,data=set8))
reg9=summary(lm(frac9~.,data=set9))
ans9=sort(regressionBF(frac9~.,data=set9))
regDif=summary(lm(dif~.,data=setDif))
ansDif=sort(regressionBF(dif~.,data=setDif))



#8th 
full=(names(ans8@numerator)=="IQ1 + WNL1 + frac_cpt7 + math7")
win_IQ=(names(ans8@numerator)=="WNL1 + frac_cpt7 + math7")
win_WNL=(names(ans8@numerator)=="IQ1 + frac_cpt7 + math7")
win_cpt=(names(ans8@numerator)=="IQ1 + WNL1 + math7")
win_math7=(names(ans8@numerator)=="IQ1 + WNL1 + frac_cpt7")
win_IQWNL=(names(ans8@numerator)=="frac_cpt7 + math7")

#BF
ans8[full]/ans8[win_IQ]
ans8[full | win_IQ | win_WNL|win_cpt|win_math7|win_IQWNL]


#9th
win=(names(ans9@numerator)=="IQ1 + WNL6 + frac_add7 + frac_cpt7 + ef1")
win_IQ1=(names(ans9@numerator)=="WNL6 + frac_add7 + frac_cpt7 + ef1")
win_WNL6=(names(ans9@numerator)=="IQ1 + frac_add7 + frac_cpt7 + ef1")
win_add7=(names(ans9@numerator)=="IQ1 + WNL6 + frac_cpt7 + ef1")
win_cpt=(names(ans9@numerator)=="IQ1 + WNL6 + frac_add7 + ef1")
win_ef1=(names(ans9@numerator)=="IQ1 + WNL6 + frac_add7 + frac_cpt7")

ans9[ win | win_IQ1 | win_add7 | win_ef1 | win_WNL6 | win_cpt]

#Dif

winmath=(names(ansDif@numerator)=="math7")
winIQ=(names(ansDif@numerator)=="IQ1")
winattn=(names(ansDif@numerator)=="attn3")
winCE1=(names(ansDif@numerator)=="ef1")
winCE5=(names(ansDif@numerator)=="ef5")
winnumset1=(names(ansDif@numerator)=="numset1")
winnumset6=(names(ansDif@numerator)=="numset6")
winsex=(names(ansDif@numerator)=="sex")
winfracc=(names(ansDif@numerator)=="frac_cpt7")
winfraca=(names(ansDif@numerator)=="frac_add7")
winage=(names(ansDif@numerator)=="age8")
winWNL1=(names(ansDif@numerator)=="WNL1")
winWNL6=(names(ansDif@numerator)=="WNL6")

ansDif[winmath|winIQ|winattn|winCE1|winCE5|winnumset1|winnumset6|winsex|winfracc|winfraca|winage|winWNL1|winWNL6]
