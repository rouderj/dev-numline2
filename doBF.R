#Jeff Rouder, 5/15
library('BayesFactor')

dat=read.csv('data.csv',head=T)
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

bf=list(ans8,ans9,ansDif)
save(file="bf.RData",bf)
