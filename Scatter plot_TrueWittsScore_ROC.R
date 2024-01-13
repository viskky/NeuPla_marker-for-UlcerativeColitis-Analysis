#Figure 2A
stripchart(df$NeuPla~df$TRUELOVE,data=df,vertical = T, method="jitter",pch=20,col=c(1),ylab="NeuPla Ratio",xlab='Truelove-Witts score')
m=tapply(df$NeuPla,df$TRUELOVE,mean)
x=1:4
segments(x-0.2,m,x+0.2,m,lwd=2)


#Figure 3A
Group=factor(df$Clinical_Activity,levels=c("REMISION","ACTIVA")) #The level to indicate the case and control
library(pROC)
roc1=roc(Group, df$Fecal_calprotectin)
plot(roc1)
roc1
roc2=roc(Group, df$CRP)
lines(roc2,col=2)
roc3=roc(Group, df$Albumin)
lines(roc3,col=3)
roc4=roc(Group, df$ESR)
lines(roc4,col=4)
roc5=roc(Group,df$NeuPla)
lines(roc5,col=5)
roc6=roc(Group,df$NLR)
lines(roc6,col=6)
legend('bottomright',c('Fecal_calprotectin','CRP','Albumin','ESR','NeuPla','NLR'),col = 1:6,lty=1,lwd=2,bty='n')
