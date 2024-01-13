Group=factor(df$Clinical_Activity,levels=c("REMISION","ACTIVA")) #for clinical activity
roc5=roc(Group,df$NeuPla)
data.frame(roc5$thresholds,roc5$sensitivities,roc5$specificities) #select the values for cut off of biomarker stated
coords(roc5,'best',best.method='youden') #to determine best cut off,specificity & sensitivity

Endo=factor(ifelse(df$Endoscopy==1,0,1)) #For Endoscopy activity
roc5=roc(Endo,df$NeuPla)
data.frame(roc5$thresholds,roc5$sensitivities,roc5$specificities)

#CRP
Group=factor(df$Clinical_Activity,levels=c("REMISION","ACTIVA")) #for clinical activity
roc5=roc(Group,df$CRP)
data.frame(roc5$thresholds,roc5$sensitivities,roc5$specificities) #select the values for cut off of biomarker stated
coords(roc5,'best',best.method='youden') #to determine best cut off,specificity & sensitivity

Endo=factor(ifelse(df$Endoscopy==1,0,1)) #For Endoscopy activity
roc5=roc(Endo,df$CRP)
data.frame(roc5$thresholds,roc5$sensitivities,roc5$specificities)
