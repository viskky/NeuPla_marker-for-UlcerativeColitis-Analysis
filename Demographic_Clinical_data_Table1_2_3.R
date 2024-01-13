df=read.csv(file.choose(), head=TRUE,sep=";",dec=",") # Read in data

#Define the categorical variables as factors:
df$Sex=factor(df$Sex)

# Drugs that the patients use
df$Mesalazine=factor(df$Mesalazine)
df$Estheroids=factor(df$Estheroids)
df$Budesonide=factor(df$Budesonide)
df$Biologics=factor(df$Biologics)
df$Azathioprine=factor(df$Azathioprine)

# Disease score variables

df$Endoscopy=factor(df$Endoscopy)
df$Pathology=factor(df$Pathology)
df$TRUELOVE=factor(df$TRUELOVE)
df$FULLMAYO=factor(df$FULLMAYO)
df$YAMA=factor(df$YAMA)
df$MONTREAL=factor(df$MONTREAL)

# Activity
df$Clinical_Activity=factor(df$Clinical_Activity)

#Table 1
my_table1=table(df$Sex) #for men and women
prop.table(my_table1) #proportion

mean(df$Age,na.rm = TRUE) #mean of age
sd(df$Age,na.rm = TRUE) #sd of age

my_table2=table(df$Clinical_Activity) #amt of activity
prop.table(my_table2) #proportion

my_table3=table(df$Mesalazine) #amt of patients treated and untreated with mesazaline
prop.table(my_table3) #proportion


#Table 2
tapply(df$Fecal_calprotectin,df$Clinical_Activity,mean,na.rm=TRUE) #mean
tapply(df$Fecal_calprotectin,df$Clinical_Activity,sd,na.rm=TRUE) #sd

tapply(df$CRP,df$Clinical_Activity,mean,na.rm=TRUE)
tapply(df$CRP,df$Clinical_Activity,sd,na.rm=TRUE) #sd

tapply(df$Leukocytes,df$Clinical_Activity,mean,na.rm=TRUE) #mean
tapply(df$Leukocytes,df$Clinical_Activity,sd,na.rm=TRUE) #sd

#Test for normality with Histogram
hist(df$CRP) #for all groups
hist(df$CRP[df$Clinical_Activity=='ACTIVA']) #per group
hist(df$CRP[df$Clinical_Activity=='REMISION']) #per group
wilcox.test(df$CRP~df$Clinical_Activity) #median is suitable

hist(df$PLATELET_COUNT) #for all groups
hist(df$PLATELET_COUNT[df$Clinical_Activity=='ACTIVA']) #per group
hist(df$PLATELET_COUNT[df$Clinical_Activity=='REMISION']) #per group
t.test(df$PLATELET_COUNT~df$Clinical_Activity) #mean is suitable because it is normally distributed


#Table 3
cor.test(df$NeuPla,df$Fecal_calprotectin,method = 'spearman')
df$Endoscopy=as.numeric(df$Endoscopy)
cor.test(df$NeuPla,df$Endoscopy,method = 'spearman') #convert Endoscopy to numerical variable using df$Endoscopy = as.numeric(df$Endoscopy)
df$Pathology=as.numeric(df$Pathology)
cor.test(df$NeuPla,df$Pathology,method = 'spearman')
df$TRUELOVE = as.numeric(df$TRUELOVE)
cor.test(df$NeuPla,df$TRUELOVE,method = 'spearman')


