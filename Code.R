#################################################################################################################################
##############################ADVANCED ECONOMETRICS OF QUALITATIVE DATA - PROJECT: BRAND CHOICE MODELS###########################
#################################################################################################################################


#CHANGE THIS FOR EVERY USER
dir<-"D:/Users/acombess/Dropbox/ENSAE/Semestre 1/Advanced econometrics of qualitative data/Projet Advanced Econometrics/"
setwd(dir)

lib<-c("plm","mlogit", "mnlogit", "mstate","pbapply")
sapply(lib, require, character.only = TRUE, quietly=T)

features<-c("hhold_id","brand","i_obs","m_obs","hhold_income","hhold_members",
            "hhold_male_educ","hhold_female_educ","erim_market","store_id","n_wk", "ind_feature",
            "ind_display","Coupon_val","Coupon_av","P.1","P.2","P.3","P.4","ft.1","ft.2","ft.3","ft.4",
            "dp.1","dp.2","dp.3","dp.4","cp_av.1","cp_av.2","cp_av.3","cp_av.4")
totalclasses<-c("factor","factor",rep("numeric",2),"factor","numeric",rep("factor",4),"numeric",rep("factor",2),
                rep("numeric",6),rep("numeric",8),rep("numeric",4))
df_ketchup <- read.table("./ketchup_est_m.txt",col.names = features, colClasses = totalclasses)
df_peanut <- read.table("./pbut_est_m.txt", col.names = features, colClasses = totalclasses)

#################DATA PREPARATION#####################################

#Changement de format pour mettre en place le logit
#Pour chaque ligne initiale, mlogit.data crée 5 lignes, une par alternative pour un choix. 
#le choix d'une marque est donc indiqué par :
#L'index des choix, qui est constitué de la colonne chid, donne choix 1 pour la 1ere semaine etc
#La colonne alt decrivant les 5 alternatives
#La colonne brand a la valeur TRUE en face de la ligne d'alt correspondand au choix du m?nage. 
#On ajoute les colonnes des variables alternative specific pour le non-achat:
length_ketchup<-nrow(df_ketchup)
P.0<-rep(0.0,length_ketchup)
ft.0<-rep(0.0,length_ketchup)
dp.0<-rep(0.0,length_ketchup)
cp_av.0<-rep(0.0,length_ketchup)
df_ketchup<-cbind(df_ketchup,P.0,ft.0,dp.0,cp_av.0)

# We compute the variable "gap" in order to add it later in the models we intend to implement
# [Ketchup] gaps computing
gap_ketchup<-append(df_ketchup[-1,11],0)-df_ketchup[,11]
gap_ketchup[gap_ketchup<0]=0
df_ketchup <- cbind(df_ketchup,gap_ketchup)
colnames(df_ketchup)[ncol(df_ketchup)]='gaps'

mlogit.ketchup<-mlogit.data(df_ketchup, choice='brand', shape='wide',
                            alt.levels=c(0,1,2,3,4),varying=16:35,sep=".",id="hhold_id")
mlogit.ketchup[,"alt"]<-as.factor(mlogit.ketchup[,"alt"])
mlogit.ketchup[,"ft"]<-as.factor(mlogit.ketchup[,"ft"])
mlogit.ketchup[,"dp"]<-as.factor(mlogit.ketchup[,"dp"])

length_peanut<-nrow(df_peanut)
P.0<-rep(0.0,length_peanut)
ft.0<-rep(0.0,length_peanut)
dp.0<-rep(0.0,length_peanut)
cp_av.0<-rep(0.0,length_peanut)
df_peanut<-cbind(df_peanut,P.0,ft.0,dp.0,cp_av.0)

# [Peanut] gaps computing
gap_peanut<-append(df_peanut[-1,11],0)-df_peanut[,11]
gap_peanut[gap_peanut<0]=0
df_peanut <- cbind(df_peanut,gap_peanut)
colnames(df_peanut)[ncol(df_peanut)]='gaps'

mlogit.peanut=mlogit.data(df_peanut, choice='brand', shape='wide',varying=16:35,sep=".",
                          alt.levels=c(0,1,2,3,4),id="hhold_id")
mlogit.peanut[,"alt"]<-as.factor(mlogit.peanut[,"alt"])
mlogit.peanut[,"ft"]<-as.factor(mlogit.peanut[,"ft"])
mlogit.peanut[,"dp"]<-as.factor(mlogit.peanut[,"dp"])



#################Descriptive statistics#################

# Distribution per brand for each category
brandshare_ketchup<-sort(table(df_ketchup$brand)[2:5],decreasing = T)
brandshare_peanut<-sort(table(df_peanut$brand)[2:5],decreasing = T)
pct_ketchup <- round(brandshare_ketchup/sum(brandshare_ketchup)*100,0)
pct_peanut <- round(brandshare_peanut/sum(brandshare_peanut)*100,0)

lbls_ketchup <- c("Heinz","Hunt's","Del Monte","Store Brand")
lbls_peanut <- c("Store Brand","JIF","Peter Pan","Skippy")
lbls_ketchup <- paste(lbls_ketchup, pct_ketchup,"%",sep=" ") # add percents to labels
lbls_peanut <- paste(lbls_peanut, pct_peanut,"%",sep=" ") # add percents to labels

par(mfrow = c(1, 2),mar=c(-2,-1,-1,-1))
pie(brandshare_ketchup,labels = lbls_ketchup,col=c("#70160D","#b82619","#BF3C30","#D47E75"),
    main="Ketchup",cex=0.8,pty='m')
pie(brandshare_peanut,labels = lbls_peanut,col=c("#917349","#C19961","#F2C586","#F5D7AD"),
    main="Peanut",cex=0.8,pty='m')

# No purchase duration distribution per household for ketchup
households.ids.ketchup <- unique(df_ketchup[,1])
spells.ketchup = aggregate(brand~hhold_id +i_obs+n_wk,data=mlogit.ketchup, function(x) return(which(x==T)-1))
b=lapply(households.ids.ketchup,function(x) spells.ketchup[spells.ketchup$hhold_id==x & spells.ketchup$brand!=0,3] )
nopurchase_durations.ketchup=lapply(b,function(x) return(x - append(0,x[1:length(x)-1])))
hist(unlist(nopurchase_durations.ketchup),100,xlab='weeks',main='[Ketchup] Overall distribution of no purchase durations')

mean.per.hhold=lapply(nopurchase_durations.ketchup,mean)
hist(unlist(mean.per.hhold),40,xlab='weeks',main = '[Ketchup] Mean duration no purchase per household')

# No purchase duration distribution per household for peanut butter
households.ids.peanut <- unique(df_peanut[,1])
spells.peanut = aggregate(brand~hhold_id +i_obs+n_wk,data=mlogit.peanut, function(x) return(which(x==T)-1))
b=lapply(households.ids.peanut,function(x) spells.peanut[spells.peanut$hhold_id==x & spells.peanut$brand!=0,3] )
nopurchase_durations.peanut=lapply(b,function(x) return(x - append(0,x[1:length(x)-1])))
par(mfrow = c(1, 2))
hist(unlist(nopurchase_durations.ketchup),100,xlab='weeks',main='[Ketchup]' )
hist(unlist(nopurchase_durations.peanut),100,xlab = 'weeks',main='[Peanut]')

## Box plot for each category
par(mfrow = c(1, 2))

boxplotketchup=boxplot(unlist(nopurchase_durations.ketchup),outline=F,horizontal = T)
boxplotpeanut=boxplot(unlist(nopurchase_durations.peanut),outline=F,horizontal = T)

mean.per.hhold.peanut=lapply(nopurchase_durations.peanut,mean)
hist(unlist(mean.per.hhold.peanut),40,xlab='weeks',main = '[Peanut] Mean duration no purchase per household')

# Number of observations per household for each category
par(mfrow=c(1,2))
obsPerhhold.ketchup= unique(mlogit.ketchup[,c(1,4)])
obsPerhhold.peanut= unique(mlogit.peanut[,c(1,4)])
hist(obsPerhhold.ketchup[,2],xlim=c(40,140),xlab="number of observations",main="[Ketchup]")
hist(obsPerhhold.peanut[,2],,xlab="number of observations",main="[Peanut]")



##################### Multinomial Logit ########################

# We need to erase the first 5 observations for each household on each category
# then we apply the Multinomial logit model

mlogit.ketchup.reduced=lapply(households.ids.ketchup,erase,mlogit.ketchup,5)

erase <- function(hhold_id,data,n){
  total = data[data$hhold_id==hhold_id,]
  if(total$m_obs >=10){
    total = total[-(1:5),]
  }
  return(total)
}

indexes.delete.ketchup=pblapply(households.ids.ketchup,indexestodelete,mlogit.ketchup,20)
mlogit.ketchup.reduced = mlogit.ketchup[-unlist(indexes.delete.ketchup),]

indexes.delete.peanut=pblapply(households.ids.peanut,indexestodelete,mlogit.peanut,10)
mlogit.peanut.reduced = mlogit.peanut[-unlist(indexes.delete.peanut),]

indexestodelete<-function(x,data,n){
  minIndex=min(which(data$hhold_id==x))
  maxIndex=max(which(data$hhold_id==x))
  mobs = data[minIndex,]$m_obs
  if(mobs >=n){
    offset= 5 * n -1 
    return(seq(minIndex,minIndex +offset) )
  }else{
    return(seq(minIndex,maxIndex))
  }
}

mlogit.ketchup.reduced[,"hhold_income"]<-as.numeric(mlogit.ketchup.reduced[,"hhold_income"])
mlogit.peanut.reduced[,"hhold_income"]<-as.numeric(mlogit.peanut.reduced[,"hhold_income"])

# Applying mutlinomial logit model

test_ketchup_reduced2 <- mlogit(brand ~  P + ft + dp +cp_av  | 
                                  hhold_income + hhold_members+gaps, 
                                data=mlogit.ketchup.reduced,reflevel = "0")
summary(test_ketchup_reduced2)

test_peanut_reduced2 <- mlogit(brand ~  P + ft + dp +cp_av  | 
                                 hhold_income + hhold_members+gaps, 
                               data=mlogit.peanut.reduced,reflevel = "0")
summary(test_peanut_reduced2)

stargazer(test_ketchup_reduced2,test_peanut_reduced2,report = "vc*")
stargazer(test_ketchup_reduced2,test_peanut_reduced2)

# Applying nested mutlinomial logit model

test_ketchup_reduced_nested <- mlogit(brand ~  P + ft + dp +cp_av  | 
                                        hhold_income + hhold_members+gaps, 
                                      data=mlogit.ketchup.reduced, 
                                      nests = list(nopurchase = c("0"),purchase = c("1","2","3","4")),
                                      un.nest.el = TRUE)
summary(test_ketchup_reduced_nested)

test_peanut_reduced_nested <- mlogit(brand ~  P + ft + dp +cp_av  | 
                                       hhold_income + hhold_members+gaps, 
                                     data=mlogit.peanut.reduced, 
                                     nests = list(nopurchase = c("0"),purchase = c("1","2","3","4")),
                                     un.nest.el = TRUE)
summary(test_peanut_reduced_nested)

stargazer(test_ketchup_reduced_nested,test_peanut_reduced_nested,report = "vc*")
stargazer(test_ketchup_reduced_nested,test_peanut_reduced_nested)
