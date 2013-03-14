###############  BIOS DESTINIE  ******************

#Il reste une question sur l'age; est-ce qu'il faut le prendre <0 ou <=0 ? 


# Extraction des biographie individuelle (liens familiaux, statuts, salaire) de la base de Destinie 2009. 
# Programmes conjoint et statut dans P:\Retraites\Destinie\VersR

# source("P:/Retraites/PENSIPP 0.0/Modèle/Outils/OutilsRetraite/OutilsMS.R")
# source("P:/Retraites/PENSIPP 0.0/Modèle/Outils/OutilsRetraite/OutilsDestinie.R")

#setwd("C:/Users/a.eidelman/Desktop/GenIPP_Pyth/liam/Patrimoine/data/")
#setwd("M:/Myliam2/Patrimoine/data/")
setwd("T:/Myliam2/Genebios/data/")
setwd("C:/Myliam2/Genebios/data/")
debut        <- 1900
t_deb        <- 109
t_fin        <- 160
# Lecture donnÃ©es d'Ã©tat initial (NB : le tableau intermÃ©diaire buf est lu sous forme de dataframe 
# et immÃ©diatement converti en type matrix, ce qui est nÃ©cessaire pour l'extraction de la 
# sous-matrice des identifiants des enfants)
buf                          <- read.csv2("init/pop.csv",header=FALSE)
dimnames(buf)[[2]] <- c("id","period","age","sexe","findet","pere","mere","n_enf","enf1","enf2","enf3","enf4","enf5","enf6")
#on retire les enf puisque seules les variables pere et mere vont nous être utiles
person_sta <- subset(buf, select=-c(n_enf,enf1,enf2,enf3,enf4,enf5,enf6))
n <- nrow(buf)
person_sta[,2] <- rep.int(2009,n)
#on passe de la date de naissance à l'age
person_sta[,3] <- 2009 - (1900+person_sta[,3])



#write.table(person_sta,file="person_sta.csv",sep=",",row.names=F)



# Lecture carriÃ¨res et biographies matrimoniales
library(reshape)
buf                          <- read.csv2("init/biosta.csv",header=FALSE)
n                            <- nrow(buf)
print (c(n," individus lus dans BioSta.csv"))
buf = subset(buf, ! as.vector(buf$V1)  %in% as.vector(person_sta[which(person_sta$age<0),]$id))
# Decede quands plus de statut
buf[is.na(buf) | (buf<0) ]<- 0
colnames(buf)<-c("id",seq(debut,debut+t_fin))
statut=melt(buf[,],id="id")
colnames(statut)[2:3] <- c("period","statut")


buf                          <- read.csv2("init/biosal4.csv",header=FALSE)
n                            <- nrow(buf)
print (c(n," individus lus dans BioSal4.csv"))
buf = subset(buf, ! as.vector(buf$V1)  %in% as.vector(person_sta[which(person_sta$age<0),]$id))
# =0 quands plus de statut
buf[is.na(buf)]<- 0
colnames(buf)<-c("id",seq(debut,debut+t_fin))
salaires=melt(buf[,],id="id")
colnames(salaires)[2:3] <- c("period","salaires")


buf                        <- read.csv2("init/biomat4.csv",header=FALSE)
n                          <- nrow(buf)
print (c(n," individus lus dans BioMat4.csv"))
buf = subset(buf, ! as.vector(buf$V1)  %in% as.vector(person_sta[which(person_sta$age<0),]$id))
buf[is.na(buf)]<- 0



colnames(buf)<-c("id",seq(debut,debut+t_fin))
conjoint=melt(buf[,],id="id")
colnames(conjoint)[2:3] <- c("period","conjoint")



person=cbind(conjoint,salaires,statut)[,c("id","period","conjoint","salaires","statut")]
rm(buf,conjoint,salaires,statut)
gc() 
write.csv(person,file="person_2060.csv",row.names=F)
PersonPast = subset(person, ! period %in% c(2009+seq(2010:2060)))

write.csv(PersonPast,file="person.csv",row.names=F)

####### table 2009 ######
person2009 <- subset(person, period==2009)
person_sta = subset(person_sta, ! as.vector(person_sta$id)  %in% as.vector(person_sta[which(person_sta$age<0),]$id))
person2009 <- merge(person2009,person_sta, by=c("id","period"),sort = F)
person2009 <- subset(person2009, age>0)

# on invente des ménages, à l'instinct. 
person2009$res<-seq(1:nrow(person2009))
person2009$res[which(person2009$conjoint < person2009$id & person2009$conjoint >0 )] <- person2009$res[person2009$conjoint[which(person2009$conjoint < person2009$id & person2009$conjoint >0 )]]
person2009$res[which(person2009$mere>0 & person2009$conjoint<0 & person2009$age <25)]<-person2009$res[person2009$mere[which(person2009$mere>0 & person2009$conjoint<0  & person2009$age <25)]]
#et puisqu'on est là, on écrit aussi un table menage dont on a besoin : 

men  <- as.matrix(unique(person2009$res))
pond <- runif(nrow(men))*20000+50 
men  <- cbind(men,rep.int(2009,nrow(men)))
colnames(men) <- c("id","period","pond")
write.csv(men,file="menage2009.csv",row.names=F)
write.csv(person2009,file="person2009.csv",row.names=F)



