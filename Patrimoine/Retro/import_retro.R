EMMENAG Année d'emménagement dans la résidence principale
####### variable utile pour remonter le passé des individus
JEQUIT, FORSEUL # depart faoyer parental

# lecture des données de l'enquêtre patrimoine
# selection des valeurs utiles pour le matching.
# construction des valeurs dans certains cas.


rm(list = ls()) # Clean the workspace
gc()            # Garbage collecting (for memory efficiency)

user <- "IPP_pers"

## AE
if (user=="AE_port"){
  chem_patr <-"M:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"M:/Myliam2/Patrimoine/lien_parent_enfant/"
}
if (user=="IPP_pers"){
  chem_patr <-"M:/Patrimoine/EP 2009-10/Stata/"
  dest <-"C:/Myliam2/Patrimoine/lien_parent_enfant/"
}
if (user=="IPP"){
  chem_patr <-"M:/Patrimoine/EP 2009-10/Stata/"
}

if (user=="IFS"){
  chem_patr <-"T:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"T:/Myliam2/Patrimoine/lien_parent_enfant/"
}


taille    <- 35729
taille.m   <- 15006

library(foreign)
men   <- read.dta(paste0(chem_patr,"menage.dta"))
ind   <- read.dta(paste0(chem_patr,"Individu.dta"))


################################ correction sur la base   ################################
# le pourquoi de ces corrections se trouve dans les programmes de verif

ind$cydeb1 <- ind$prodep  #= pmax(anfinetu,jeactif)
ind$modif  <- "" #on crée une varaible MODIF qui retient que c'est un ménage qui a subi une modif.
ind$cydeb1[c(6723,7137,10641,21847,30072,31545,33382)] <- ind$anais[c(6723,7137,10641,21847,30072,31545,33382)]+20
ind$cydeb1[15207] <- 1963 ; ind$cydeb1[27801] <- 1999
ind$modif[c(15207,27801,6723,7137,10641,21847,30072,31545,33382)] <- "cydeb1 manq"
ind$cyact3[10834] <- "04"
ind$cyact2[23585] <- "11"
ind$cyact3[27817] <- "05"
ind$modif[c(10834,23585,27817)] <- "cyact manq"

moulinette <- function(vecteur) { #fonction qui décale tous les événements de 1 vers la gauche, utile quand il y a un trou
  for (cas in 1:15) {
    ind[vecteur,c(paste0("cyact",cas),paste0("cydeb",cas),paste0("cycaus",cas),paste0("cytpto",cas) )    ]  <<- 
      ind[vecteur, c(paste0("cyact",cas+1),paste0("cydeb",cas+1),paste0("cycaus",cas+1),paste0("cytpto",cas+1))]
    print(paste0('on traite le cas ',cas,' sur 15'))
  }
  ind$modif[vecteur] <<- "decal act"
}
moulinette( c(8298,which(ind$cyact2!="" & ind$cyact1=="" & (ind$cydeb1==ind$cydeb2 | ind$cydeb1>ind$cydeb2 | ind$cydeb1==(ind$cydeb2-1) )))) 

#toute les activités
list.prob.date <- which(ind$cyact2!="" & ind$cyact1==""  & !(ind$cydeb1==ind$cydeb2 | ind$cydeb1>ind$cydeb2 | ind$cydeb1==(ind$cydeb2-1) ))
#on va les mettre à 04 sauf si, leur cyact2==04 et là on met à chomage. 
ind$cyact1[intersect(list.prob.date, which(ind$cyact2!="04"))] <- "04"
ind$cyact1[intersect(list.prob.date, which(ind$cyact2=="04"))] <- "02"
ind$modif[list.prob.date] <- "cyact1 manq"
ind$modif[which(is.na(ind$cydeb1)& (ind$cyact1!=""|ind$cyact2!=""))]  <- "jeact ou anfinetu manq"
ind$cydeb1[which(is.na(ind$cydeb1)& (ind$cyact1!=""|ind$cyact2!=""))] <- pmax(ind$jeactif[which(is.na(ind$cydeb1)&(ind$cyact1!=""|ind$cyact2!=""))],
                                                                              ind$anfinetu[which(is.na(ind$cydeb1)&(ind$cyact1!=""|ind$cyact2!=""))], na.rm = TRUE)
#quand l'ordre des dates n'est pas le bon on fait l'hypothèse que c'est la première date entre anfinetu et jeactif qu'il faut prendre en non pas l'autre
list.prob.deb <- which(ind$cydeb1>ind$cydeb2)
ind$cydeb1[list.prob.deb]  <- pmin(ind$anfinetu[list.prob.deb],ind$jeactif[list.prob.deb])
ind$cydeb1[which(ind$cyact1=="")] <- NA

rm(moulinette,list.prob.date,list.prob.deb,chem_patr)
gc()




################################  utiles   ################################

## on modifie les identifiants ménage pour que ça ait une tête sympa; on changera éventuellement à la fin
# pour que le numéro soient simplifiés mais pour l'instant cette forme peut être plus évidente en cas de merge
# (encore que si on a une table de passage...)##
ind$res <- sprintf("%06d",match(ind$identmen,men$identmen))   #on laisse un zéro devant, toujours pratique 
men$res <- sprintf("%06d",rep(1:length(unique(men$identmen)))) # en cas de duplication, etc
ind$id  <- paste(ind$res,ind$noi)


# on retire identind qui ne sert à rien, et prodep qu'on a amélioré dans cydeb1 et toutes les variables construite ou inutile
ind <- subset(ind, select = - c(prodep,t5age))
# on retire aussi toute les variables concernant pr et cj puisqu'on peut
# recuperer celle qu'on veut plus tard
names(men)
grep("^pr", names(men))
ToRemove = names(men)[grep("pr$", names(men))] 
ToRemove <- ToRemove[which(! ToRemove %in% c("indepr","r_dcpr","r_detpr"))]
ToRemove = c(ToRemove, names(men)[grep("cj$", names(men))] )
men = subset(men, select= names(men)[which(! names(men) %in% ToRemove)] )
# dans la meme veine, on retire les variables diplomes
ToRemove = names(men)[grep("^diplom", names(men))] 
men = subset(men, select= names(men)[which(! names(men) %in% ToRemove)] )



attach(men)
attach(ind) #note bien : l'ordre est important entre les deux attach, on veut que identmen renvoie aux identmen de ind.

#############################   Travail sur les carrière #######################################

min(cydeb1[!is.na(cydeb1)])

carriere <- matrix("",length(which(!is.na(cydeb1))),2010-1900+3 )
colnames(carriere) <- colnames(carriere,do.NULL=FALSE,prefix="t")
carriere[,2010-1900+1] <- noi[which(!is.na(cydeb1))]
carriere[,2010-1900+2] <- identmen[which(!is.na(cydeb1))]
carriere[,2010-1900+3] <- identind[which(!is.na(cydeb1))]
colnames(carriere)[2010-1900+1] <- "noi"
colnames(carriere)[2010-1900+2] <- "res" 
colnames(carriere)[2010-1900+3] <- "id" 


list.actif <- which(!is.na(cydeb1))
for (i in 1:length(list.actif)) {
  indiv <- list.actif[i]
  for (cas in c(1:15))  { 
    if (is.na(get(paste0("cydeb",cas))[indiv])) {break}
    else {  
      if (is.na(get(paste0("cydeb",cas+1))[indiv])) {
        carriere[i, (get(paste0("cydeb",cas))[indiv]-1900):(2010-1900)] <- get(paste0("cyact",cas))[indiv]
      }
      else {
        carriere[i, (get(paste0("cydeb",cas))[indiv]-1900):(get(paste0("cydeb",cas+1))[indiv]-1900)] <- get(paste0("cyact",cas))[indiv]
      }
    }
  }
  if(!is.na(cydeb16[indiv])){
    carriere[i,(cydeb16[indiv] -1900):(2010-1900)] <- cyact16[indiv]
  }
}
carriere  <- carriere[,(min(cydeb1[!is.na(cydeb1)]-1900)):(2010-1900+3)] #tout ça pour avoir les bon noms de variables facilement...pas ouf



#############################   Travail sur salaire à t ########################################################
#on veut des revenus net 2001
tx.sal <- -15 - 5.1  #le -5.1 est la CSG déductible qu'il faut retirer du revenu imposable, le -15 est l'evolution moyenne de revenus entre 2001 (EIR) et 2010 (Patrimoine)
tx.ret <- -15 - 4.2  
tx.cho <- -15 - 3.8# mais très mal fait, il faudra absoluement y revenir.
tx.remp.ret <- 54/100 # d'après SimIPP
tx.remp.cho <- 70/100 # d'après DidIPP
ind$zrev   <- ind$zsalaires_i*(1+tx.sal/100)+ zretraites_i*(1+tx.ret/100)/tx.remp.ret + zchomage_i*(1+tx.cho/100)/tx.remp.ret
ind$zrev   <- ind$zrev*(1+tx.sal/100)


############## construction des variables sépecifiquement pour le matching #############
tranche.sal      <- c(-1,0,2000,5000,7500,10000,12500,15000,10000*rep(2:3),45000,60000,max(ind$zrev))
ind$TrancheRev   <- cut(ind$zrev,breaks= tranche.sal,labels=rep(0:11)) #table(cut(ind$zrev,breaks= tranche.sal,labels=rep(1:11)))


# hist(zsalaires_i[zsalaires_i>0 & zsalaires_i<60000],100,main="distribution des salaires individuels",ylim=c(0,700))
# hist(ind$zrev[ind$zrev>0 & ind$zrev<60000],100,main="distribution des revenus individuels",ylim=c(0,700))

#toutes les modalités qu'on reconstitue pas dans l'EIC, on les mets à 15
carriere2 <- carriere
carriere2[carriere2=="01" | carriere2=="06" | carriere2=="07" | carriere2=="09" | carriere2=="13"] <- "15"
carriere2[,"noi"] <- carriere[,"noi"] #on remet le noi qu'on a écrasé au dessus

#age olympique (modulo 4 quoi)
ind$anais2 <- cut(ind$anais,breaks=1904+4*rep(0:27), labels=1902+4*rep(1:27))
ind$anais2 <- as.numeric(levels(ind$anais2))[ind$anais2]

# simplifie le nom de pcs_act et pcs_ant en pcs
names(ind)[which(names(ind)=="pcs_act")] <- "pcs2" #note : on vérifie qu'à chaque fois seul l'un des deux pcs est rempli.
ind$pcs2 <- pmax(ind$pcs2,ind$pcs_ant)
ind$pcs2[which(ind$pcs2=="00")] <- ""
ind$pcs1 <- substr(ind$pcs2,1,1)

# length(which(zsalaires_i>50000))
# 
# #il faut travailler pour obtenir un revenu individuel
# nb_sal   <- numeric(taille.m)
# for (i in 1:taillem) {
#   nb_sal[i]  <- length( which(RSAL_I==1 & identmen==i ))
# }
# #on verifie qu'on a nb_sal=0 et RSAL=2 simultanément 
# setdiff( which(RSAL==2) , which(nb_sal==0)) # ok
# table(nb_sal)
# table(RIND)
# nb_ind   <- numeric(taillem)
# for (i in 1:taillem) {
#   nb_ind[i]  <- length( which(RIND_I==1 & identmen==i ))
# }
# setdiff( which(RIND==2) , which(nb_ind==0)) # ok



#############################   Synthèse des données   ################################################
detach(ind) ; detach(men) ; attach(ind)
P_match <- cbind(noi,identmen,identind,sexe,anais,anais2,etamatri,statut,enfant,nb_enf,pcs1,pcs2,zrev,TrancheRev,pond,retrai)


save(P_match,carriere,carriere2, file="P:/GenIPP/Programmes/1Données_initiales/Matching/pat_match.RData")

# peut-être supprimer MNAIS, si on le garde, il faut tout faire en mois, pourqu'il n'y 
#ait pas plus de distance entre décembre et janvier (à cause du changement d'année) qu'entre mars et avril par exemple.
