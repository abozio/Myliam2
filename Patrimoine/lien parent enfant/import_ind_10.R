# lecture des données de l'enquêtre patrimoine, en particulier des variables individuelles permettant
# de simuler

rm(list = ls()) # Clean the workspace
gc()            # Garbage collecting (for memory efficiency)

user <- "AE_port"

## AE
if (user=="AE_port"){
  chem_patr <-"M:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"M:/Myliam2/Patrimoine/"
}
if (user=="IPP"){
  chem_patr <-"M:/Patrimoine/EP 2009-10/Stata/"
}
if (user=="IFS"){
  chem_patr <-"T:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"T:/Myliam2/Patrimoine/"
}


taille    <- 35729
taille.m   <- 15006

library(foreign)
men   <- read.dta(paste0(chem_patr,"menage.dta"))
ind   <- read.dta(paste0(chem_patr,"Individu.dta"))


################################ correction sur la base   ################################
# le pourquoi de ces corrections se trouve dans les programmes de verif dans la partie matching (hors de MyLiam pour 
# l'instant)

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

rm(moulinette,list.prob.date,list.prob.deb)



################################  on renomme   ################################

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

########  Liste des naissances des enfants  ######
# cette liste n'a pas besoin d'être faite, il faut juste le numéro du père et de la mère que l'on a pour les enfants à
# domicile, pour les autres, on va faire un matching. 






####### variable utile pour remonter le passé des individus
JEQUIT, FORSEUL # depart faoyer parental



#on a deux cas: les enfants cohabitant a) et les enfants décohabitant b). Ils sont enregistrés différemment. 
#pour les premiers, on regarde pour chacun qui est leur pere et qui est leur mere dans les tables parent1 et parent2
#ensuite dans une grosse étape, on crée la matrice enfant

#on récupère en même temps le nombre d'enfant
nb_enf  <- numeric(taille)
enfant <- matrix(0,taille,17,dimnames=(list( NULL,
            c("enf1","enf2","enf3","enf4","enf5","enf6","enf7","enf8","enf9","enf10","enf11","enf12","enf13","enf14","enf15","enf16","enf17"))))
#on s'arrete à 17 parce qu'on n'a pas plus dans l'enquête, si on en change il faut verifier 

## a) enfants cohabitant
parent1 <- numeric(taille)
parent2 <- numeric(taille)
for (i in which(enf==1)) {
  parent1[i] <- which(identmen==identmen[i] & lienpref=="00")
  if ( length(which(identmen==identmen[i] & lienpref=="01"))>0) { 
    parent2[i] <- which(identmen==identmen[i] & lienpref=="01")
  }
}



par.enf1 <- function(i) {
  if ( length(identmen==identmen[i] & lienpref=="00")>0) {parent1[i] <- which(identmen==identmen[i] & lienpref=="00")}
  if ( length(which(identmen==identmen[i] & lienpref=="01"))>0){  parent2[i] <- which(identmen==identmen[i] & lienpref=="01") }
}

for (i in which(enf==1)) {
  par.enf1(i)
}


for (i in which(enf==2)) {
  parent1[i] <-  which(identmen==identmen[i] & lienpref=="00")
}
for (i in which(enf==3)) {
  parent1[i] <-  which(identmen==identmen[i] & lienpref=="01")
}


#la fonction qui met les parents et qui sera utilisé ci-après pour les petits enfants
ajout.parent <- function() {
  if (length(ll)==1) {
    parent1[i] <<- ll
    if (length( which(identmen==identmen[i] & !enf %in% c(1,2,3) & ! lienpref %in% c("00","01") & noi != i ) ) >0 ) {
      parent2[i] <<- which(identmen==identmen[i] & !enf %in% c(1,2,3) & ! lienpref %in% c("00","01") & noi != i )[1]
    }
  }
}

for (i in which(lienpref=="21")) {
  #liste des enfants de la PR et donc des parents potentiels
  ll <- which(identmen==identmen[i] & enf %in% c(1,2,3))
  ajout.parent()
  if (is.na(parent1[i])) {
    if (mer1e[i] =="1" & per1e[i]=="1") {
      ll <- intersect(ll, which(couple==1) )
      ajout.parent()
    }
    else if (mer1e[i] =="1" & per1e[i]!="1") {
     ll <- intersect(ll, which(sexe==2))
     ajout.parent()
     ll <- ll[1]
     ajout.parent()
    }
    else if (mer1e[i]!="1" & per1e[i]=="1") {
      ll <- intersect(ll, which(sexe==1))
      ajout.parent()
      ll <- ll[1]
      ajout.parent()
    }
  }
}


# on fait aussi tourner ce qui suit parce qu'on a besoin du nombre d'enfant

for (i in which(anais<1995)) { #on ne tourne que sur les plus de 14 ans
  # on définit la liste des enfants hors ménage list
  list <- NULL
  if (lienpref[i]=="00") {
    for (k in 1:12) {
      if (get(paste0("hodln",k))[i] %in% c('1','2')) {
        list <- c(list,get(paste0("hodan",k))[i])
      }
    }
  }
  if (lienpref[i]=="01") {
    for (k in 1:12) {
      if (get(paste0("hodln",k))[i] %in% c('1','3')) {
        list <- c(list,get(paste0("hodan",k))[i])
      }
    }
  }
  
  # on a tous les pour calculer le nombre d enfant
  # on prend d'abord les enfants cohabitant
  l <-  union( anais[which(parent1==i)] ,  anais[which(parent2==i)]) #nombre d'enfant cohabitant avec i
  nb_enf[i] <-  length(l)+length(list)
  if (length(list)>0) {
  }
}
  

# recuperation des variables sur les enfants hors du domicile
# comme on aura besoin des infos, il faut aller les chercher


inf_pr = ind[which(ind$lienpref=="00"),c("id","res","sexe","anais","per1e","mer1e","cs42")]
inf_cj = ind[which(ind$lienpref=="01"),c("id","res","sexe","anais","per1e","mer1e","cs42")]
inf_pr$gpar = 2-( inf_pr$per1e %in% c("1","2") | inf_pr$mer1e %in% c("1","2") ) 
inf_cj$gpar = 2-( inf_cj$per1e %in% c("1","2") | inf_cj$mer1e %in% c("1","2") )
inf_pr = subset(inf_pr, select= c("id","res","sexe","anais","gpar","cs42"))
inf_cj = subset(inf_cj, select= c("id","res","sexe","anais","gpar","cs42"))
colnames(inf_pr) <- paste0(colnames(inf_pr),"pr")
colnames(inf_cj) <- paste0(colnames(inf_cj),"cj")

men = merge( men,inf_pr,by.x="res",by.y="respr")
men = merge( men,inf_cj,by.x="res",by.y="rescj",all=TRUE)

rm(inf_pr,inf_cj)



#TODO: gerer les valeurs manquante
#TODO: s'occuper de la precision sur les statuts
save_inf <- function(i){
  liste  <<- which(men[,paste0("hodln",i)] !="")
  
  hodln <- men[liste,paste0("hodln",i)]
  sexe <- character(length(liste))
  # info sur l'enfant
  sexe   <- men[liste,paste0("hodsex",i)]
  anais  = men[liste,paste0("hodan",i)]
  couple = men[liste,paste0("hodco",i)] #couple=1 et couple=2 devront etre groupes en couple=1
  couple[which(couple=="2")]<-"3"
  dip6   = men[liste,paste0("hodip",i)]
  nb_enf = men[liste,paste0("hodenf",i)]
  # son activite
  situa=  1*(men[liste,paste0("hodemp",i)]==1) +
         2*(men[liste,paste0("hodcho",i)]==3) +
         3*(men[liste,paste0("hodcho",i)]==3) +
         4*(men[liste,paste0("hodcho",i)]==1) +
         5*(men[liste,paste0("hodemp",i)]==2) +
         6*(men[liste,paste0("hodcho",i)]==2) +
         7*(men[liste,paste0("hodcho",i)]==4)
 # on verifie que hodcho est rempli seulement quand hodemp=3
  classif = ifelse( men[liste,paste0("hodpri",i)] %in% c("1","2","3","4"), 
                    men[liste,paste0("hodpri",i)],  
                    men[liste,paste0("hodniv",i)])

  
  # info sur les parents
  info_mere    <- matrix("",length(liste),5)
  info_pere    <- matrix("",length(liste),4)
  colnames(info_mere) <- c("mere","jemnais","gparmat","jemprof","jemact")
  colnames(info_pere) <- c("pere","jepnais","gparpat","jepprof")
  
  inf_pere <- function(enfants,pers) {
    if (pers == "pr") {
      info_pere[enfants,] <<- as.matrix(men[liste[enfants],c("idpr","anaispr","gparpr","cs42pr")])
    }
    if (pers == "cj") {
      info_pere[enfants,] <<- as.matrix(men[liste[enfants],c("idcj","anaiscj","gparcj","cs42cj")])
    }
  }
  inf_mere <- function(enfants,pers) {
    if (pers == "pr") {
      info_mere[enfants,1:4] <<- as.matrix(men[liste[enfants],c("idpr","anaispr","gparpr","cs42pr")])
    }
    if (pers == "cj") {
      info_mere[enfants,1:4] <<- as.matrix(men[liste[enfants],c("idcj","anaiscj","gparcj","cs42cj")])
    }
  }
  
  inf_pere( which(men$sexepr[liste]==1 & men[liste,paste0("hodln",i)]=="1"), "pr")
  inf_mere( which(men$sexepr[liste]==1 & men[liste,paste0("hodln",i)]=="1"), "cj")
  inf_pere( which(men$sexepr[liste]==2 & men[liste,paste0("hodln",i)]=="1"), "cj")
  inf_mere( which(men$sexepr[liste]==2 & men[liste,paste0("hodln",i)]=="1"), "pr")
  inf_pere( which(men$sexepr[liste]==1 & men[liste,paste0("hodln",i)]=="2"), "pr")
  inf_mere( which(men$sexepr[liste]==2 & men[liste,paste0("hodln",i)]=="2"), "pr")
  inf_pere( which(men$sexepr[liste]==1 & men[liste,paste0("hodln",i)]=="3"), "cj")
  inf_mere( which(men$sexepr[liste]==2 & men[liste,paste0("hodln",i)]=="3"), "cj")

  print(length(liste))
  to_match = rep.int(1,length(liste))
  ajout <- cbind(sexe,anais,couple,dip6,nb_enf,situa,classif,info_pere,info_mere, to_match,hodln)
}

enf_ailleurs <- save_inf(1)
for (k in 2:12) {
  enf_ailleurs <- rbind(enf_ailleurs,save_inf(k))
}





# on peut maintenant retirer toutes les variables concernant les enfants a l exterieur du menage
ToRemove = names(men)[grep("^hod", names(men))] 
men = subset(men, select= names(men)[which(! names(men) %in% ToRemove)] )



#### info sur les parents
cherche_parent <- as.matrix(subset(ind, per1e == "2" | mer1e == "2", 
                         select= c(id,sexe,anais,couple,dip14,situa,jemnais,gparmat,jemprof,jemact,
                                   jepnais,gparpat,jepprof,per1e,mer1e,jegrave_div,classif)))

# on supprime les infos quand on ne cherche pas ce parent
pas_pere <- which(cherche_parent[,"per1e"] %in% c("1","3","4") )
cherche_parent[pas_pere,c("jepnais","gparpat","jepprof")] <- NA
pas_mere <- which(cherche_parent[,"mer1e"] %in% c("1","3","4") )
cherche_parent[pas_mere,c("jemnais","gparmat","jemprof","jemact")] <- NA
rm(pas_pere,pas_mere)

#  hodind=substr(acti,1,1)
#   if (hodind==4 & statut!=6) {hodind=5}

dip6 = character(nrow(cherche_parent))
dip6[] <- "6"
dip6[which(cherche_parent[,"dip14"]>=30)]  <- "5"
dip6[which(cherche_parent[,"dip14"]>=41)]  <- "4"
dip6[which(cherche_parent[,"dip14"]>=43)]  <- "3"
dip6[which(cherche_parent[,"dip14"]>=50)]  <- "2"
dip6[which(cherche_parent[,"dip14"]>=60)]  <- "1"
to_match = rep.int(0,nrow(cherche_parent))

cherche_parent[which(cherche_parent[,"classif"] %in% c("1","2","3")),"classif"] <- "a"
cherche_parent[which(cherche_parent[,"classif"] %in% c("4","5")),"classif"] <- "2"
cherche_parent[which(cherche_parent[,"classif"] %in% c("6","7")),"classif"] <- "1"
cherche_parent[which(cherche_parent[,"classif"] %in% c("8","9")),"classif"] <- "3"
cherche_parent[which(cherche_parent[,"classif"] %in% c("a")),"classif"] <- "4"

cherche_parent <- subset(cherche_parent,select=-c(dip14))
cherche_parent <- cbind(cherche_parent,dip6,to_match,nb_enf[which(per1e == "2" | mer1e == "2")])
colnames(cherche_parent)[which(colnames(cherche_parent)=="")] <- "nb_enf" 

library(plyr)

lien = rbind.fill.matrix(cherche_parent,enf_ailleurs)
lien[16025:16100,]

f  <- paste0(dest,"lien.h5")
write.csv(lien,file=f)


# il n'y a plus qu'a faire ce matching....

#JEGRAVE_DIV==1 alors on cherche parmi HODLN=2 ou HODLN=3

# pour l'instant, on laisse de cote la profession des parents qui de toute facon ne colle pas parce que c'est dans
# la jeunesse de l'individu
















# hist(enfant[which(enfant>0 & enfant<2000)],100)
# length(which(enfant>0))/2
# max(nb_enf)

# # parenthese sur l'age à la première naissance
# table(enfant[which(enfant>0)])
# anais [ which(enfant<1940 & enfant >0,arr.ind=TRUE)[,"row"] ]
# enfant2 <- enfant - ANAIS
# which(enfant2 > -1900 & enfant2 < 10,arr.ind=TRUE)
# # ind[6518:6522,c("enf","LIEN","ANAIS")] -> explique la correction du début
# enfant2[enfant2<0] <- 9999
# 
# 
# age_enf <- matrix(0,taille,14) 
# for (i in 1:taille) {
#   k<-1
#   while (min(enfant2[i,]) < 999 ) { 
#     age_enf[i,k] <-  min(enfant2[i,] )
#     enfant2[ i, which.min(enfant2[i,] ) ] <- 9999 
#     k<-k+1
#    }
# }
# age_enf[age_enf==0] <- NA
# 
# 
# mean(age_enf[,1],na.rm=TRUE)
# res <-aggregate(age_enf[ ANAIS<1964 & SEXE==2  ,1], by = list(ANAIS[ ANAIS<1964  & SEXE==2]), FUN = mean, na.rm=TRUE)
# plot(res)
# # ça ne marche pas trop ! 
# il faudrait faire ça en fonction de l'année de naissance de l'enfant, ça ce serait beau



##############################  Statut à la date t   ###########################################

#############################   Travail sur les carrière ########################################################

min(cydeb1[!is.na(cydeb1)])

carriere <- matrix("",length(which(!is.na(cydeb1))),2010-1900+3 )
colnames(carriere) <- colnames(carriere,do.NULL=FALSE,prefix="t")
carriere[,2010-1900+1] <- noi[which(!is.na(cydeb1))]
carriere[,2010-1900+2] <- identmen[which(!is.na(cydeb1))]
carriere[,2010-1900+3] <- identind[which(!is.na(cydeb1))]
colnames(carriere)[2010-1900+1] <- "noi"
colnames(carriere)[2010-1900+2] <- "identmen" 
colnames(carriere)[2010-1900+3] <- "identind" 


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
