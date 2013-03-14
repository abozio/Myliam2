
# lecture des données de l'enquêtre patrimoine
# selection des valeurs utiles pour le matching.
# construction des valeurs dans certains cas.


rm(list = ls()) # Clean the workspace
gc()            # Garbage collecting (for memory efficiency)

user <- "IPP_pers"

## AE
if (user=="AE_port"){
  chem_patr <-"M:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"M:/Myliam2/Patrimoine/Import/"
}
if (user=="IPP_pers"){
  chem_patr <-"M:/Patrimoine/EP 2009-10/Stata/"
  dest <-"C:/Myliam2/Patrimoine/Import/"
}
if (user=="IPP"){
  chem_patr <-"M:/Patrimoine/EP 2009-10/Stata/"
}

if (user=="IFS"){
  chem_patr <-"T:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"T:/Myliam2/Patrimoine/Import/"
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
ind$idi  <- paste(ind$res,ind$noi)
ind$id   <- seq(nrow(ind))

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
attach(ind) #note bien : l'ordre est important entre les deux attach, on veut que res renvoie aux res de ind.

############################## conjoint ################################
conj <- numeric(taille)
ConjMiss <- c()
ConjToo  <- c()


for (i in which(couple==1)) {
  if(length(which(res==res[i] & couple==1 & id != i))==1){
    conj[i]=which(res==res[i] & couple==1 & id != i)
  }
  else if(length(which(res==res[i] & couple==1 & id != i))>1){
    
    if (ind[i,"lienpref"] %in% c("00","01")) {
      conj[i]=which(res==res[i] & couple==1 & id != i & ind[,"lienpref"] %in% c("00","01"))
    }
    else if (ind[i,"lienpref"] %in% c("02","31") & i != "13582") { # sans ce truc ad-hoc, il faut verifier la longueur 
      # à chaque fois, j'ai pas le courage même si une petite macro pour faire ça serait bien utile pour plein de chose
      conj[i]=which(res==res[i] & couple==1 & id != i & ind[,"lienpref"] %in% c("02","31"))
    }    
    else if (ind[i,"lienpref"] %in% c("03")) {
      conj[i]=which(res==res[i] & couple==1 & id != i & ind[,"lienpref"] %in% c("03"))
    }
    else if (ind[i,"lienpref"] %in% c("32")) {
      conj[i]=which(res==res[i] & couple==1 & id != i & ind[,"lienpref"] %in% c("32"))
    }
    else {
      if (length(which(res==res[i] & couple==1 & id != i & couple==1 & !(ind[,"lienpref"] %in% c("00","01","02","03","32"))))==1    ) {
        conj[i]=which(res==res[i] & couple==1 & id != i & couple==1 & !(ind[,"lienpref"] %in% c("00","01","02","03","32"))) }
      else {
        print (i,ind[i,"lienpref"] )
        ConjToo  <- c(ConjToo,i)}
      }
      
  }
  else {ConjMiss <- c(ConjMiss,i)}
}
# ind[which(res %in% ind[ConjToo,"res"]),]
# table(ind$couple)

############################## Liste des naissances des enfants ################################

#on a deux cas: les enfants cohabitant a) et les enfants décohabitant b). Ils sont enregistrés différemment. 
#pour les premiers, on regarde pour chacun qui est leur pere et qui est leur mere dans les tables parent1 et parent2
#ensuite dans une grosse étape, on crée la matrice enfant

#on récupère en même temps le nombre d'enfant
nb_enf  <- numeric(taille)

## a) enfants cohabitant
# TODO : améliorer ce programme en utilisant by
parent1 <- numeric(taille)
parent2 <- numeric(taille)
for (i in which(enf==1)) {
  parent1[i] <- which(res==res[i] & lienpref=="00")
  if ( length(which(res==res[i] & lienpref=="01"))>0) { 
    parent2[i] <- which(res==res[i] & lienpref=="01")
  }
}

par.enf1 <- function(i) {
  if ( length(res==res[i] & lienpref=="00")>0) {parent1[i] <- which(res==res[i] & lienpref=="00")}
  if ( length(which(res==res[i] & lienpref=="01"))>0){  parent2[i] <- which(res==res[i] & lienpref=="01") }
}

for (i in which(enf==1)) {
  par.enf1(i)
}


for (i in which(enf==2)) {
  parent1[i] <-  which(res==res[i] & lienpref=="00")
}
for (i in which(enf==3)) {
  parent1[i] <-  which(res==res[i] & lienpref=="01")
}


#la fonction qui met les parents et qui sera utilisé ci-après pour les petits enfants
ajout.parent <- function() {
  if (length(ll)==1) {
    parent1[i] <<- ll
    if (length( which(res==res[i] & !enf %in% c(1,2,3) & ! lienpref %in% c("00","01") & noi != i ) ) >0 ) {
      parent2[i] <<- which(res==res[i] & !enf %in% c(1,2,3) & ! lienpref %in% c("00","01") & noi != i )[1]
    }
  }
}

for (i in which(lienpref=="21")) {
  #liste des enfants de la PR et donc des parents potentiels
  ll <- which(res==res[i] & enf %in% c(1,2,3))
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

# On ne fait que maintenant le travail sur le sexe pour dissocier, pere et mere
# C'est peut-être inutile, on verra
pere <- parent1
mere <- parent2

for (i in which(parent1>0)) {
  if (sexe[parent1[i]]==1) { pere[i]= parent1[i]}
  else { mere[i]= parent1[i]}
  if (parent2[i]>0) {
    if (sexe[parent2[i]]==1) { pere[i]= parent2[i]}
    else { mere[i]= parent2[i]}
  } 
}


age =  2009 - ind$anais
period = rep(2009,taille)
person = cbind(ind$id,period,ind$res,age,ind$sexe,conj,mere,pere,situa,ind[,"zsalaires_i"])
colnames(person)[c(1,3,5,6,10)] <- c("id","res","sexe","conjoint","salaire")
person =apply(person, 2,as.numeric)
person[which(is.na(person))]  <- 0

write.csv(person,file=paste0(dest,"person2009.csv"),row.names=F)


####### table ménage ####
period = rep(2009,taille.m)
menage = cbind(men$res,period,loyer,tu,zeat,surface,resage,restyp,reshlm,men$pond)
colnames(menage)[c(1,10)] <- c("id","pond")
menage =apply(menage, 2,as.numeric)
menage[which(is.na(menage))]  <- 0

write.csv(menage,file=paste0(dest,"menage2009.csv"),row.names=F)
