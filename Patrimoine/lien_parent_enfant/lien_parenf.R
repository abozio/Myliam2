# Partie à décommenter si on veut faire tourner indépendemment de import.R

# rm(list = ls()) # Clean the workspace
# gc()            # Garbage collecting (for memory efficiency)
# 
user <- "IPP"

## AE
if (user=="AE_port"){
  chem_patr <-"M:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"M:/Myliam2/Patrimoine/lien_parent_enfant/"
}
if (user=="IPP"){
  chem_patr <-"M:/Patrimoine/EP 2009-10/Stata/"
  dest <-"C:/Myliam2/Patrimoine/lien_parent_enfant/"
}
if (user=="IFS"){
  chem_patr <-"T:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"T:/Myliam2/Patrimoine/lien_parent_enfant/"
}
# 
# 
# taille    <- 35729
# taille.m   <- 15006
# 
# library(foreign)
# men   <- read.dta(paste0(chem_patr,"menage.dta"))
# ind   <- read.dta(paste0(chem_patr,"Individu.dta"))
# 
# ################################  on renomme   ################################
# 
# ## on modifie les identifiants ménage pour que ça ait une tête sympa; on changera éventuellement à la fin
# # pour que le numéro soient simplifiés mais pour l'instant cette forme peut être plus évidente en cas de merge
# # (encore que si on a une table de passage...)##
# ind$res <- sprintf("%06d",match(ind$identmen,men$identmen))   #on laisse un zéro devant, toujours pratique 
# men$res <- sprintf("%06d",rep(1:length(unique(men$identmen)))) # en cas de duplication, etc
# ind$id  <- paste(ind$res,ind$noi)
# 
# # on retire aussi toute les variables concernant pr et cj puisqu'on peut
# # recuperer celle qu'on veut plus tard
# names(men)
# grep("^pr", names(men))
# ToRemove = names(men)[grep("pr$", names(men))] 
# ToRemove <- ToRemove[which(! ToRemove %in% c("indepr","r_dcpr","r_detpr"))]
# ToRemove = c(ToRemove, names(men)[grep("cj$", names(men))] )
# men = subset(men, select= names(men)[which(! names(men) %in% ToRemove)] )
# # dans la meme veine, on retire les variables diplomes
# ToRemove = names(men)[grep("^diplom", names(men))] 
# men = subset(men, select= names(men)[which(! names(men) %in% ToRemove)] )
# 
# attach(men)
# attach(ind) #note bien : l'ordre est important entre les deux attach, on veut que identmen renvoie aux identmen de ind.
# 
# ########  Liste des naissances des enfants  ######
# # cette liste n'a pas besoin d'être faite, il faut juste le numéro du père et de la mère que l'on a pour les enfants à
# # domicile, pour les autres, on va faire un matching. 
# 
# 



# Partie à décommenter si on veut faire tourner indépendemment de import.R
# 
# #on récupère en même temps le nombre d'enfant
# nb_enf  <- numeric(taille)
# enfant <- matrix(0,taille,17,dimnames=(list( NULL,
#                                              c("enf1","enf2","enf3","enf4","enf5","enf6","enf7","enf8","enf9","enf10","enf11","enf12","enf13","enf14","enf15","enf16","enf17"))))
# #on s'arrete à 17 parce qu'on n'a pas plus dans l'enquête, si on en change il faut verifier 
# 
# ## a) enfants cohabitant
# parent1 <- numeric(taille)
# parent2 <- numeric(taille)
# for (i in which(enf==1)) {
#   parent1[i] <- which(identmen==identmen[i] & lienpref=="00")
#   if ( length(which(identmen==identmen[i] & lienpref=="01"))>0) { 
#     parent2[i] <- which(identmen==identmen[i] & lienpref=="01")
#   }
# }
# 
# 
# 
# par.enf1 <- function(i) {
#   if ( length(identmen==identmen[i] & lienpref=="00")>0) {parent1[i] <- which(identmen==identmen[i] & lienpref=="00")}
#   if ( length(which(identmen==identmen[i] & lienpref=="01"))>0){  parent2[i] <- which(identmen==identmen[i] & lienpref=="01") }
# }
# 
# for (i in which(enf==1)) {
#   par.enf1(i)
# }
# 
# 
# for (i in which(enf==2)) {
#   parent1[i] <-  which(identmen==identmen[i] & lienpref=="00")
# }
# for (i in which(enf==3)) {
#   parent1[i] <-  which(identmen==identmen[i] & lienpref=="01")
# }
# 
# 
# #la fonction qui met les PARENTs et qui sera utilisé ci-après pour les petits enfants
# ajout.PARENT <- function() {
#   if (length(ll)==1) {
#     parent1[i] <<- ll
#     if (length( which(identmen==identmen[i] & !enf %in% c(1,2,3) & ! lienpref %in% c("00","01") & noi != i ) ) >0 ) {
#       parent2[i] <<- which(identmen==identmen[i] & !enf %in% c(1,2,3) & ! lienpref %in% c("00","01") & noi != i )[1]
#     }
#   }
# }
# 
# for (i in which(lienpref=="21")) {
#   #liste des enfants de la PR et donc des PARENTs potentiels
#   ll <- which(identmen==identmen[i] & enf %in% c(1,2,3))
#   ajout.PARENT()
#   if (is.na(parent1[i])) {
#     if (mer1e[i] =="1" & per1e[i]=="1") {
#       ll <- intersect(ll, which(couple==1) )
#       ajout.PARENT()
#     }
#     else if (mer1e[i] =="1" & per1e[i]!="1") {
#       ll <- intersect(ll, which(sexe==2))
#       ajout.PARENT()
#       ll <- ll[1]
#       ajout.PARENT()
#     }
#     else if (mer1e[i]!="1" & per1e[i]=="1") {
#       ll <- intersect(ll, which(sexe==1))
#       ajout.PARENT()
#       ll <- ll[1]
#       ajout.PARENT()
#     }
#   }
# }
# 
# 
# # on fait aussi tourner ce qui suit parce qu'on a besoin du nombre d'enfant
# 
# for (i in which(anais<1995)) { #on ne tourne que sur les plus de 14 ans
#   # on définit la liste des enfants hors ménage list
#   list <- NULL
#   if (lienpref[i]=="00") {
#     for (k in 1:12) {
#       if (get(paste0("hodln",k))[i] %in% c('1','2')) {
#         list <- c(list,get(paste0("hodan",k))[i])
#       }
#     }
#   }
#   if (lienpref[i]=="01") {
#     for (k in 1:12) {
#       if (get(paste0("hodln",k))[i] %in% c('1','3')) {
#         list <- c(list,get(paste0("hodan",k))[i])
#       }
#     }
#   }
#   
#   # on a tous les pour calculer le nombre d enfant
#   # on prend d'abord les enfants cohabitant
#   l <-  union( anais[which(parent1==i)] ,  anais[which(parent2==i)]) #nombre d'enfant cohabitant avec i
#   nb_enf[i] <-  length(l)+length(list)
#   if (length(list)>0) {
#   }
# }


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
names(men)
men = merge( men,inf_pr,by.x="res",by.y="respr")
men = merge( men,inf_cj,by.x="res",by.y="rescj",all=TRUE)


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
  
  
  # info sur les PARENTs
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
  
  res <- men[liste,"res"]
  pond <- men[liste,"pond"]
  print(length(liste))
  PARENT = rep.int(1,length(liste))
  ajout <- cbind(res,sexe,anais,couple,dip6,nb_enf,situa,classif,info_pere,info_mere, PARENT,hodln,pond)
}

look_child <- save_inf(1)
for (k in 2:12) {
  look_child <- rbind(look_child,save_inf(k))
}




# on peut maintenant retirer toutes les variables concernant les enfants a l exterieur du menage
# ToRemove = names(men)[grep("^hod", names(men))] 
# # length(ToRemove) 157 variables tout de même
# men = subset(men, select= names(men)[which(! names(men) %in% ToRemove)] )



#### info sur les PARENTs
look_parent <- as.matrix(subset(ind, per1e == "2" | mer1e == "2", 
                                   select= c(id,sexe,anais,couple,dip14,situa,jemnais,gparmat,jemprof,jemact,
                                             jepnais,gparpat,jepprof,per1e,mer1e,jegrave_div,classif,pond)))

# on supprime les infos quand on ne cherche pas ce PARENT
pas_pere <- which(look_parent[,"per1e"] %in% c("1","3","4") )
look_parent[pas_pere,c("jepnais","gparpat","jepprof")] <- NA
pas_mere <- which(look_parent[,"mer1e"] %in% c("1","3","4") )
look_parent[pas_mere,c("jemnais","gparmat","jemprof","jemact")] <- NA
rm(pas_pere,pas_mere)

#  hodind=substr(acti,1,1)
#   if (hodind==4 & statut!=6) {hodind=5}

dip6 = character(nrow(look_parent))
dip6[] <- "6"
dip6[which(look_parent[,"dip14"]>=30)]  <- "5"
dip6[which(look_parent[,"dip14"]>=41)]  <- "4"
dip6[which(look_parent[,"dip14"]>=43)]  <- "3"
dip6[which(look_parent[,"dip14"]>=50)]  <- "2"
dip6[which(look_parent[,"dip14"]>=60)]  <- "1"
PARENT = rep.int(0,nrow(look_parent))

look_parent[which(look_parent[,"classif"] %in% c("1","2","3")),"classif"] <- "a"
look_parent[which(look_parent[,"classif"] %in% c("4","5")),"classif"] <- "2"
look_parent[which(look_parent[,"classif"] %in% c("6","7")),"classif"] <- "1"
look_parent[which(look_parent[,"classif"] %in% c("8","9")),"classif"] <- "3"
look_parent[which(look_parent[,"classif"] %in% c("a")),"classif"] <- "4"

look_parent <- subset(look_parent,select=-c(dip14))
look_parent <- cbind(look_parent,dip6,PARENT,nb_enf[which(per1e == "2" | mer1e == "2")])
colnames(look_parent)[which(colnames(look_parent)=="")] <- "nb_enf" 

library(plyr)

lien = rbind.fill.matrix(look_parent,look_child)
taille = nrow(lien)
colnames(lien)[1] <- "id_origin"
id = seq(1:taille)
period = rep(2009, taille)
lien = cbind(id,period,lien)

lien[16025:16032,]

lien = subset( lien, select = c(id,period,PARENT,id_origin,pere,mere,sexe,anais,couple,dip6,nb_enf,situa,classif,pond,
                                mer1e,per1e))
# TODO : ajouter les infos sur les grands-parents en vie, facile, et autres
dimnames(lien)
lien =apply(lien, 2,as.numeric)
lien[which(is.na(lien))]  <- 0
f  <- paste0(dest,"lien.csv")
write.csv(lien,file=f)

save(lien,file=paste0(dest,"lien.R"))
# 
# # élément à utiliser si on veut matcher, not
# look_child = as.data.frame(apply(look_child, 2,as.numeric))   
# look_parent = as.data.frame(apply(look_parent, 2,as.numeric))
# mother_child = subset(look_child, !is.na(mere), select= c(mere,pere,sexe,anais,couple,dip6,nb_enf,situa,classif))
# fater_child = subset(look_child, is.na(mere),  select= c(mere,pere,sexe,anais,couple,dip6,nb_enf,situa,classif))
# 
# 
# test = merge(person, mother_child, by.x="id", by.y="mere", suffixes=c("","mere"), all=TRUE)
# which(mother_child$mere==7)
# mother_child[c(1,6710),]
# voir = subset(test, is.na(period))
# test = merge(test, look_parent, by="id", suffixes=c("","match"), all=TRUE)




#JEGRAVE_DIV==1 alors on cherche parmi HODLN=2 ou HODLN=3
# pour l'instant, on laisse de cote la profession des PARENTs qui de toute facon ne colle pas parce que c'est dans
# la jeunesse de l'individu


## il n'y a plus qu'a faire ce matching....
# list.class = c("sexe","anais","couple","dip6","nb_enf","situa")
# lienDF=as.data.frame(lien)
# test <- split(lienDF, f=as.list(lienDF[ ,c(list.class)]), drop=TRUE)
# test[[2]]
# 
# taille.ssgroupe <- unlist(lapply(test,nrow)) # table(taille.ssgroupe.pat) ; sum(taille.ssgroupe.pat)
# 
# l.pat <- split(pat, f = as.list(pat[,c(match.exact) ]),drop=TRUE)
