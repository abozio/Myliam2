rm(list = ls()) # Clean the workspace
gc()            # Garbage collecting (for memory efficiency)

user <- "IFS"

## AE
if (user=="AE_port"){
  chem_patr <-"M:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"M:/Myliam2/Patrimoine/lien parent enfant/"
}
if (user=="IPP"){
  chem_patr <-"M:/Patrimoine/EP 2009-10/Stata/"
}
if (user=="IFS"){
  chem_patr <-"T:/data/Patrimoine/EP 2009-10/Stata/"
  dest <-"T:/Myliam2/Patrimoine/lien parent enfant/"
}

load(file=paste0(dest,"lien.R"))
# library(StatMatch)
library(weights)
source(paste0(dest,"NND.hotdeck.R"))

match_on = intersect( colnames(cherche_parent) , colnames(enf_ailleurs))
match_on = setdiff(match_on, c("to_match","pond")) 


id_match = seq(nrow(cherche_parent))
cherche_parent = cbind(cherche_parent, id_match )
cherche_parent =  y <- subset(cherche_parent, select= -id)
cherche=apply(cherche_parent, 2,as.numeric)

id_match = seq(nrow(enf_ailleurs))
enf_ailleurs = cbind(enf_ailleurs, id_match )
enf_ailleurs =  y <- subset(enf_ailleurs, select= -id)
trouve=apply(enf_ailleurs, 2,as.numeric)

### on fait des categories, il faut les faires plus petites !

je  = subset(as.data.frame(lien), to_match==0  ) 
jep  = subset(as.data.frame(lien), to_match==1 ) 
sum(je$pond)
sum(jep$pond)

mean(je$anais)
mean(jep$anais)

hist(je$anais)
hist(jep$anais)
wtd.hist(je$anais, weight = je$pond)
wtd.hist(jep$anais, weight = jep$pond)
jep  = subset(as.data.frame(lien), to_match==1 & anais != 0) 
## jeunes enfants
je  = subset(as.data.frame(lien), to_match==0 & anais>(2009-15) ) 
jep  = subset(as.data.frame(lien), to_match==1 & anais>(2009-15) ) 

sum(je$pond)
sum(jep$pond)
table(je$per1e)
table(je$mer1e)
table(je$mer1e,je$per1e)
# il faudra les traiter specifiquement

## deux parents 


two  = subset(as.data.frame(lien), to_match==0 & per1e==2 & mer1e==2  ) 
twop  = subset(as.data.frame(lien), to_match==1 & hodln==1  ) 
sum(two$pond)
sum(twop$pond)

mean(two$anais)
mean(twop$anais)

hist(two$anais)
hist(twop$anais)
wtd.hist(two$anais, weight = two$pond)
wtd.hist(twop$anais, weight = twop$pond)

two  = cherche_parent[which(cherche_parent[,"per1e"]==2 & cherche_parent[,"mer1e"]==2),]
twop = enf_ailleurs  [which(enf_ailleurs  [,"hodln"]==1),]
# remarque : on devrait avoir des two non apparies dans twop puisqu'on peut avoir deux parents 
# vivant sans qu'ils vivent ensemble, il faudra les basculerdans pere et mere

## mere a trouver 
mere  = cherche_parent[which(!is.na(cherche_parent[,"jemnais"]) & is.na(cherche_parent[,"jepnais"])),]
merep = enf_ailleurs  [which(enf_ailleurs[,"mere"] !="" & enf_ailleurs[,"pere"] ==""),]

## pere a trouver 
pere  = cherche_parent[which(is.na(cherche_parent[,"jemnais"]) & !is.na(cherche_parent[,"jepnais"])),]
perep = enf_ailleurs  [which(enf_ailleurs[,"mere"] =="" & enf_ailleurs[,"pere"] !=""),]



res <- NND.hotdeck(as.data.frame(two), as.data.frame(twop), match_on, 
                   don.class=NULL, dist.fun="Manhattan",
                   constrained=FALSE, constr.alg="Hungarian")

res <- NND.hotdeck(cherche_parent, enf_ailleurs, match_on, 
                   don.class=NULL, dist.fun="Manhattan",
                   constrained=FALSE, constr.alg="Hungarian") 
