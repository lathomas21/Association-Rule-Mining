library(arules)

#load in data
titanic = read.csv("titanic.csv")
game_of_thrones= read.csv("game_of_thrones.csv")
retail = read.csv("retail.csv")

#titanic start
#all rules:
rulesT<- apriori(titanic)
inspect(rulesT)
inspect(head(rulesT))

#rules that meet criteria
rulesTbetter<- apriori(titanic, parameter = list(support=0.01, confidence=.9))
inspect(rulesTbetter)
inspect(head(rulesTbetter))

#get rid of redundant rules meet criteria
subset.matrix1<- is.subset(rulesTbetter,rulesTbetter)
subset.matrix1[lower.tri(subset.matrix1, diag=T)]<-NA
redundantT<-colSums(subset.matrix1, na.rm=T) >= 1
which(redundantT)

rules.prunnedT<-rulesTbetter[!redundantT]
inspect(rules.prunnedT)
inspect(head(rules.prunnedT))


#sort non redundant rules that meet criteria
rules_orderT<-sort(rules.prunnedT, by="lift")
inspect(rules_orderT)
inspect(head(rules_orderT))

#rules for survived yes/ no

rules<- apriori(titanic, appearance=list(rhs= c("Survived=Yes", "Survived=No"),
                                default="lhs"))
inspect(rules)
inspect(head(rules))
#rules for survived yes/ no that meet criteria

rules<- apriori(titanic, parameter = list(support=0.01, confidence=.9),
                appearance=list(rhs= c("Survived=Yes", "Survived=No"),
                default="lhs"))
inspect(rules)
inspect(head(rules))

#get rid of redundant rules meet criteria for survive
subset.matrix2<- is.subset(rules,rules)
subset.matrix2[lower.tri(subset.matrix2, diag=T)]<-NA
redundant<-colSums(subset.matrix2, na.rm=T) >= 1
which(redundant)

rules.prunned<-rules[!redundant]
inspect(rules.prunned)
inspect(head(rules.prunned))


#sort non redundant rules that meet criteria for survive
rules_order<-sort(rules.prunned, by="lift")
inspect(rules_order)
inspect(head(rules_order))
#titanic end

#retail start


retail$TransactionNo=NULL

#make retail factors so we can find rules
retail[] <- lapply(retail, factor)

#find all rules
rulesR<- apriori(retail)
inspect(rulesR)
inspect(head(rulesR))

#find rules that meet criteria
rulesRbetter<- apriori(retail, parameter = list(support=0.01, confidence=.9))
inspect(rulesRbetter)
inspect(head(rulesRbetter))


#rules for beverage, meat and personal care
rules<- apriori(retail, parameter = list(support=0.01, confidence=.9),
                appearance=list(rhs= c("Beverage=1", "Meat=1", "PersonalCare=1",
                                       "Beverage=0", "Meat=0", "PersonalCare=0"),
                default="lhs"))
inspect(rules)
inspect(head(rules))
#get rid of redundant rules that meet criteria
subset.matrix4<- is.subset(rules,rules)
subset.matrix4[lower.tri(subset.matrix4, diag=T)]<-NA
redundant<-colSums(subset.matrix4, na.rm=T) >= 1
which(redundant)

rules.prunned<-rules[!redundant]
inspect(rules.prunned)
inspect(head(rules.prunned))

#sort non redunant rules that meet criteria
rules_order<-sort(rules.prunned, by="lift")
inspect(rules_order)
inspect(head(rules_order))

#retail end

#game of thrones start
#make game of thrones factors
game_of_thrones[] <- lapply(game_of_thrones, factor)

#find all rules
rulesG <- apriori(game_of_thrones)
inspect(rulesG)
inspect(head(rulesG))

#find rules that meet criteria
rulesGbetter<- apriori(game_of_thrones, parameter = list(support=0.01, confidence=.9))
nspect(rulesGbetter)
inspect(head(rulesGbetter))

#find non redundant rules that meet criteria
subset.matrix5<- is.subset(rulesGbetter,rulesGbetter)
subset.matrix5[lower.tri(subset.matrix5, diag=T)]<-NA
redundantG<-colSums(subset.matrix5, na.rm=T) >= 1
which(redundantG)

rules.prunnedG<-rulesGbetter[!redundantG]
inspect(rules.prunnedG)
inspect(head(rules.prunnedG))

#sort non redundant rules that meet criteria
rules_orderG<-sort(rules.prunnedG, by="lift")
inspect(rules_orderG)
inspect(head(rules_orderG))

#rules for survival
rules<- apriori(game_of_thrones, parameter = list(support=0.01, confidence=.9),
                appearance=list(rhs= c("Survives=1", "Survives=0"),
                default="lhs"))
inspect(rules)
inspect(head(rules))

#find non redundant rules that meet criteria
subset.matrix3<- is.subset(rules,rules)
subset.matrix3[lower.tri(subset.matrix3, diag=T)]<-NA
redundant<-colSums(subset.matrix3, na.rm=T) >= 1
which(redundant)

rules.prunned<-rules[!redundant]
inspect(rules.prunned)
inspect(head(rules.prunned))

#sort non redundant rules that meet criteria

rules_order<-sort(rules.prunned, by="lift")
inspect(rules_order)
inspect(head(rules_order))
#game of thrones end

#find rules for not survive in game of thrones
rules<- apriori(game_of_thrones, parameter = list(support=0.01, confidence=.8),
                appearance=list(rhs= c("Survives=0"),
                                default="lhs"))
inspect(rules)
inspect(head(rules))

subset.matrix6<- is.subset(rules,rules)
subset.matrix6[lower.tri(subset.matrix6, diag=T)]<-NA
redundant<-colSums(subset.matrix6, na.rm=T) >= 1
which(redundant)

rules.prunned<-rules[!redundant]
inspect(rules.prunned)
inspect(head(rules.prunned))

#sort non redundant rules that meet criteria

rules_order<-sort(rules.prunned, by="lift")
inspect(rules_order)
inspect(head(rules_order))