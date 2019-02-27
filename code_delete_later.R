items <- data.frame(crmQs$X35,crmQs$X34, crmQs$X33,crmQs$X15,
                    crmQs$X14,crmQs$X13,crmQs$X12,crmQs$X11,
                    crmQs$X10,crmQs$X9, crmQs$X8, crmQs$X7,
                    crmQs$X6,crmQs$X5,crmQs$X4,crmQs$X3,
                    crmQs$X2,crmQs$X1,crm$FAT,crm$TT)
choices  = c("Never", "Low/Poor", "Adequate/Sometimes", "Moderate", "High/Always")
for(i in 1:ncol(items)) {
  items[,i] = factor(items[,i], levels=1:5, labels=choices, ordered=TRUE)
}
View(items)
View(items)
title <- "Perceptions of CRM success indicators"

plot(likert(items), wrap= 30) + ggtitle(title)
summary(likert(items))

##### used likert_fun found from http://reganmian.net/blog/2013/10/02/likert-graphs-in-r-embedding-metadata-for-easier-plotting/

db <- items
db <- likert_add_fullnames(db, c("crmQs.X1"="Involvement in CRM", 
                                 "crmQs.X2"= "Barangay involvement in CRM", 
                                 "crmQs.X3"="Ability to influence CRM", 
                                 "crmQs.X4"= "Barangay's ability to influence CRM", 
                                 "crmQs.X5"="Ability to influence CRM planning", 
                                 "crmQs.X6"= "Ability to influence programs post-implementation", 
                                 "crmQs.X7"="Community cooperation", 
                                 "crmQs.X8"="Fisherfolk cooperation", 
                                 "crmQs.X9"= "Community cooperation with LGU", 
                                 "crmQs.X10"= "Fair resolution of fishery disputes", 
                                 "crmQs.X11"="Control over monitoring and regulation of resources", 
                                 "crmQs.X12"= "Equal access to coastal resources", 
                                 "crmQs.X13"= "How well community complies with regulations", 
                                 "crmQs.X14"= "How well fishery regulations are enforced", 
                                 "crmQs.X15"= "How well information is shared", 
                                 "crmQs.X33"= "Willing to participate in CRM", 
                                 "crmQs.X34"= "CRM is a priority", 
                                 "crmQs.X35"= "Informed about CRM activities",
                                 "crm.FAT"= "Fish abundance",
                                 "crm.TT"= "Threat to resources"))
#don't see full names, and didn't come up in plot, so try with rename
db <- rename(db, c(crmQs.X1="Involvement in CRM", 
                   crmQs.X2= "Barangay involvement in CRM", 
                   crmQs.X3="Ability to influence CRM", 
                   crmQs.X4= "Barangay's ability to influence CRM", 
                   crmQs.X5="Ability to influence CRM planning", 
                   crmQs.X6= "Ability to influence programs post-implementation", 
                   crmQs.X7="Community cooperation", 
                   crmQs.X8="Fisherfolk cooperation", 
                   crmQs.X9= "Community cooperation with LGU", 
                   crmQs.X10= "Fair resolution of fishery disputes", 
                   crmQs.X11="Control over monitoring and regulation of resources", 
                   crmQs.X12= "Equal access to coastal resources", 
                   crmQs.X13= "How well community complies with regulations", 
                   crmQs.X14= "How well fishery regulations are enforced", 
                   crmQs.X15= "How well information is shared", 
                   crmQs.X33= "Willing to participate in CRM", 
                   crmQs.X34= "CRM is a priority", 
                   crmQs.X35= "Informed about CRM activities",
                   crm.FAT= "Fish abundance",
                   crm.TT= "Threat to resources"))
View(db)

groups <- list(
  "Equity"=c("Involvement in CRM","Ability to influence CRM",
             "Ability to influence CRM planning","Ability to influence programs post-implementation", 
             "Barangay involvement in CRM", "Barangay's ability to influence CRM",
             "Control over monitoring and regulation of resources", "Equal access to coastal resources"), 
  "Efficiency"=c("Community cooperation","Fisherfolk cooperation","Community cooperation with LGU",
                 "Fair resolution of fishery disputes"), 
  "Sustainability"=c("How well community complies with regulations","How well fishery regulations are enforced",
                     "How well information is shared","CRM is a priority","Willing to participate in CRM", 
                     "Informed about CRM activities", "Threat to resources", "Fish abundance"))

par(mar=c(5.1, 4.1, 4.1, 2.1))
db <- likert_store_groups(db, groups)
plot_likert_groups(db, all=T) #plots all 3 separately
plot_likert_groups(db, groups="Equity") #if want to plot one at a time

