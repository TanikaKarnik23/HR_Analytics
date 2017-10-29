#Association rules
df<- read.csv("HR_comma_sep.csv")
datafieldummy<- dummy.data.frame(df,names = c("department", "salary"), sep = ".")
str(datafiledummy)
datafieldummy$satisfaction_level_bins<-cut(datafieldummy$satisfaction_level, breaks=c(0,0.33,0.66,1), 
                                           include.lowest=TRUE, 
                                           labels = c("not-satisfied", "neutral", "satisfied"))
datafieldummy$last_evaluation_bins<-cut(datafieldummy$last_evaluation, breaks=c(0,0.33,0.66,1), 
                                        include.lowest=TRUE, 
                                        labels = c("low-performer", "average-performer", "high-performer"))
datafieldummy$average_monthly_hours_bins<-cut(datafieldummy$average_montly_hours, 
                                              breaks = c(90, 150, 210, 270, 330), 
                                              include.lowest=TRUE, 
                                              labels = c("90-150hours", "150-210hours", "210-270hours", "270-330hours"))
datafieldummy$number_project_bins<-cut(datafieldummy$number_project, breaks = c(0,5,10), 
                                       include.lowest=TRUE, labels = c("less-than-5", "more-than-5"))
datafieldummy$time_spend_company_bins=cut(datafieldummy$time_spend_company, breaks = c(0,3,6,10), 
                                          include.lowest=TRUE, labels = c("0-3years", "3-6years", "more-than-6years") )

datafieldummy$satisfaction_level<-NULL
datafieldummy$last_evaluation<-NULL
datafieldummy$average_montly_hours<-NULL
datafieldummy$number_project<-NULL
datafieldummy$time_spend_company<-NULL

library("arules")
install.packages("Rcpp")
library("arulesViz")
datafieldummy<- dummy.data.frame(datafieldummy,names=c("time_spend_company_bins","number_project_bins",
                                                       "average_monthly_hours_bins",
                                                       "last_evaluation_bins","satisfaction_level_bins"), sep=".")
str(datafieldummy)
datafieldummy<-data.frame(lapply(datafieldummy,factor))
rules<-apriori(data=datafieldummy, parameter = list(supp=0.1, conf=0.5,maxlen=6), 
               appearance = list(default="lhs",rhs="left=1"),control = list(verbose=F))
# inspect(rules)
rules_sorted<-sort(rules,decreasing=TRUE,by="lift")
rules_sorted
summary(rules_sorted)
#plot(rules_sorted,shading = "lift")
sel <- plot(rules_sorted[1:1000], measure=c("support", "confidence"), shading="confidence", interactive=TRUE)
inspect(rules_sorted[1:100])




