# Set your working directory for your data:
setwd("/your/working/directory")
library(ggplot2)

#  Load your csv files to plot:

# Start with DMSO data for 3 replicates.
exp1 = read.csv("Replicate 1 Control.csv")
exp2 = read.csv("Replicate 2 Control.csv")
exp3 = read.csv("Replicate 3 Control.csv")

# Add in the lowest dose:
exp4 = read.csv("Replicate 1 low.csv")
exp5 = read.csv("Replicate 2 low.csv")
exp6 = read.csv("Replicate 3 low.csv")

# Add in the data for the intermediate dose:
exp7 = read.csv("Replicate 1 medium.csv")
exp8 = read.csv("Replicate 2 medium.csv")
exp9 = read.csv("Replicate 3 medium.csv")
  
# Add in the data for the highest dose:
exp10 = read.csv("Replicate 1 high.csv")
exp11 = read.csv("Replicate 2 high.csv")
exp12 = read.csv("Replicate 3 high.csv")

# Select which datasets you want to include in your analysis from the above.
total = rbind(exp1,exp2,exp3,exp4,exp5,exp6,exp7,exp8,exp9,exp10,exp11,exp12)

# Update order of the conditions on the x axis - dependent on number of conditions in the .csv, [c(4,3,1,2)]) statement re-orders bars 1-4
class(total$TreatmentConditions)
total$TreatmentConditions = as.factor(total$TreatmentConditions)
class(total$TreatmentConditions)
levels(total$TreatmentConditions)

# Re-order the conditons as required:
total$TreatmentConditions = factor(total$TreatmentConditions, levels=levels(total$TreatmentConditions)[c(4,3,1,2)])
levels(total$TreatmentConditions)

# Set amount of jitter in the plots
jitter<- position_jitter(width=0.2)

# Preliminary plots  ----------------------------------------------------------------

#Plot 1/Aspect Ratio
plt_all<-ggplot(total,aes(y=AR.Ch0,x=TreatmentConditions, col=Replicate))+geom_point(position=jitter)+xlab("Treatment")+ylab("1 / Aspect Ratio")+theme(legend.position = "none")
plt_all

#Plot Circularity
plt_all2<-ggplot(total,aes(y=Circularity,x=TreatmentConditions, col=Replicate))+geom_point(position=jitter)+xlab("Treatment")+ylab("Circularity")+theme(legend.position = "none")
plt_all2


#Plot Area
plt_all3<-ggplot(total,aes(y=Area.Ch0,x=TreatmentConditions, col=Replicate))+geom_point(position=jitter)+xlab("Treatment")+ylab("Area / pixels^2")+theme(legend.position = "none")
plt_all3


# Boxplots ----------------------------------------------------------------
library(ggpubr)
library(rstatix)

# Aspect Ratio Box Plot
bxp<-ggboxplot(total,x="TreatmentConditions",y="AR.Ch0", width=0.6, add="jitter", color="grey30",fill="grey92", add.params=list(color="Replicate"))+xlab("Treatment")+ylab("1 / Aspect Ratio")+theme(legend.position="none")

# Run Student's t-test to have the annotations: want to compare AR.Ch0 between treatments and control.

stat.test<-total %>%
  t_test(AR.Ch0 ~ TreatmentConditions) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test
# Don't show any annotations when P>0.05.
# Note line 74: where group1=='Control' and group2=='Control' means we're only looking for significance where the control is in the pair of conditions being compared.
stat.test <- stat.test %>%
  add_xy_position(x="TreatmentConditions", dodge=0.2)

#### !!! NOTE: Need to change terms of stat test depending on name of control.
bxp + stat_pvalue_manual(stat.test[which((stat.test$p.adj<0.05)&((stat.test$group1=="Control")|(stat.test$group2=="Control"))),], cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"), tip.length = 0.01, bracket.nudge.y=0.05) + grids(axis=c("y"), color="grey85", size=NULL, linetype = "dashed")

# For explicit P values, use line below instead.
#bxp + stat_pvalue_manual(stat.test[which((stat.test$p.adj<0.05)&((stat.test$group1=="0.15% DMSO")|(stat.test$group2=="0.15% DMSO"))),], label="p.adj", tip.length = 0.01, bracket.nudge.y = -2000) + grids(axis=c("y"), color="grey85", size=NULL, linetype = "dashed")

# Circularity box plot:
bxp2<-ggboxplot(total,x="TreatmentConditions",y="Circularity", width=0.6, add="jitter", color="grey30",fill="grey92", add.params=list(color="Replicate"))+xlab("Treatment")+ylab("Circularity")+theme(legend.position="none")

stat.test<-total %>%
  t_test(Circularity ~ TreatmentConditions) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test
# Don't show any annotations when P>0.05.
stat.test <- stat.test %>%
  add_xy_position(x="TreatmentConditions", dodge=0.4)

#### !!! NOTE Need to change terms of stat test depending on name of control.
bxp2 + stat_pvalue_manual(stat.test[which((stat.test$p.adj<0.05)&((stat.test$group1=="Control")|(stat.test$group2=="Control"))),], cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"), tip.length = 0.01, bracket.nudge.y = 0) + grids(axis=c("y"), color="grey85", size=NULL, linetype = "dashed")

# For explicit P values, use line below instead.
#bxp2 + stat_pvalue_manual(stat.test[which((stat.test$p.adj<0.05)&((stat.test$group1=="0.15% DMSO")|(stat.test$group2=="0.15% DMSO"))),], label="p.adj", tip.length = 0.01, bracket.nudge.y = -2000) + grids(axis=c("y"), color="grey85", size=NULL, linetype = "dashed")

# Area box plot:
bxp3<-ggboxplot(total,x="TreatmentConditions",y="Area.Ch0", width=0.6, add="jitter", color="grey30",fill="grey92", add.params=list(color="Replicate"))+xlab("Treatment")+ylab("Area / pixels^2")+theme(legend.position="none")

stat.test<-total %>%
  t_test(Area.Ch0 ~ TreatmentConditions) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

# Don't show any annotations when P>0.05.
stat.test <- stat.test %>%
  add_xy_position(x="TreatmentConditions", dodge=0.8)

#### !!! NOTE Need to change terms of stat test depending on name of control.
bxp3 + stat_pvalue_manual(stat.test[which((stat.test$p.adj<0.05)&((stat.test$group1=="Control")|(stat.test$group2=="Control"))),], cutpoints = c(0, 1e-04, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"), tip.length = 0.01, bracket.nudge.y = -4800) + grids(axis=c("y"), color="grey85", size=NULL, linetype = "dashed")

# For explicit P values, use line below instead.
#bxp3 + stat_pvalue_manual(stat.test[which((stat.test$p.adj<0.05)&((stat.test$group1=="0.15% DMSO")|(stat.test$group2=="0.15% DMSO"))),], label="p.adj", tip.length = 0.01, bracket.nudge.y = -2000) + grids(axis=c("y"), color="grey85", size=NULL, linetype = "dashed")


# ANOVA -------------------------------------------------------------------
# Analyse .csv files with one-way ANOVA and a Dunnett post-hoc test if results are significant.
## "total" has all the data together. H0: means are the same; H1 means different.
# Assuming data drawn from normal distribution.
anova_one_way_AR <- aov(AR.Ch0~TreatmentConditions, data = total)
summary(anova_one_way_AR)

anova_one_way_circ <- aov(Circularity~TreatmentConditions, data = total)
summary(anova_one_way_circ)

anova_one_way_area <- aov(Area.Ch0~TreatmentConditions, data = total)
summary(anova_one_way_area)

library(multcomp)
dunnett_AR <- glht(anova_one_way_AR, linfct = mcp(TreatmentConditions = "Dunnett"))
summary(dunnett_AR)

dunnett_circ <- glht(anova_one_way_circ, linfct = mcp(TreatmentConditions = "Dunnett"))
summary(dunnett_circ)

dunnett_area <- glht(anova_one_way_area, linfct = mcp(TreatmentConditions = "Dunnett"))
summary(dunnett_area)

# Run the multiple comparison with Tukey's post-hoc correction to compare all conditions with each other.
tukey_AR <- glht(anova_one_way_AR, linfct = mcp(TreatmentConditions = "Tukey"))
summary(tukey_AR)

tukey_circ <- glht(anova_one_way_circ, linfct = mcp(TreatmentConditions = "Tukey"))
summary(tukey_circ)

tukey_area <- glht(anova_one_way_area, linfct = mcp(TreatmentConditions = "Tukey"))
summary(tukey_area)