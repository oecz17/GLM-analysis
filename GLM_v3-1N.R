#CODE FOR PROJECT 2
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("foreach")
#install.packages("xlsx")

source("multiplot.R")

library(ggplot2)
library(foreach)
library(xlsx)
setwd("D:/KTH/Regression Analysis/Project2")
######################### Section 1: Read data #########################

# This is where GLM data is read form tractors.csv into a table in R 

glmdata <- read.table("Tractors.csv", header=TRUE, sep=";", dec="," )

##### Base rate estimation

# yearly costs
x = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
year_costs = c()
for (val in x){
  cost = sum(subset(glmdata, RiskYear == val)$ClaimCost)
  year_costs = c(year_costs, cost)
}
avg_year_cost = mean(year_costs)
year_premium_target = avg_year_cost/0.9

data_2016 = subset(glmdata, RiskYear == 2016)
cost_2016 = sum(subset(glmdata, RiskYear == 2016)$ClaimCost)
perclaim_2016 = cost_2016 / sum(subset(glmdata, RiskYear == 2016)$NoOfClaims)
pertime_2016 = cost_2016 / sum(subset(glmdata, RiskYear==2016)$Duration)

hist(glmdata$Weight[glmdata$Weight<420 && glmdata$Weight>50],200)

data_2016$weight_group <- cut(data_2016$Weight, 
                              breaks = c(-Inf, 420, 800, 1500, 2900, 4300, Inf), 
                              labels = c("01_<420", "02_420-800kg", "03_800-1500-kg", "04_1500-2900kg", '05_2900-4300kg', '06_>=4300kg'), 
                              right = FALSE)

data_2016$age_group <- cut(data_2016$VehicleAge, 
                           breaks = c(-Inf, 6, 9, 17, Inf), 
                           labels = c("01_<4years", "02_4-9years", "03_9-17years", "04_>17years"), 
                           right = FALSE)

data_2016_agg <- aggregate(data_2016[,6:8],by=list(weight_group = data_2016$weight_group,
                                                   age_group = data_2016$age_group,
                                                   Climate = data_2016$Climate,
                                                   ActivityCode = data_2016$ActivityCode), FUN=sum, na.rm=TRUE)



######################### Section 2: Create groups & aggregate data #########################

# First, any continuous variable needs to be grouped into discrete groups 
# The code below groups the variable weight, from table glmdata, into six groups, and stores this in a new column, weight_group 
# It also groups the variable VehicleAge into four groups

hist(glmdata$Weight,200)

glmdata$weight_group <- cut(glmdata$Weight, 
                       breaks = c(-Inf, 420, 800, 1500, 2900 ,4300 , Inf), 
                       labels = c("01_<420", "02_420-800kg", "03_800-1500-kg", "04_1500-2900kg", '05_2900-4300kg', '06_>=4300kg'), 
                       right = FALSE)

table(glmdata$weight_group)

hist(glmdata$VehicleAge,200)

glmdata$age_group <- cut(glmdata$VehicleAge, 
                            breaks = c(-Inf, 6, 9, 17, Inf), 
                            labels = c("01_<4years", "02_4-9years", "03_9-17years", "04_>17years"), 
                            right = FALSE)

table(glmdata$age_group)

# Secondly, we aggregate the data.
# That is, instead of having one row per tractor, we want one row for each existing combination of variables 
# This code aggregates columns 6-8 of glmdata, by three variables: weight_group, Climate, and ActivityCode 
# Tha aggregated data is stored in a new table, glmdata2 

glmdata2 <- aggregate(glmdata[,6:8],by=list(weight_group = glmdata$weight_group, 
                                            Climate = glmdata$Climate,
                                            ActivityCode = glmdata$ActivityCode,
                                            age_group = glmdata$age_group), FUN=sum, na.rm=TRUE)

# We then do some preparation for the output the GLM function will give.
# This piece of code creates a new table, glmdata3, with a row per variable and group, and with data on the total duration corresponding to this group.


glmdata3 <-
  data.frame(rating.factor =
               c(rep("Weight", nlevels(glmdata2$weight_group)),
                 rep("Climate", nlevels(glmdata2$Climate)),
                 rep("ActivityCode", nlevels(glmdata2$ActivityCode)),
                 rep("Age", nlevels(glmdata2$age_group))),
             class =
               c(levels(glmdata2$weight_group),
                 levels(glmdata2$Climate),
                 levels(glmdata2$ActivityCode),
                 levels(glmdata2$age_group)),
             stringsAsFactors = FALSE)

new.cols <-
  foreach (rating.factor = c("weight_group", "Climate", "ActivityCode", "age_group"),
           .combine = rbind) %do%
           {
             nclaims <- tapply(glmdata2$NoOfClaims, glmdata2[[rating.factor]], sum)
             sums <- tapply(glmdata2$Duration, glmdata2[[rating.factor]], sum)
             n.levels <- nlevels(glmdata2[[rating.factor]])
             contrasts(glmdata2[[rating.factor]]) <-
               contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
             data.frame(duration = sums, n.claims = nclaims)
           }
glmdata3 <- cbind(glmdata3, new.cols)
rm(new.cols)

######################### Section 3: GLM analysis #########################

# The GLM analysis. It is performed using R's built in GLM function 

# First, we model the claims frequency. 
# The first part of this performs a GLM analysis, with glmdata2 as the data source modelling NoOfClaims, by the Duration. It looks at four variables: weight_group, Climate, age and ActivityCode.

model.frequency <-
  glm(NoOfClaims ~ weight_group + Climate + ActivityCode + age_group + offset(log(Duration)),
      data = glmdata2, family = poisson)

summary(model.frequency)

# Then we save the coefficients resulting from the GLM analysis in an array

rels <- coef(model.frequency)
rels <- exp(rels[1] + rels[-1])/exp(rels[1])

# Finally, we attach the coefficients to the already prepared table glmdata3, in a column named rels.frequency
# There is no good way of doing this automatically, so we need to do some manual tricks
# This code creates a vector with 6 positions consisting of the integer 1, and then positions number 1-5 in the rels array.
# Then it attaches this to rows 1-6 of glmdata3, sorted from highest to lowest duration, since the GLM data is on this form.
# In other words, the code takes the GLM coefficients for the six weight groups and saves those in glmdata3, in the rows corresponding to those groups.
# After that, it does the same thing for the rest of the GLM coefficients, belonging to climate and activity code vairables.


variableLevels <- c(nlevels(glmdata2[["weight_group"]]),
                 nlevels(glmdata2[["Climate"]]),
                 nlevels(glmdata2[["ActivityCode"]]),
                 nlevels(glmdata2[["age_group"]]))

cs <- cumsum(variableLevels)
cs_rels <- cs
for(i in 1:length(variableLevels)){
  cs_rels[i] <- cs[i]-i
}

attachRels <- function(rels_vec, vector, cs, cs_rels) {
  c(c(1, rels_vec[ 1 : cs_rels[1] ])[rank(-vector[ 1 : cs[1] ], ties.method = "first")],
    c(1, rels_vec[ (cs_rels[1]+1) : cs_rels[2] ])[rank(-vector[ (cs[1]+1) : cs[2] ], ties.method = "first")],
    c(1, rels_vec[ (cs_rels[2]+1) : cs_rels[3] ])[rank(-vector[ (cs[2]+1) : cs[3] ], ties.method = "first")],
    c(1, rels_vec[ (cs_rels[3]+1) : cs_rels[4] ])[rank(-vector[ (cs[3]+1) : cs[4] ], ties.method = "first")])
}

#glmdata3$rels.frequency <- attachRels(rels, glmdata3$duration, cs, cs_rels)

glmdata3$rels.frequency <-
  c(c(1, rels[1:4])[rank(-glmdata3$duration[1:5], ties.method = "first")], # 5 weight groups
    c(1, rels[5:8])[rank(-glmdata3$duration[6:10], ties.method = "first")], # 5 age groups
    c(1, rels[9:10])[rank(-glmdata3$duration[11:13], ties.method = "first")], # 3 climate guorps
    c(1, rels[11:20])[rank(-glmdata3$duration[14:24], ties.method = "first")]) # 11 activity groups


# We then do the same thing again, now modelling severity instead of claim frequency.
# That means that, in this part, we want to look at the average claim. So first, we calculate the average claim for each row in glmdata2

glmdata2$avgclaim=glmdata2$ClaimCost/glmdata2$NoOfClaims

# Then we do the same thing as we did when modelling claims frequency, but we look at average claim;
# A GLM analysis is run, the coefficients stored, and saved in a new column, named rels.severity, glmdata3

model.severity <-
  glm(avgclaim ~ weight_group + Climate + ActivityCode + age_group ,
      data = glmdata2[glmdata2$avgclaim>0,], family = Gamma("log"), weight=NoOfClaims)

#rels <- coef(model.severity)
#rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
#glmdata3$rels.severity <- attachRels(rels, glmdata3$duration, cs, cs_rels)


rels <- coef(model.severity)
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
glmdata3$rels.severity <-
  c(c(1, rels[1:4])[rank(-glmdata3$duration[1:5], ties.method = "first")], # 5 weight groups
    c(1, rels[5:8])[rank(-glmdata3$duration[6:10], ties.method = "first")], # 5 age groups
    c(1, rels[9:10])[rank(-glmdata3$duration[11:13], ties.method = "first")], # 3 climate guorps
    c(1, rels[11:20])[rank(-glmdata3$duration[14:24], ties.method = "first")]) # 11 activity groups

# Finally, the final risk factor is calculated, as the product of the frequency and severity factors. 
glmdata3$rels.risk <- with(glmdata3, rels.frequency*rels.severity)

######################### Section 4: Plotting #########################

# In this section, the results from the GLM are plotted.

# First, long variable names need to be cut, to fit into the plots.
# This row of code cuts away everything except for the first letter for variable names belonging to activity codes.
glmdata3[glmdata3$rating.factor == "ActivityCode",2] <- substr(glmdata3$class,1,1)[10:20]  


# Then the results are plotted. This code plots the GLM factors for frequency, severity, and total risk, for the three variables Weight, Climate, and Activity code.

p1 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.frequency)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: frequency factors") +
      geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=1) +theme(axis.text.x = element_text(angle = 30, hjust = 1))

p2 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.severity)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: severity factors") +
      geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p3 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.risk)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: risk factors") +
      geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=1.6)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p4 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.05)

p5 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.1)

p6 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.1)

p7 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) 

p8 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)

p9 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)

p10 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) 

p11 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)

p12 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)



multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, cols=4)

# output table
glm_out = glmdata3[,-2]


######################## Model Validation
lrtest(model.frequency)
lrtest(model.severity)

######################### Section 5: Export factors to Excel #########################

#As a last step, the risk factors are exported to excel. 
# The dopcument will be saved in the folder set as your working directory.

write.xlsx(glmdata3, "glmfactors.xlsx")

write.xlsx(data_2016_agg, "data2016.xlsx")
