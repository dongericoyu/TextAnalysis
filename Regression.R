if (!require(checkpoint)) install.packages("checkpoint")
checkpoint("2017-12-22")

library(checkpoint)
library(haven)
library(readr)
library(stringr)
library(ggplot2)
library(mi)
library(betareg)
library(truncnorm)
library(lme4)
library(nlme)
library(mitools)
library(interplot)
library(dotwhisker)
library(broom)
library(dplyr)
library(purrr)
library(stargazer)
library(qwraps2)
library(readxl)
all_sentiment_mean <- read_excel("all_sentiment_mean.xlsx")
View(all_sentiment_mean)


#1.merged_pre2017
summary(all_sentiment_mean)

#(1)base model: The models are estimated using multilevel linear regression of individuals nested in provinces,
#with both the intercept and the coefficient for pro_just allowed to vary across the province
pre2017_gop<-glm(pre_2017 ~ male + age + gop + seniority + reelect_pressure, data=sentiment_pre17)
summary(pre2017_gop)

pre2017_ideo<-glm(pre_2017 ~ male +  age + ideology  + reelect_pressure, data=sentiment_pre17)
summary(pre2017_ideo)


post2017_gop<-glm(post_2017 ~ male + age + gop + seniority + reelect_pressure, data=sentiment_post17)
summary(post2017_gop)

post2017_ideo<-glm(post_2017 ~ male + age + ideology + seniority + reelect_pressure, data=sentiment_post17)
summary(post2017_ideo)


summary(sentiment_pre17)
summary(sentiment_post17)


table_1<-stargazer(pre2017_gop, pre2017_ideo, post2017_gop, post2017_ideo,  title="Linear Regression (OLS) on Senators' Twitter Sentiment",
                           align=TRUE, dep.var.labels=c("Twitter Sentiment"),
                           covariate.labels=c(),
                           omit.stat=c("LL","ser","f"), no.space=TRUE)

"Male", "Age", "GOP","Ideology", "Seniority",
"Reelect_pressure"

par(mfrow=c(1,2))
x.scale <- c(-1, 1)d
coefplot(pre2017_gop, xlim=c(-0.1,0.1),main="影响推特情绪的因素(党派）", col.pts= "red")
coefplot(pre2017_ideo, xlim=c(-0.1,0.1), main="影响推特情绪的因素(意识形态）", col.pts= "blue")

par(mfrow=c(1,2))
x.scale <- c(-1, 1)d
coefplot(post2017_gop, xlim=c(-0.1,0.1),main="影响推特情绪的因素(党派）", col.pts= "red")
coefplot(post2017_ideo, xlim=c(-0.1,0.1), main="影响推特情绪的因素(意识形态）", col.pts= "blue")


#groupped IVS 
# Define order for predictors that can be grouped
reordered_vars_ideo2017 <- c("male", "gop","ideology", "age","seniority",
                             "reelect_pressure")






## ask Fred or Hu;

# Generate a tidy data frame
m_ideo2017_df <- rbind(tidy(pre2017) %>% mutate(model = "pre2017"),      # tidy results &
                       tidy(post2017) %>% mutate(model = "post2017")) %>%  # identify model.
  #by_2sd(mtcars) %>% #1/8/2017: Hu said that should delete by_2sd, since it is a rescaling function used in Andrew and Gelman paper;
  # rescale coefficients ##
  mutate(term = factor(term, levels = reordered_vars_ideo2017)) %>%  # make term a factor &
  group_by(model) %>% arrange(term) %>%                     # reorder
  relabel_predictors(c(male = "male",                      # relabel predictors
                       gop = "gop", 
                       ideology = "ideology", 
                       age = "age",
                       seniority = "seniority",
                       reelect_pressure = "reelect_pressure"))

m_ideo2017_df




# Save finalized plot to an object 
ideo2017_set_group<-dwplot(m_ideo2017_df, dodge_soze = 1) + 
  relabel_y_axis(c("sentiment_mean(before2017)", "Male", "Belief of Procedural Justice", "Nulla poena sine lege" )) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Figure 1. Regression Results of Trial Experiment 2017") +
  theme(plot.title = element_text(face="bold"),
        legend.justification=c(0, 0), legend.position=c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title = element_blank()) 

# Create list of brackets (label, topmost included predictor, bottommost included predictor)
three_brackets_2017 <- list( c("Treatment", "Experimental_Trial"), 
                             c("Demographic", "Male"),
                             c("Belief in Justice", "Belief of Procedural Justice", "Nulla poena sine lege"))

ideo2017_set_gplot <- ideo2017_set_group %>% add_brackets(three_brackets_2017)

# to save to file (not run)
grid.arrange(ideo2017_set_group)    # to display







##creating table by stargazer
table_ideo2017<-stargazer(demsupport_2017, pro_trial_2017, law_trial_2017, title="Regression Results of Court Trial Experiment 2017",
                          align=TRUE, dep.var.labels=c("Support for Democracy","Conditional Effect (1)", "Conditional Effect (2)"),
                          covariate.labels=c("Treatment", "Male", "Belief of Procedural Justice", "Nulla poena sine lege",
                                             "Procedural Justice*Treatment", "Nulla poena sine lege* Treatment"),
                          column.sep.width = "1pt", omit.stat=c("LL","ser","f"), no.space=TRUE)













#pre2017<-lmer(formula = sentiment ~ male +  gop + ideology+ (1 | id), 
#data = sentiment_pre17)


#str(terms(netizen_state))
#stopifnot(identical(terms(netizen_state,fixed.only= FALSE),
#                     terms(model.frame(netizen_state))))
#attr(terms(netizen_state,FALSE), "dataClasses")

#netizen_state_ML <- update(netizen_state, REML = FALSE)
#netizen_state2 <- lmer(formunla = trust_state ~ male + age + CCP + income + edu + poli_discussion + online_act + pro_just + (pro_just||province), sleepstudy)
#anova(netizen_state, netizen_state2)
#summary(netizen_state2)
