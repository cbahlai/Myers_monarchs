
# Load packages
library(glmmADMB)
library(bbmle)
library(arm)
library(lme4)
library(piecewiseSEM)
library(ggplot2)

#bring data in

data_aug2016<-read.csv(file="deployment2_2016_notsmooth_csv.csv", header=TRUE)

#make a plant patch column called 'patch'
data_aug2016<-within(data_aug2016, patch <- paste(block,treatment,exclosure_treatment, sep ='.'))

data_aug2016<-within(data_aug2016, surviving <- (total_all_stages/initial_count))

#drop turf and all but 72 hr obs 
data_aug2016_3day <- data_aug2016[ which(data_aug2016$hours_since_deployment == 71 & data_aug2016$treatment != 'turf'), ]

#####day3 survival###### eggs and caterpillars coming back to life not corrected#######

m0 = glmer(cbind(total_all_stages, initial_count) ~
             1 + (1|patch), family=binomial(), data=data_aug2016_3day)

m1 = glmer(cbind(total_all_stages, initial_count) ~
             treatment + (1|patch), family=binomial(), data=data_aug2016_3day)

m2 = glmer(cbind(total_all_stages, initial_count) ~
             exclosure_treatment + (1|patch), family=binomial(), data=data_aug2016_3day)

m3 = glmer(cbind(total_all_stages, initial_count) ~
             treatment + exclosure_treatment + (1|patch), family=binomial(), data=data_aug2016_3day)

m4 = glmer(cbind(total_all_stages, initial_count) ~
             treatment*exclosure_treatment + (1|patch), family=binomial(), data=data_aug2016_3day)

# check out AICc values (from pkg bbmle)
AICctab(m0, m1)

# Do plant treatments influence survival?
anova(m0, m1)

# Do exclosures treatments influence survival?
anova(m0, m2)

# Do habitat treatments and exclosure treatments both independently or interactively influence?
anova(m0, m1, m3, m4)
anova(m0, m2, m3, m4)
anova(m2, m4)
anova(m0, m4) #significance of interaction/fullmodel
summary(m4)



# Plotting
# FIGURE 1a
display(m4)
summary(m4)
o = sim(m4, n.sims=10000)
colMeans(fixef(o))


m4.CI = data.frame(
  x = rep(c('bare', 'corn', 'prairie', 'soy'), 3),
  exclosure_treatment = rep(c('closed', 'open', 'sham'), each=4),
  coefs = c(plogis( mean( o@fixef[,1] ) ), 
            plogis( mean( o@fixef[,1] + o@fixef[,2] ) ),
            plogis( mean( o@fixef[,1] + o@fixef[,3] ) ),
            plogis( mean( o@fixef[,1] + o@fixef[,4] ) ),
            plogis( mean( o@fixef[,1] + o@fixef[,2] + o@fixef[,3] + o@fixef[,5] ) ),
            plogis( mean( o@fixef[,1] + o@fixef[,2] + o@fixef[,4] + o@fixef[,6] ) )
  ),
  lower = c(plogis( quantile( o@fixef[,1], 0.025 ) ), 
            plogis( quantile( o@fixef[,1] + o@fixef[,3], 0.025 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,4], 0.025 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,2], 0.025 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,2] + o@fixef[,3] + o@fixef[,5], 0.025 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,2] + o@fixef[,4] + o@fixef[,6], 0.025 ) )
  ),
  upper = c(plogis( quantile( o@fixef[,1], 0.975 ) ), 
            plogis( quantile( o@fixef[,1] + o@fixef[,3], 0.975 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,4], 0.975 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,2], 0.975 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,2] + o@fixef[,3] + o@fixef[,5], 0.975 ) ),
            plogis( quantile( o@fixef[,1] + o@fixef[,2] + o@fixef[,4] + o@fixef[,6], 0.975 ) )
  )
)

# effect sizes:
(m4.CI$coefs[2]-m4.CI$coefs[1]) / m4.CI$coefs[1]
(m4.CI$coefs[3]-m4.CI$coefs[1]) / m4.CI$coefs[1]


##make the figure
cols <- c("corn" = "gold2", "prairie" = "yellowgreen", "soy" = "mediumpurple", "bare" = "firebrick1", "turf" ="dodgerblue2" )
fig1a = ggplot(data_aug2016_3day, aes(x = treatment, y = total_all_stages/initial_count, colour = treatment))+
  geom_point(alpha=0.5, position = position_jitter(width=0.05, height=0.01)) +
  scale_color_manual(values=cols)+
  facet_grid(.~exclosure_treatment) + 
  labs(x = 'habitat treatment', y = 'survival') +
  geom_pointrange(data=m4.CI, aes(x = x, y = coefs, ymin=lower, ymax=upper), 
                  col = 1, size=0.5, position = position_nudge(x = -0.35), alpha=1) 

fig1a
