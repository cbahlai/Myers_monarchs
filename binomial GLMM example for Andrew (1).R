# Hi, Andrew! Here is some code showing how to run binomial GLMMs. This unfortunately
# won't actually run for you because there were a bunch of parts to this and the data are
# in a Google Sheets document. I didn't have time to clean and extract the data set and
# send it your way. But this will give you a view of what to do.





# Load packages
library(glmmADMB)
library(bbmle)
library(arm)
library(lme4)
library(piecewiseSEM)



# Load from Google Drive and prep data
source('~/Dropbox/CPB Field 2016/CPB pop dyn 2016 - data prep.r')


########## Day8 SURVIVAL #################################################################
# these are corrected so that beetles don't die and come back to life

m0 = glmer(cbind(cpbDay8Cor, cpbInitial.x - cpbDay8Cor) ~
	1 + (1|plotf), family=binomial(), data=dw4)

m1 = glmer(cbind(cpbDay8Cor, cpbInitial.x - cpbDay8Cor) ~
	plantTrtf + (1|plotf), family=binomial(), data=dw4)

m2 = glmer(cbind(cpbDay8Cor, cpbInitial.x - cpbDay8Cor) ~
	podiTrtf + (1|plotf), family=binomial(), data=dw4)

m3 = glmer(cbind(cpbDay8Cor, cpbInitial.x - cpbDay8Cor) ~
	podiTrtf + plantTrtf + (1|plotf), family=binomial(), data=dw4)

m4 = glmer(cbind(cpbDay8Cor, cpbInitial.x - cpbDay8Cor) ~
	podiTrtf * plantTrtf + (1|plotf), family=binomial(), data=dw4)


# check out AICc values (from pkg bbmle)
AICctab(m0, m1,m2,m3,m4)

# Do plant treatments influence survival?
anova(m0, m1)

# Does podi treatment influence survival?
anova(m0, m2)

# Do plant treatments and podi treatments both independently or interactively influence?
anova(m0, m1, m3, m4)
anova(m0, m2, m3, m4)
anova(m2, m4)
anova(m0, m4) # Use this test for significance of interaction/fullmodel

summary(m4)

# R squared ... Unfortunately the sem.model.fits function only works on data in the LONG
# format, so I had to reformat the data so there is one row per individual (with 0 or 1)
# That's what dw4.day8.LONG is. Then I rerun the model with the LONG data and calc the R2
m4LONG = glmer(BINARYSURVIVAL ~ podiTrtf * plantTrtf + (1|plotf), family='binomial', data=dw4.day8.LONG)
sem.model.fits(m4LONG)
# Note if these seems like a pain, I can show you other (easier!) ways to calc R2




# Plotting
# FIGURE 1a
display(m4)
summary(m4)
o = sim(m4, n.sims=10000)
colMeans(fixef(o))


m4.CI = data.frame(
	x = rep(c('K', 'Y', 'YK'),2),
	podi = rep(c('n', 'y'), each=3),
	coefs = c(plogis( mean( o@fixef[,1] ) ), 
		plogis( mean( o@fixef[,1] + o@fixef[,3] ) ),
		plogis( mean( o@fixef[,1] + o@fixef[,4] ) ),
		plogis( mean( o@fixef[,1] + o@fixef[,2] ) ),
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


fig1a = ggplot(dw4, aes(x = plantTrtf, y = survivalDay8Cor)) + geom_point(alpha=0.5, position = position_jitter(width=0.05, height=0.01)) +
	facet_grid(.~podi, labeller = labeller(podi = c('n' = 'control', 
		'y' = 'predation risk'))) + 
	labs(x = 'Plant treatment', y = 'Survival') +
	geom_pointrange(data=m4.CI, aes(x = x, y = coefs, ymin=lower, ymax=upper), 
	col = 1, size=0.5, position = position_nudge(x = -0.35), alpha=1) +  
	scale_x_discrete(labels=c('K' = 'R', 'Y' = 'S', 'YK' = 'Mix'))
