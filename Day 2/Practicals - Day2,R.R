# 1 Set-up ----
###------PRACTICALS 1
##----REGRESSION MODELLING---------
# my home Directory
path <- "C:/Users/SimpleNick/Documents/NPC/NIG_POP_Workshop/Nig_POP/Day\ 2"
setwd(path)
getwd()

install.packages("plotly")
##----Simple regression
#-- Load data
simple <- read.csv("simple_reg.csv")
names(simple)
head(simple)
head(simple, 20)

library(psych)
headTail(simple) # view both first 4 and last 4 rows

# # install required packages
# installed.packages("ggdag")
# installed.packages("plotly")
# installed.packages("RColorBrewer")
# installed.packages("kableExtra")
# installed.packages("here")
# installed.packages("parallel")
# installed.packages("sf")
# installed.packages("raster")
# installed.packages("units")
# installed.packages("tidyverse")
# installed.packages("tmap")
# installed.packages("rstan")

png("simple_scatter.png")
plot(simple$income, simple$happiness,
    xlab = "Income", ylab = "Happiness",
    col = c("goldenrod", "purple"),
    pch = "0")
abline(a = 0, b = 1, lwd = 3.5)
abline(v = 4)

##----Check for linearity and Normality
with(simple, plot(happiness ~ income))
hist(simple$happiness)
###
fit <- lm(happiness ~ income, data = simple)
summary(fit)
plot(fit)

#--: happiness = 0.204 + 0.713*income
png("simple_histogram_both.png")
par(mfrow = c(1, 2))
hist(simple$happiness, xlab = "Happiness", col = c("goldenrod"), main = "")
abline(a = 0, b = 1)
abline(v = 3.5, h = 30, lwd = 2.5)

hist(simple$happiness, xlab="Income", col = c("purple"))
abline(a = 0, b = 1)
abline(v = 3.5, h = 30, lwd = 2.5)

hist(simple$happiness, xlab="Income", col = c("purple"))
abline(a = 0, b = 1)
abline(v = mean(simple$happiness), lwd = 2.5, col = c("white"))
abline(h = 30, lwd = 2.5, col = c("red"))

# check for normality
par(mfrow = c(1, 2))
# par(mfrow = (c(2, 1)))
hist(simple$happiness, main = "")
hist(simple$income, main = "")


# Regression Model Equation
# Nominal:
  # Happiness = intercept + income # nolint
  # intercept is the minium happiness someone can have


(yy <- 0.204 + 0.714 * 100000)
# Linear Regression
fit <- lm(happiness ~ income, data = simple)
summary(fit)
plot(fit)

fit_b <- lm(income ~ happiness, data = simple)
summary(fit_b)
plot(fit_b)


##----Multiple regression
#--Load data
multiple <- read.csv("multi_reg.csv")
names(multiple)
fit2 <- lm(heart.disease ~ biking + smoking, data = multiple)
summary(fit2)

headTail(multiple)

par(mfrow = c(1, 1))
plot(multiple$heart.disease, multiple$smoking,
     xlab = "Heart Disease", ylab = "Smoking",
     col = c("blue", "red"), pch = "9")

plot(multiple$heart.disease, multiple$biking,
     xlab = "Heart Disease", ylab = "Smoking",
     col = c("gold", "purple"), pch = "9"
    )

par(mfrow = c(2, 2))
plot(fit2)
par(mfrow = c(1, 1))

#--: 
# load libraries
library(tidyverse) # managing data
library(ggplot2) #visualization
library(ggdag) # drawing DAG
library(kableExtra) # visualising table
library(here) # handling path
library(rstan) # running Bayesian models


# draw linear model DAG
dagify(
  Y ~ mu,
  Y ~ sigma,
  mu ~ alpha,
  mu ~ beta,
  mu ~ X,
  outcome = "Y"
) %>%
  tidy_dagitty(seed=11) %>% 
  mutate(color=c('data','parameter',  'parameter','parameter',  'parameter','data')) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "grey20",size=6,  parse=T) +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Graph of a linear regression', color='', shape='')

# stan setup
options(mc.cores = parallel::detectCores()) #set up the maximum number of cores used by stan
rstan::rstan_options(auto_write = TRUE) # speed up running time 



# 2 Simulated data ----

# Simulate data
seed <- 2004
set.seed(seed)
data <- tibble(y=rnorm(1e3, mean= 5, sd=50))

# plot simulated data
ggplot(data, aes(x=y))+
  geom_histogram(bins = 50)+
  theme_minimal()+
  geom_vline(xintercept = mean(data$y), color='orange', size=1)+
  annotate('text', x=15, y=30, label=paste0('Observed mean: ', round(mean(data$y),2)), colour='orange', angle=90)+
  geom_segment(x=0,y=1, xend=sd(data$y), yend=1, colour='orange', size=1)+
  annotate('text', x=50, y=4, label=paste0('Observed sd: ', round(sd(data$y),2)), colour='orange')+
  labs(y='', x='observed y')+
  theme(axis.text.y = element_blank())

# Normal model DAG
dagify(
  Y ~ mu,
  Y ~ sigma) %>%
  tidy_dagitty(seed=41) %>% 
  mutate(color=c('parameter', 'parameter','data')) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend, color=color,shape=color)) +
  geom_dag_point() +
  geom_dag_edges() +
  geom_dag_text(col = "grey20",size=6,  parse=T) +
  scale_shape_manual(values=c(15,19))+
  theme_dag()+ labs(title = 'Model with simulated data: Normal distribution of Y', color='', shape='')


# define mu prior
data$prior_mu <- rnorm(1e3, mean= 0, sd=100)

ggplot(data)+
  geom_histogram(aes(x=y, after_stat(density)), bins=50, fill='grey30')+
  geom_density(aes(x=prior_mu), colour='#00BFC4', size=1)+
  theme_minimal()+
  annotate('text', y=0.0020, x=250, label=paste0('Normal(0,100)'), colour='#00BFC4', size=5)+
  theme(axis.text.y = element_blank())+
  labs(y='', x='observed y')

# define sigma prior
data$prior_sigma <- runif(1e3,min = 0, max = 200)

ggplot(data)+
  geom_histogram(aes(x=y, after_stat(density)), bins=50, fill='grey30')+
  geom_density(aes(x=prior_sigma), colour='#00BFC4', size=1, trim=T)+
  theme_minimal()+
  annotate('text', y=0.0060, x=150, label=paste0('Uniform(0,200)'), colour='#00BFC4', size=5)+
  theme(axis.text.y = element_blank())+
  labs(y='', x='observed y')



# prepare data for stan
stan_data <- list(
  y = data$y,
  n = nrow(data))

# mcmc settings
chains <- 4
warmup <- 250
iter <- 500

# parameters to monitor
pars <- c('mu','sigma')

# mcmc
fit <- rstan::stan(file = file.path('tutorial1_model.stan'), 
                   data = stan_data,
                   iter = warmup + iter, 
                   chains = chains,
                   warmup = warmup, 
                   pars = pars,
                   seed = seed)

# plot trace
stan_trace(fit, inc_warmup = T)

# summarise estimated parameters
estimated <- summary(fit, pars=pars)$summary

estimated %>% kbl() %>% kable_minimal()

# plot estimated parameters
mean_pop <- mean(data$y)
sd_pop <- sd(data$y)


mu_plot <- stan_plot(fit, 'mu',fill_color='orange')+
  annotate('segment',x=mean_pop, xend=mean_pop, 
           y=0.7, yend=1.2,col='grey40', size=1)+
  annotate('text',x=mean_pop, 
           y=1.5, col='grey40',label= paste0('Observed\n',round(mean_pop,2)), fontface =2, size=4.5)+
  annotate('segment',x=5, xend=5, 
           y=0.7, yend=1.2,col='grey10', size=1)+
  annotate('text',x=5, 
           y=1.5, col='grey10',label= paste0('True\n',5), fontface =2, size=4.5)+
  annotate('text',x=estimated['mu', 'mean'], 
           y=1.5, col='orange',label= paste0('Estimated\n',round(estimated['mu', 'mean'],2)), fontface =2, size=4.5)

sigma_plot <- stan_plot(fit, 'sigma', fill_color='orange')+
  annotate('segment',x=sd_pop, xend=sd_pop, 
           y=0.7, yend=1.2,col='grey40', size=1)+
  annotate('text',x=sd_pop, 
           y=1.5, col='grey40',
           label= paste0('Observed\n', round(sd_pop,2)), fontface =2, size=4.5)+
  annotate('segment',x=50, xend=50, 
           y=0.7, yend=1.2,col='grey10', size=1)+
  annotate('text',x=50, 
           y=1.5, col='grey10',label= paste0('True\n',50), fontface =2, size=4.5)+
  annotate('text',x=estimated['sigma', 'mean'], 
           y=1.5, col='orange',label= paste0('Estimated\n',round(estimated['sigma', 'mean'],2)), fontface =2, size=4.5)

gridExtra::grid.arrange(mu_plot, sigma_plot, nrow=2)
