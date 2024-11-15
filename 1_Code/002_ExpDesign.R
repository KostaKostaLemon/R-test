
# load Packeges -----------------------------------------------------------

library(ggplot2)# plotting
library(dplyr)# data management and summary statistics
library(ggpubr)# plotting
library(agricolae)# experimental design
library(openxlsx)# import and export Excel files
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Code --------------------------------------------------------------------

sample_sizes=c(2,3,5,10,20) # Different sample sizes to test
treatment_effects <- c(5)  # Effect size to test
std_devs <- c(5)         # Standard deviations to test

# Generate one example dataset per combination for visualization
example_data <- do.call(rbind, lapply(sample_sizes, function(n) {
  do.call(rbind, lapply(std_devs, function(sd) {
    do.call(rbind, lapply(treatment_effects, function(effect) {
      
      # Generate data for control and treatment groups
      control <- rnorm(n, mean = 20, sd = sd)
      treatment <- rnorm(n, mean = 20 + effect, sd = sd)
      
      # Combine control and treatment data into a single data frame
      data.frame(
        group = rep(c("Control", "Treatment"), each = n),
        value = c(control, treatment),
        sample_size = n,
        sd = sd,
        effect = effect
      )
    }))
  }))
}))

# Plot the example data for each combination
ggplot(example_data, aes(x = group, y = value, fill = group)) +
  geom_point(shape=3, size=1)+
  stat_summary(color = "#00BA38", fun = mean, geom = "point",  size = 2, position = position_nudge(x =0.2)) +
  stat_compare_means(method = "t.test", label.y.npc  =0.9) +
  facet_grid(sd ~ sample_size, labeller = labeller(
    sd = function(x) paste("SD =", x),
    sample_size = function(x) paste("Sample size =", x)
  )) +
  theme_bw() +
  theme(legend.position = "none")

# Experimental Design - Agricolae ------
trt=c("A","B","C","D","E")
outdesign.crd <-design.crd(trt,r=4, serie=1, seed=1234)
book.crd<-outdesign.crd$book # field book
head(book.crd)
#book.crd


# Ausmaße des Versuchsfeldsdefinieren -------------------------------------
book.crd$col=rep(1:4, times=5)
book.crd$row=rep(1:5, each=4)
head(book.crd)


# plot Feldplan -----------------------------------------------------------

ggplot(data=book.crd, aes(x=col, y=row, fill=trt, label=trt))+
  geom_tile(colour=1)+
  geom_text()+
  geom_text(aes(label=plots-10), nudge_y = 0.4, nudge_x = -0.4, size=4)+ 
  scale_y_reverse()+
  theme(legend.position = "bottom")

# ANalyse des Designs mit linearem Model

mod <- lm(y ~ trt, data = dat)

# Analyse mit generalisisetem linearem Model

mod <- glm(y ~ trt, family = "poisson", data = dat)

# Randomized complete block design -----------------------------------------------------------
trt=c("A","B","C","D","E")
outdesign.rcbd <-design.rcbd(trt, r=4,serie=1, seed=1236, "Wichmann-Hill")
book.rcbd<-outdesign.rcbd$book# field book
head(book.rcbd)
###

book.rcbd$col=rep(1:5, 4)
book.rcbd$row=as.numeric(book.rcbd$block)
head(book.rcbd)

###
p.rcbd = ggplot(data=book.rcbd, aes(x=col, y=row, fill=trt, label=trt))+
  geom_tile(colour=1)+
  geom_text()+
  geom_text(aes(label=plots-10), nudge_y = 0.4, nudge_x = -0.4, size=4)+ 
  scale_y_reverse(breaks=1:4)+
  facet_grid(block~., scales="free", labeller = labeller(block=label_both))+
  theme(legend.position = "bottom")
p.rcbd

# Analyse as Lineares Model
mod <- lm(y ~ trt + block, data = dat)

# Analyse as GLM
mod <- glm(y ~ trt + block, family = "poisson", data = dat)

# If you have many blocks (more than 5 some say more than 8), block may be also treated as random effect in a mixed effect model. To fit these models you can use:
   library(glmmTMB) #for linear mixed effect model 
 mod<-glmmTMB(y ~ trt +  (1|block), data=dat) 
 
#or generalised linear mixed effect model 
 mod<-glmmTMB(y ~ trt + (1|block), data=dat, family="poisson") 
 
 #or
 library(lme4) 
mod <- lmer(y ~ trt + (1|block), data=dat) #and 
mod <- glmer(y ~ trt + (1|block), data=d, family="poisson")

 
 # These models assume a single measurement per treatment replication. However, if multiple measurements are taken within each replication (e.g., counting the number of aphids on 10 plants per plot), you have two options:

       #   Aggregate the data: 
# You can calculate summary statistics, like the mean or sum, per plot to represent a single value for each replication.

        # Account for pseudoreplications: 
# Alternatively, if you wish to retain the individual measurements, you can account for pseudoreplications by including a random effect for each plot, representing the level at which measurements occur without randomization. The mixed effect model would be: 
 
 mod <- glmmTMB(y ~ trt + block + (1|PlotID), data=dat) 
 #or 
 mod <- lmer(y ~ trt + block + (1|PlotID), data=dat) 
 #with PlotID having 20 levels.
 # 
 # This random effect accounts for the variability within plots and prevents pseudoreplications from inflating the sample size and violating the independence assumption. 


# Splitplot# --------------------------------------------------------------

 geno<-c("g1","g2","g3","g4")
 N<-c("low","med","high")
 outdesign <-design.split(geno,N,r=4,serie=2,seed=415,kinds ="Super-Duper")
 book<-outdesign$book# field book
 book$col=rep(1:4, each=3)
 book$row=rep(1:3)
 book$PlotID=1:length(book$row)
 head(book) 

 # Plot
 ggplot(data=book, aes(x=col, y=row, fill=geno, label=N))+
   geom_tile(colour=1)+
   geom_text()+
   scale_y_reverse(breaks=1:4)+
   geom_text(aes(label=PlotID), nudge_y = 0.3, nudge_x = -0.3, size=3)+ 
   facet_wrap(~block, scales="free",
              labeller = labeller(block=label_both))+
   theme(legend.position = "bottom")+
   guides(fill = guide_legend(nrow = 1)) 

# combine main- and subplot variables
 book$mainplot=factor(paste(book$block, book$geno, sep="."))

# aaand plot
 ggplot(data=book, aes(x=col, y=row, fill=factor(plots), label=mainplot))+
   geom_tile(colour=1)+
   geom_text()+
   scale_y_reverse(breaks=1:4)+
   geom_text(aes(label=PlotID), nudge_y = 0.3, nudge_x = -0.3, size=3)+ 
   facet_wrap(~block, scales="free",
              labeller = labeller(block=label_both))+
   theme(legend.position = "bottom")+
   guides(fill = guide_legend(nrow = 1))

 
# ANALYZE NEstedt
 library(lme4)
 mod<-lmer(y ~ geno * N + block +  (1|mainplot), data=dat) 

 mod<-glmer(y ~ geno * N + block +  (1|mainplot), data=d, family="poisson")
 

# Export field book -------------------------------------------------------

 # You can export the field book and the plot of the design using library(openxlsx). 
 
 wb <- createWorkbook()
 addWorksheet(wb, sheetName = "book.rcbd")
 writeData(wb, "book.rcbd", book.rcbd) 
 addWorksheet(wb, sheetName = "rcbd.plot", gridLines = FALSE)
 print(p.rcbd)
 insertPlot(wb, "rcbd.plot", xy=c("B", 2), width = 20, height = 15, fileType = "png", units = "cm") ## 
 saveWorkbook(wb, file = "rcbd_design.xlsx", overwrite = TRUE)
 