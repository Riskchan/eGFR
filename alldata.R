library(MASS)
library(tikzDevice)
library(ggplot2)

# LaTeX labeling
latex_labels <- function(x){
  if(length(x)==0) 
    return(character())
  paste0("$", x, "$")
}

setwd("/Users/Akihiko 1/Dropbox/2020/RCB/data")

# Data loading  
data <- read.csv("alldata.csv", header=T, stringsAsFactors=T)
str(data)

selected_data <- data[data$BMI<20,]
str(selected_data)

################################################################################
# BMI analysis
################################################################################
# Linear regression
model <- lm(Diff~BMI+Age+Sex+Cr, data=selected_data)
summary(model)

# Stepwise regression
res <- step(model, direction="both", k=log(nrow(selected_data)))
summary(res)

# Plot
g <- ggplot(data, aes(x=BMI, y=Diff, shape=Sex, color=Sex))
g <- g + geom_point() + scale_shape_manual(values=c(1,2)) 
g <- g + geom_abline(slope = res$coefficients[2], intercept = res$coefficients[1], size = 0.3)
g <- g + annotate("text", x=28, y=85, label=paste("$y=", round(res$coefficients[2], 2) , "x+" , round(res$coefficients[1], 2), "$" ))
g <- g + annotate("text", x=28, y=65, label=paste("$R^2=", round(summary(res)$r.squared, 3), "$"))
g <- g + theme_bw()
g <- g + theme(legend.position = c(0.8,0.8), legend.background = element_rect(fill = "white", colour = "black", size=0.3))
g <- g + labs(x="BMI (kg/m$^2$)", y="$\\Delta$eGFR (mL/min/1.73m$^2$)")
g <- g + scale_x_continuous(labels = latex_labels) + scale_y_continuous(labels = latex_labels)
plot(g)

# tikzDevice output
#tikz("../tex/alldata.tex", width=4, height=4)
#print(g)
#dev.off()

################################################################################
# Age analysis
################################################################################
# Plot
g <- ggplot(data, aes(x=Age, y=Diff, shape=Sex, color=Sex))
g <- g + geom_point() + scale_shape_manual(values=c(1,2)) 
g <- g + theme_bw()
g <- g + theme(legend.position = c(0.8,0.8), legend.background = element_rect(fill = "white", colour = "black", size=0.3))
g <- g + labs(x="Age (Year)", y="$\\Delta$eGFR (mL/min/1.73m$^2$)")
g <- g + scale_x_continuous(labels = latex_labels) + scale_y_continuous(labels = latex_labels)
plot(g)

# tikzDevice output
#tikz("../tex/agedata.tex", width=4, height=4)
#print(g)
#dev.off()


################################################################################
# Sex analysis
################################################################################

# Equivalence analysis
library("equivalence")
t.test(data[data$Sex=="F", ]$Diff, data[data$Sex=="M", ]$Diff)
tost(data[data$Sex=="F", ]$Diff, data[data$Sex=="M", ]$Diff, epsilon = 10)

# Plot
g <- ggplot(data, aes(x=Sex, y=Diff))
g <- g + geom_jitter(aes(color=Sex, shape=Sex))
g <- g + theme_bw() + scale_shape_manual(values=c(1,2)) + theme(legend.position = 'none')
g <- g + labs(x="Sex", y="$\\Delta$eGFR (mL/min/1.73m$^2$)")
g <- g + scale_y_continuous(labels = latex_labels)
plot(g)

# tikzDevice output
tikz("../tex/sexdata.tex", width=4, height=4)
print(g)
dev.off()

################################################################################
# Baseline Cr
################################################################################

# Plot
g <- ggplot(data, aes(x=Cr, y=Diff, shape=Sex, color=Sex))
g <- g + geom_point() + scale_shape_manual(values=c(1,2)) 
g <- g + theme_bw()
g <- g + theme(legend.position = c(0.8,0.8), legend.background = element_rect(fill = "white", colour = "black", size=0.3))
g <- g + labs(x="Cr (mg/dL)", y="$\\Delta$eGFR (mL/min/1.73m$^2$)")
g <- g + scale_x_continuous(labels = latex_labels) + scale_y_continuous(labels = latex_labels)
plot(g)

# tikzDevice output
#tikz("../tex/crdata.tex", width=4, height=4)
#print(g)
#dev.off()


