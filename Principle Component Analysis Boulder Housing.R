# PCA analysis for boulder housing data
# PCA is capturing information through variance

# read data
rm(list = ls())
setwd("~/Downloads/5415 Advanced Data Analytics/Boulder Housing")

load("boulder-cleaned.RData")
boulder.clean <- na.omit(boulder.clean)

# remove several columns from the data
boulder.clean1 <- boulder.clean[, !names(boulder.clean) %in% c("HOME.TYPE", 
                                                               "ADDRESS", "ZIP", "LIST.PRICE", "PARKING.TYPE")]

# Perform principle component analysis scale=TRUE specifies that the
# variables should be scaled, which is usually a good idea
boulder.pr <- prcomp(boulder.clean1, scale = TRUE)


# This plot shows the percentage of variations explained by each
# principle component.  This is a way to examine the importance of each
# principle component.  In follow-up analysis, you may not want to
# include all principle components, but only a few important ones
plot(summary(boulder.pr)$importance[2, ], xlab = "Principle Component", 
     ylab = "Proportion of Variance")

# perform linear regression on the principle components
boulder.clean2 <- cbind(LIST.PRICE = boulder.clean$LIST.PRICE, 
                        data.frame(boulder.pr$x))
lm.fit <- lm(LIST.PRICE ~ ., data = boulder.clean2)
summary(lm.fit)