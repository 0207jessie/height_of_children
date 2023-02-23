# Report ---------------------------------

# Question of Interest: Is there any relationship between the height of parents and their children. 
# If so, does this relationship differ depending on the child’s gender?

height <- read.csv("childheights.csv")

# Is there any relationship between the height of parents and their children -----------------------------


# Exploratory analysis ---------------------------

summary(height$childHeight) #summary statistics 

plot(childHeight ~ midparentHeight, data = height_2, ylab = "Height of Child (in inches)", xlab = "Mid-parent Height (in inches)",
     sub = "Figure 1: Scatterplot of midparentHeight versus childHeight")


# Statistical analysis --------------------------

Model1 <- lm(childHeight ~ midparentHeight, data = height) # Simple linear model
summary(Model1) # Table 1: Linear Model1 output from R

plot(childHeight ~ midparentHeight, data = height_2, ylab = "Height of Child (in inches)", xlab = "Mid-parent Height (in inches)",
     sub = "Figure 2: Scatterplot of midparentHeight versus childHeight with fitted line from Model1 superimosed")
abline(Model1)


# Assumptions ----------------------------------

plot(rstandard(Model1) ~ fitted(Model1), xlab = " Fitted values", ylab = " Standardised ressiduals", sub = " Figure 3: Standardised residuals versus fitted values from Model1")
abline(h=0, lty=2, lwd=3) # Checking for mean zero and constant variance 

hist(rstandard(Model1), xlab = "Standardised residuals", sub = "Figure 4: Histogram of the standardised residuals obtained from Model1",
     main = "Histogram of Standardised Residuals" ) # Checking if errors come from a normal distribution

qqnorm(rstandard(Model1), sub = " Figure 5: Normal probability (Q-Q) plot from Model1") # Checking if errors come from a normal distribution
abline(a=0, b = 1)

#2 ----------------------------------------------------------------------------------------------------------------------------------------

  
# Does this relationship differ by gender  -----------------------------------------

height_2 <- height
Gender <- height_2$gender
Gender <- as.factor(Gender) # R knows that gender is a factor
Gender <- as.numeric(Gender) # matrix form

tapply(height_2$childHeight, height_2$gender, summary) # Summary statistics by factor gender


# Exploratory analysis 2 -----------------------------

plot(childHeight ~ midparentHeight, data = height_2, ylab = "Height of Child (in inches)", xlab = "Mid-parent Height (in inches)",
     sub = "Figure 6: Scatterplot of midparentHeight versus childHeight with gender highlighted", 
     col = Gender, cex = 1, shape = Gender, pch = Gender)
legend("topleft", legend = c("Female", "Male"), horiz = TRUE, bty = "n",cex = 1.2, col = 1:2, pch = 1:2)


# Statistical analysis 2 --------------------------------------

Model2 <- lm(childHeight ~ midparentHeight * gender, data = height) # Model between child's height and parents' height different for gender differences

anova(Model2) # Table 2: Analysis of Variance Table for Model2
# Interaction term not statistically significant (0.3144), so we now fit model which assumes parallel lines

Model3 <- lm(childHeight ~ midparentHeight + gender, data = height)

anova(Model3) # Table 4: Analysis of Variance Table for Model3
# The factor gender is statistically significant, this will be the final model

summary(Model3) # Table 5: Linear Model3 output from R

augmented_Model3 <- augment(Model3)
glimpse(augmented_Model3)

data_space_2 <- ggplot(augmented_Model3, aes(x = midparentHeight, y = childHeight, colour = gender))+
  geom_point() 
data_space_2 + geom_line(aes(y = .fitted)) # Figure 7: Scatterplot of midparentHeight versus childHeight with gender highlighted and Model 3 superimposed 


# Assumptions 2 ---------------------------------------------------------

plot(rstandard(Model3) ~ fitted(Model3), xlab = " Fitted values", ylab = " Standardised ressiduals", sub = " Figure 8: Standardised residuals versus fitted values from Model3")
abline(h=0, lty=2, lwd=3) # Checking for mean zero and constant variance 

hist(rstandard(Model3), xlab = "Standardised residuals", sub = "Figure 9: Histogram of the standardised residuals obtained from Model3",
     main = "Histogram of Standardised Residuals" ) # Checking if errors come from a normal distribution

qqnorm(rstandard(Model3), sub = " Figure 10: Normal probability (Q-Q) plot from Model3") # Checking if errors come from a normal distribution
abline(a=0, b = 1)








 
