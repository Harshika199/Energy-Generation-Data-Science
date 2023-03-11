install.packages("readxl")
install.packages("janitor")
install.packages("corrplot")
install.packages("FactoMineR")
install.packages("factoextra")

library(readxl)
library(janitor)
library(corrplot)
library(FactoMineR)
library(factoextra)

df <- read_xlsx("/Users/mac/Desktop/GroupProject1/DailyNetGenerationByEnergySourceInNewEngland2.xlsx")
#Monthly averages
df$Day <- as.Date(df$Day) #Change date to date-class

# Get months:
df$Month <- months(df$Day)
df2 <- aggregate(cbind(df$`other (MWh)`, df$`wind (MWh)`, df$`solar (MWh)`,
                       df$`hydro (MWh)`, df$`petroleum products (MWh)`, df$`natural gas (MWh)`, df$`coal (MWh)`, df$`nuclear (MWh)`) ~Month, data = df, FUN = mean, na.rm = TRUE)
x <- c("Month", "other", "wind", "solor", "hydro", "petroleum products", "natural gas", "coal", "nuclear")
colnames(df2) <- x
df2 <- df2[order(match(df2$Month, month.name))]
rownames(df2) <- NULL
#df2 is aggregated dataframe with month

df2_nomonth <- df2[,-c(1)]
df2_scale <- as.data.frame(scale(df2_nomonth)) #Scaling

#PCA
sol <- prcomp(df2_scale)
summary(sol)
sol$rotation[, 1]
proj <- as.data.frame(sol$x)

Sol <- PCA(df2_scale, scale.unit = TRUE, graph = TRUE)
#Eigen Value
Sol$eig
#Cor of Var
Sol$var$coord
#COS2
for Var
Sol$var$cos2
Sol$var$coord^2
#For Idividual
#individual score~coordinate of individual
Sol$ind$coord
#individual variability captured
Sol$ind$cos2
#scree plot
fviz_eig(Sol)
#correlation circle with cos2
fviz_pca_var(Sol, col.var = "cos2")
#Biplot
fviz_pca_biplot(Sol, col.ind = "cos2")

#Transform data based on PCAs
pca_res <- prcomp(df2_nomonth, center = TRUE, scale. = TRUE)
summary(pca_res)
# We only pick the 2 first PCs
df2_transform = as.data.frame(-pca_res$x[,1: 2])

#We then do clustering and we pick 4 clusters
fviz_nbclust(df2_transform, kmeans, method = 'wss')

#CLUSTERING
k = 4
kmeans_energy = kmeans(df2_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_energy, data = df2_transform)

df_no_day <- df[, c(2: 9)]
cor_matrix <- cor(df_no_day)
corrplot(cor_matrix)





#MULTIPLE LINEAR REGRESSION 
install.packages("readxl")
install.packages("janitor")
install.packages("corrplot")

library(readxl)
library(janitor)
library(corrplot)

df_indivar <- read_xlsx("/Users/mac/Desktop/GroupProject1/Daily electricity demand net generation and net imports for New England____.xlsx")
df_lmp <- read_xlsx("/Users/mac/Desktop/GroupProject1/Daily locational marginal prices.xlsx")
df_source <- read_xlsx("/Users/mac/Desktop/GroupProject1/DailyNetGenerationByEnergySourceInNewEngland.xlsx")

#daily averages
lmp_day <- colMeans(matrix(df_lmp$`real time dollars per megawatthour ($/MWh)`,4))
df_lmp_day <- data.frame(lmp_day)

#Combine coulmns:
df_regression <-cbind(df_indivar, df_lmp_day)
df_merge <- cbind(df_regression, df_source)
df_merge <- df_merge[,-c(7)]

df_regression_noday  <- df_merge[,-c(1,3,4)]
df_scaled <- as.data.frame(scale(df_regression_noday))
model <- lm(lmp_day ~.-1, data=df_scaled)
summary(model)
anova(model)

#Checking for Multicollinearity with VIF
library(car)
#Syntax: 
vif(lm(, data=frame))
car::vif(lm(lmp_day ~., data=df_scaled))
cor(df_scaled)

#We start by looking at the model
model <- lm(lmp_day ~`solar megawatthours (MWh)` + `coal megawatthours (MWh)` + `hydro megawatthours (MWh)` + `wind megawatthours (MWh)` + `nuclear megawatthours (MWh)` +`other megawatthours (MWh)` + `petroleum products megawatthours (MWh)` + `natural gas megawatthours (MWh)`, data=df_scaled)
summary(model)
anova(model)

#We then go through the different steps of selection variables.

#Step 1: Multicollinearity 
#We only look at the generation from different sources. 
#We Use VIF
car::vif(lm(lmp_day ~`solar megawatthours (MWh)` + `coal megawatthours (MWh)` +`hydro megawatthours (MWh)` + `wind megawatthours (MWh)` + `nuclear megawatthours (MWh)` + `other megawatthours (MWh)` + `petroleum products megawatthours (MWh)` +  `natural gas megawatthours (MWh)`, data=df_scaled))
#Conclusion there is no significant multicollinearity 

#Step 2
#Variable selection. We use the best subset selection
Models <- leaps::regsubsets(lmp_day ~`solar megawatthours (MWh)` + `coal megawatthours (MWh)` + `hydro megawatthours (MWh)` + `wind megawatthours (MWh)` + `nuclear megawatthours (MWh)` + `other megawatthours (MWh)` + `petroleum products megawatthours (MWh)` + `natural gas megawatthours (MWh)`, data=df_scaled, nvmax=8)
#Store the summary
choices<-summary(models)
names(choices)
#check out items inside the "choices"
print(choices$which)
print(choices$adjr2)
print(choices$cp)

#Residual Sum of Square: Declines with more variables
plot(choices$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

#Adjusted R Square: Higher
plot(choices$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
#Find maximum
adj_r2_max <- which.max(choices$adjr2) 
adj_r2_max
#Plot the maximum point
points(adj_r2_max, choices$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

#Cp: Lower
plot(choices$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
#Find minimum
cp_min <- which.min(choices$cp) 
cp_min
#Plot the minimum point
points(cp_min, choices$cp[cp_min], col = "red", cex = 2, pch = 20)

#BIC: Lower
plot(choices$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
#Find minimum
bic_min <- which.min(choices$bic) 
bic_min 
#Plot the minimum point
points(bic_min, choices$bic[bic_min], col = "red", cex = 2, pch = 20)

#Based on the above analysis, we conclude that we only need 5 independent variables
#We remove Wind, nuclear, others to end up with 5 independent variables.
#We validate the model and sees that all the independent variables are significant except intercept, so we remove this one
#We see that hydro is insignificant in the F-test, so we remove hydro
model <- lm(lmp_day ~`solar megawatthours (MWh)` + `coal megawatthours (MWh)` + `hydro megawatthours (MWh)`  + `petroleum products megawatthours (MWh)` + `natural gas megawatthours (MWh)`-1, data=df_scaled)
summary(model)
anova(model)

#Removing hydro
model <- lm(lmp_day ~`solar megawatthours (MWh)` + `coal megawatthours (MWh)` + `petroleum products megawatthours (MWh)` + `natural gas megawatthours (MWh)`-1, data=df_scaled)
summary(model)
anova(model)

#K-Fold Validation: Goodness of model by comparing MSR
# Issue right now because there are NAs in the dataset. 
install.packages("DAAG",type = "binary")
library(DAAG)
#Get original MSE
anova(model)
#Compare with the k-fold MSE
Cvoutput <- cv.lm(data = df_scaled, model, m=3, printit = TRUE)

#Step 3 
# We Check for heteroskedasticity
library(ggplot2)
ggplot(data = df_scaled, aes(y = model$residuals, x = model$fitted.values)) + geom_point(col = 'blue') + geom_abline(slope = 0)

#2: Q-Q plot (expect to see straight line)
standardresiduals<-rstandard(model)
qqnorm(standardresiduals, ylab = "Standardized Residuals", pch = 1, frame = FALSE)
qqline(standardresiduals, datax = FALSE)


#White's standard error, it will adjust variance of coefficients
library(sandwich)
library(lmtest)

#from the bptest we cannot reject the null hypothesis. This means there is no violation.
#Hetereo does not exist.
lmtest::bptest(model)
#Null hypothesis: there is no heteroskedasticity
#Alternative hypothesis: heteroskedasticity exists
#DONE
summary(model)

