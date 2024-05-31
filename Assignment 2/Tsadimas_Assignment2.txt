getwd()
setwd("C:/Users/rg_ts/OneDrive/Υπολογιστής/MsC/aueb-Data Analysis/Εργασία 2η")
# Check and install packages if necessary
packages_needed <- c("hms", "nortest", "psych", "caret", "tidyr", "gmodels", "readxl", "ggplot2", "corrplot", "glmnet", "car")
for (pkg in packages_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load libraries
library(hms)
library(nortest)
library(psych)
library(caret)
library(tidyr)
library(gmodels)
library(readxl)
library(ggplot2)
library(corrplot)
library(glmnet)
library(car)



data = read.csv("sixt_21.csv",sep = ";",header = TRUE, na.strings = "NULL")
data = subset(data, select = -X)

str(data)
summary(data)
sapply(data, class)
describe(data)
sum(is.na(data))
colSums(is.na(data))   #i have 7773 missing values, too many to simply ignore


# Convert categorical variables to factors
categorical_vars <- c("Agent.group", "Driver.Country_Disp", "Group", "Charged.group", 
                      "Sales.Channel.2", "Segment", "Manufacturer", "Color", 
                      "Rate.title", "Status")

data[categorical_vars] <- lapply(data[categorical_vars], factor)

# Convert binary variables to factors
binary_vars <- c("AD", "B", "BC", "BE", "BF", "BO", "BR", "BS", "CS", "DI", 
                 "FDW", "LD", "NV", "PAI", "SC", "SS", "SUB", "TF", "TG", 
                 "UPS", "Upgra")
data[binary_vars] <- lapply(data[binary_vars], factor)

# Convert Date/Time variables to Date or POSIXct
data$`Check.out.Date` <- as.Date(data$`Check.out.Date`, format="%d/%m/%Y")
data$`Booking.Date` <- as.Date(data$`Booking.Date`, format="%d/%m/%Y")


# Convert Time variables to POSIXct
# Assuming data$`Check.out.Time` and data$`Booking.Time` originally contain hour values
data$`Check.out.Time` <- as_hms(paste0(data$`Check.out.Time`, ":00:00"))
data$`Booking.Time` <- as_hms(paste0(data$`Booking.Time`, ":00:00"))

# Omitting rows with any missing values
data_clean <- na.omit(data)
data<-data_clean
str(data)
summary(data)
sum(is.na(data))
rm(data_clean)


# Adding the last two columns to create a new column
data$On.Desk.Net.Revenue <- data$OnDesk.Insurance.Net.Revenue + data$OnDesk.Non.Insurance.Net.Revenue

# Now, remove the original columns from the dataset
data <- subset(data, select = -c(OnDesk.Insurance.Net.Revenue, OnDesk.Non.Insurance.Net.Revenue))

str(data)
summary(data)

data = data[data$Status != "CNC", ]


#now our data set is ready for EDA



ggplot(data, aes(x = Booking.Date, y = On.Desk.Net.Revenue)) +
  geom_line() + 
  labs(title = "On-Desk Net Revenue Over Time", x = "Date", y = "On-Desk Net Revenue") +
  theme_minimal() +
  coord_cartesian(xlim = c(as.Date("2022-01-01"), as.Date("2022-10-01")))

#we see here the relationship between time and revenue
#,the majority in june-sept as expected

# plot for Driver.Age
par(mfrow=c(2,2))
boxplot(data$Driver.Age, main = "Boxplot for Driver Age", col = "green")
hist(data$Driver.Age, col = "green", xlab = "Driver's Age", breaks = 50, xlim = c(0, 120)) 
qqnorm(data$Driver.Age, main = "Driver Age")
qqline(data$Driver.Age)
par(mfrow=c(1,1))
#we see that most customers are from 25 to 55 age group

#of course the response
boxplot(data$On.Desk.Net.Revenue, main = "Boxplot for On Desk Net Revenue", col = "red")
hist(data$On.Desk.Net.Revenue, col = "red", xlab = "On Desk Net Revenue", breaks = 50, xlim = c(0, 1000)) 
qqnorm(data$On.Desk.Net.Revenue, main = "On Desk Net Revenue")
qqline(data$On.Desk.Net.Revenue)


hist(log(data$On.Desk.Net.Revenue), col = "red", xlab = "On Desk Net Revenue", breaks = 50) 

#looks like it follows exponential, definately no normality

#now Group variable

barplot(table(data$Group), 
        main = "Bar Plot for Group Variable",
        xlab = "Group",
        ylab = "Frequency",
        col = "skyblue",
        border = "black",
        las = 2)
#most purchesed product  MDMR, and then EDMR 
#MANUAL,4,5 door ,unspecidied fuel with aircondition 
#mini or economy

#now the Mnufacturer

barplot(table(data$Manufacturer), 
        main = "Bar Plot for Manufacturer",
        xlab = "Group",
        ylab = "Frequency",
        col = "skyblue1",
        border = "black",
        las = 2)
#by far fiat, most rent

#now for Rate.title
rate_counts <- table(data$Rate.title)

# Create a pie chart
pie(rate_counts,
    main = "Pie Chart for Rate Title",
    col = rainbow(length(rate_counts)), # Use rainbow colors for better distinction
    labels = paste(names(rate_counts), ": ", rate_counts), # Include labels with names and frequencies
    cex = 0.8, # Adjust label size
    clockwise = TRUE)
#almost all Sixt import rates ,type of reservation's cost rate category

#finally segment
# Count the frequency of each Segment
segment_counts <- table(data$Segment)

# Create a pie chart
pie(segment_counts,
    main = "Pie Chart for Segment",
    col = rainbow(length(segment_counts)), # Use rainbow colors for better distinction
    labels = paste(names(segment_counts), ": ", segment_counts), # Include labels with names and frequencies
    cex = 0.8,
    clockwise = TRUE)
#most of our reservation come from retail,individual customers





numeric_data <- data[sapply(data, is.numeric)]

correlation_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(correlation_matrix, method = "circle", type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 30,
         tl.cex = 0.6,
         cutoff = 0.3)

#pairwise assosiation
#response vs Driver age
ggplot(data, aes(x = Driver.Age, y = On.Desk.Net.Revenue)) +
  geom_point(color = "orange") +
  labs(title = "Scatter Plot of Driver Age vs On-Desk Net Revenue", 
       x = "Driver Age", 
       y = "On-Desk Net Revenue") +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "lightblue1"))
#similar spending across age groups

#response vs group

ggplot(data, aes(x = Group, y = On.Desk.Net.Revenue)) +
  geom_boxplot(fill = "lightblue", outlier.color = "orange") + 
  labs(title = "Box Plot of On-Desk Net Revenue by Group",
       x = "Group",
       y = "On-Desk Net Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#response and manufacturer
ggplot(data, aes(x = Manufacturer, y = On.Desk.Net.Revenue)) +
  geom_boxplot(fill = "lightblue", outlier.color = "orange") + 
  labs(title = "Box Plot of On-Desk Net Revenue by Manufacturer",
       x = "Manufacturer",
       y = "On-Desk Net Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#same across groups porsche and volvo a litlte higher

#group against Manufacturer to see preference across groups
ggplot(data, aes(x = Manufacturer, fill = Group)) +
  geom_bar(position = "dodge") +
  labs(title = "Clustered Bar Chart for Group against Manufacturer",
       x = "Manufacturer",
       y = "Count",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#MDMR fiat dominates ,EDMR nissan

#age vs manufacturer

ggplot(data, aes(x = Manufacturer, y = Driver.Age)) +
  geom_boxplot(fill = "lightblue", outlier.color = "orange") + 
  labs(title = "Box Plot of Manufacturer by Age of Driver",
       x = "Manufacturer",
       y = "Age of Driver") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Land rover for over 60 not great differences elsewhere,
#peugeot fiat citroen lower age means

anov<-aov(data$On.Desk.Net.Revenue ~data$Agent.group)
lillie.test(anov$res)#oxi kanonikothta
kruskal.test(data$On.Desk.Net.Revenue~data$Agent.group)#diaforopoihsh diamesvn sta Agent group
boxplot(data$On.Desk.Net.Revenue~data$Agent.group,ylab="On Desk Revenue",xlab="Agent group",col=3:7)#apeikonish diameswn 
pairwise.t.test(data$On.Desk.Net.Revenue,data$Agent.group,pool.sd = T) 
#online broker -corp
#sixt-corp 
#sixt-travel
#walk in -corp

by(data$On.Desk.Net.Revenue,data$AD,lillie.test)
wilcox.test(data$On.Desk.Net.Revenue~data$AD)
boxplot(data$On.Desk.Net.Revenue~data$AD,ylab="On Desk Revenue",xlab="AD",col=2:3,ylim=c(0,1500))#apeikonish diameswn 

by(data$On.Desk.Net.Revenue,data$NV,lillie.test)
wilcox.test(data$On.Desk.Net.Revenue~data$NV)

by(data$On.Desk.Net.Revenue,data$PAI,lillie.test)
wilcox.test(data$On.Desk.Net.Revenue~data$PAI)

by(data$On.Desk.Net.Revenue,data$SS,lillie.test)
wilcox.test(data$On.Desk.Net.Revenue~data$SS)

#PAIRWISE Response vs BINARY variables 

by(data$On.Desk.Net.Revenue, data$BF,lillie.test) #reject the H0 (we dont have normality)
wilcox.test(data$On.Desk.Net.Revenue~data$BF) #reject the H0 
boxplot(data$Days~data$BF, main= "Net Revenue of Insurance & Non-Insurance vs Loss Damage Waiver minimum", xlab="Net Revenue of Insurance & Non-Insurance", ylab="Loss Damage Waiver minimum", col=3:4, las=1, ylim=c(0,60))


by(data$On.Desk.Net.Revenue, data$LD,lillie.test) #reject the H0 (we dont have normality)
wilcox.test(data$On.Desk.Net.Revenue~data$LD) #reject the H0 
boxplot(data$Days~data$LD, main= "Net Revenue of Insurance & Non-Insurance vs LD-Loss Damage Waiver", xlab="Net Revenue of Insurance & Non-Insurance", ylab="LD-Loss Damage Waiver", col=3:4, las=1, ylim=c(0,60))

by(data$On.Desk.Net.Revenue, data$BC,lillie.test) #reject the H0 (we dont have normality)
wilcox.test(data$On.Desk.Net.Revenue~data$BC) #do not reject the H0 


by(data$On.Desk.Net.Revenue, data$BO,lillie.test) #reject the H0 (we dont have normality)
wilcox.test(data$On.Desk.Net.Revenue~data$BO) #do not reject the H0 

by(data$On.Desk.Net.Revenue, data$BE,lillie.test) #reject the H0 (we dont have normality)
wilcox.test(data$On.Desk.Net.Revenue~data$BE) #do not reject the H0 

by(data$On.Desk.Net.Revenue, data$DI,lillie.test) #reject the H0 (we dont have normality)
wilcox.test(data$On.Desk.Net.Revenue~data$DI) #do not reject the H0




chisq.test(data$Manufacturer,data$Group)
CrossTable(data$Manufacturer,data$Group)  
chisq.test(data$Manufacturer,data$Rate.title)  
CrossTable(data$Manufacturer, data$Rate.title)

#now the model
str(data)
sapply(data[, sapply(data, is.factor)], function(x) length(levels(x)))
data <- droplevels(data)
data <- data[, !(names(data) %in% c("Upgra", "SC","Status","SUB","Res.no","Agr..no","Driver.ID"))]
sapply(data[, sapply(data, is.factor)], function(x) length(levels(x)))

m1<-lm(data$On.Desk.Net.Revenue~.,data = data)
summary(m1)
anova(m1)
#full model, 17 variables significant

#LASSO 

# Ensure glmnet is installed and loaded

# Prepare data for glmnet
x <- data.matrix(data[-c(42)])
y <- data$On.Desk.Net.Revenue


x.standardized <- scale(x)
# Cross-validation for lambda selection
set.seed(123) # For reproducibility
cv.out <- cv.glmnet(x.standardized, y, alpha = 1) # alpha=1 for LASSO

# Best lambda
lambda.best <- cv.out$lambda.min
lambda.best
lambda.1se <- cv.out$lambda.1se
lambda.1se

# Fit LASSO model with selected lambda
lasso.model <- glmnet(x.standardized, y, alpha = 1, lambda = lambda.best)
lasso.model2 <- glmnet(x.standardized, y, alpha = 1, lambda = lambda.1se)
# Display the coefficients
coef(lasso.model)
coef(lasso.model2)

plot(cv.out)
abline(v = log(cv.out$lambda.min), col = "red")
abline(v = log(cv.out$lambda.1se), col = "blue")


subdata <- data[, !(names(data) %in% c("BO", "CS","BR","BS","DI","FDW",
                                    "NV","PAI","SS","TG","Check.out.Date",
                                    "Check.out.Time","Booking.Time","BC","BE",
                                    "C.O.Mileage","Check.out.Station","Charged.group",
                                    "Internet.Non.Insurance.Net.Revenue","Past.Rentals.Entry "))]

m2_after_lasso <- lm(On.Desk.Net.Revenue ~ . - BO - CS - BR - BS - DI - FDW - 
                       NV - PAI - SS - TG - Check.out.Date - Check.out.Time - 
                       Booking.Time - BC - BE - C.O.Mileage - Check.out.Station - 
                       Charged.group - Internet.Non.Insurance.Net.Revenue - 
                       Past.Rentals.Entry, 
                     data = data)

summary(m2_after_lasso)
anova(m2_after_lasso)
#18 significant variables
mfull<-lm(data$On.Desk.Net.Revenue~.,data = subdata)
mnull<-lm(data$On.Desk.Net.Revenue~1,data = subdata)
best_model_stepwise <- step(m2_after_lasso, scope = list(lower = mnull, upper = mfull),
                            direction = "both")
summary(best_model_stepwise)
anova(best_model_stepwise)
#we are left with 13 variables ,after the stepwise 
#and after excluding Pre.paid.Amount - B- Booking.Date-First.Licence.Year-Rate.title
#as statistically insignificant

m3<-lm(On.Desk.Net.Revenue ~ . - BO - CS - BR - BS - DI - FDW - 
         NV - PAI - SS - TG - Check.out.Date - Check.out.Time - 
         Booking.Time - BC - BE - C.O.Mileage - Check.out.Station - 
         Charged.group - Internet.Non.Insurance.Net.Revenue - 
         Past.Rentals.Entry-UPS-AD-Pre.paid.Amount-B-Booking.Date-
         First.Licence.Year-Rate.title , 
       data = data)
summary(m3)
anova(m3)
#this is our best model up to that point

m4<-lm(On.Desk.Net.Revenue ~ Days+Pre.paid.Amount+Agent.group+Booking.Date+
        BR+LD+TG+Check.out.Station+Charged.group+Internet.Insurance.Net.Revenue+
         Internet.Non.Insurance.Net.Revenue+Rental_Cost_Res+Sales.Channel.2+Manufacturer, 
       data = data)
summary(m4)
anova(m4)

m5<-lm(On.Desk.Net.Revenue ~ Days+Agent.group+Booking.Date+
         LD+TG+Check.out.Station+Charged.group+Internet.Insurance.Net.Revenue+
         Rental_Cost_Res+Manufacturer, 
       data = data)
summary(m5)
anova(m5)

m6 = lm(On.Desk.Net.Revenue ~ LD + Group + 
          Charged.group + Rental_Cost_Res + 
          Manufacturer , data = data)
summary(m6)
anova(m6)


#lets work with model 6 since it has similar predictive power with all others
#but uses only 5 variables

#NOW the assumptions

##ASSUMPTIONS
#1 homoscedasticity

ncvTest(m6)
yhat.quantiles<-cut(fitted(m6), breaks=quantile(fitted(m6), probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(m6)~yhat.quantiles)
par(mfrow=c(2,2))
#homoscedasticity exists
boxplot(rstudent(m6)~yhat.quantiles)

#2 non linearity

residualPlot(m6,type="rstudent") 


#3 independence of errors
plot(rstudent(m6), type='l') 

#4 normality
plot(m6,which=2)  
par(mfrow=c(1,1))
shapiro.test(rstudent(m6)) 

lillie.test(rstudent(m6)) 




ctrl <- trainControl(method = "cv", number = 10)

model_cv <- train(On.Desk.Net.Revenue ~ ., data = data, method = "lm", trControl = ctrl)

print(model_cv)
summary(model_cv)
summary(model_cv$finalModel)

model_cv1 <- train(On.Desk.Net.Revenue ~ . - BO - CS - BR - BS - DI - FDW - 
         NV - PAI - SS - TG - Check.out.Date - Check.out.Time - 
         Booking.Time - BC - BE - C.O.Mileage - Check.out.Station - 
         Charged.group - Internet.Non.Insurance.Net.Revenue - 
         Past.Rentals.Entry-UPS-AD-Pre.paid.Amount-B-Booking.Date-
         First.Licence.Year-Rate.title, data = data, method = "lm", trControl = ctrl)

print(model_cv1)
summary(model_cv1)

#so i will keep model 6




sixt_test_data <- read_excel("sixt_Test_Dataset.xlsx")  # Ensure the file name includes the correct extension (.xlsx or .xls)

sixt_test_data <- lapply(sixt_test_data, function(x) {
  if(is.character(x)) {
    x[x == "NULL"] <- NA
  }
  return(x)
})

sixt_test_data <- as.data.frame(sixt_test_data)


str(sixt_test_data)
summary(sixt_test_data)
sapply(sixt_test_data, class)
describe(sixt_test_data)
sum(is.na(sixt_test_data))
colSums(is.na(sixt_test_data))   #i have 7773 missing values, too many to simply ignore


# Convert categorical variables to factors
categorical_vars <- c("Agent.group", "Driver.Country_Disp", "Group", "Charged.group", 
                      "Sales.Channel.2", "Segment", "Manufacturer", "Color", 
                      "Rate.title", "Status")

sixt_test_data[categorical_vars] <- lapply(sixt_test_data[categorical_vars], factor)

# Convert binary variables to factors
binary_vars <- c("AD", "B", "BC", "BE", "BF", "BO", "BR", "BS", "CS", "DI", 
                 "FDW", "LD", "NV", "PAI", "SC", "SS", "SUB", "TF", "TG", 
                 "UPS", "Upgra")
sixt_test_data[binary_vars] <- lapply(sixt_test_data[binary_vars], factor)

# Convert Date/Time variables to Date or POSIXct
sixt_test_data$`Check.out.Date` <- as.Date(sixt_test_data$`Check.out.Date`, format="%d/%m/%Y")
sixt_test_data$`Booking.Date` <- as.Date(sixt_test_data$`Booking.Date`, format="%d/%m/%Y")


# Convert Time variables to POSIXct
# Assuming data$`Check.out.Time` and data$`Booking.Time` originally contain hour values
sixt_test_data$`Check.out.Time` <- as_hms(paste0(sixt_test_data$`Check.out.Time`, ":00:00"))
sixt_test_data$`Booking.Time` <- as_hms(paste0(sixt_test_data$`Booking.Time`, ":00:00"))

# Omitting rows with any missing values
data_clean <- na.omit(sixt_test_data)
sixt_test_data<-data_clean
str(sixt_test_data)
summary(sixt_test_data)
sum(is.na(sixt_test_data))
rm(data_clean)


sixt_test_data$OnDesk.Insurance.Net.Revenue<-as.numeric(sixt_test_data$OnDesk.Insurance.Net.Revenue)
sixt_test_data$OnDesk.Non.Insurance.Net.Revenue<-as.numeric(sixt_test_data$OnDesk.Non.Insurance.Net.Revenue)

# Adding the last two columns to create a new column
sixt_test_data$On.Desk.Net.Revenue <- sixt_test_data$OnDesk.Insurance.Net.Revenue + sixt_test_data$OnDesk.Non.Insurance.Net.Revenue

# Now, remove the original columns from the dataset
sixt_test_data <- subset(sixt_test_data, select = -c(OnDesk.Insurance.Net.Revenue, OnDesk.Non.Insurance.Net.Revenue))

str(sixt_test_data)
summary(sixt_test_data)

sixt_test_data = sixt_test_data[sixt_test_data$Status != "CNC", ]

str(sixt_test_data)
sapply(sixt_test_data[, sapply(sixt_test_data, is.factor)], function(x) length(levels(x)))
sixt_test_data <- droplevels(sixt_test_data)
sixt_test_data <- sixt_test_data[, !(names(sixt_test_data) %in% c("Upgra", "SC","Status","SUB","Res.no","Agr..no","Driver.ID"))]
sapply(sixt_test_data[, sapply(sixt_test_data, is.factor)], function(x) length(levels(x)))

m4<-lm(On.Desk.Net.Revenue ~ Days+Pre.paid.Amount+Agent.group+Booking.Date+
         BR+LD+TG+Check.out.Station+Charged.group+Internet.Insurance.Net.Revenue+
         Internet.Non.Insurance.Net.Revenue+Rental_Cost_Res+Sales.Channel.2+Manufacturer, 
       data = sixt_test_data)
summary(m4)
anova(m4)

m5<-lm(On.Desk.Net.Revenue ~ Days+Agent.group+Booking.Date+
         LD+TG+Check.out.Station+Charged.group+Internet.Insurance.Net.Revenue+
         Rental_Cost_Res+Manufacturer, 
       data = sixt_test_data)
summary(m5)
anova(m5)

m6 = lm(On.Desk.Net.Revenue ~ LD + Group + 
          Charged.group + Rental_Cost_Res + 
          Manufacturer , data = sixt_test_data)
summary(m6)
anova(m6)

model_cv1 <- train(On.Desk.Net.Revenue ~ . - BO - CS - BR - BS - DI - FDW - 
                     NV - PAI - SS - TG - Check.out.Date - Check.out.Time - 
                     Booking.Time - BC - BE - C.O.Mileage - Check.out.Station - 
                     Charged.group - Internet.Non.Insurance.Net.Revenue - 
                     Past.Rentals.Entry-UPS-AD-Pre.paid.Amount-B-Booking.Date-
                     First.Licence.Year-Rate.title, data = sixt_test_data, method = "lm", trControl = ctrl)

print(model_cv1)
summary(model_cv1)

#worse predictive power than in our training dataset ,
#but again i will keep model 6
summary(m6)
anova(m6)
predict(m6)
#the expected value of net revenue on desk is 4.59 euros
#when all other predictors are on the base level
#and rental cost res zero

#when loss damage waiver is selected
#Pricing Strategy: The company might reassess how the loss damage waiver
#is priced or marketed to balance its impact on net revenue

#popular option are not significant
#meaning the customers that select non conventional options
#more probable to buy incrementals

100*sum(data$On.Desk.Net.Revenue == 0)/length(data$On.Desk.Net.Revenue)
#[1] 46.12313
100*sum(sixt_test_data$On.Desk.Net.Revenue == 0)/length(sixt_test_data$On.Desk.Net.Revenue)
#[1] 47.27798
