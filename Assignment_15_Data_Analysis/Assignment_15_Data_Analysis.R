#assignment 15
install.packages("psych")
install.packages("sjmisc")
install.packages("corrplot")
install.packages("GGally")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("gridExtra")
install.packages("nortest")
install.packages("tidyr")
install.packages("gmodels")
install.packages("car")
library(car)
library(gmodels)
library(ggplot2)
library(gridExtra)
library(psych)
library(corrplot)
library(grid)
library(GGally)
library(tidyr)
library(nortest)
library(ggpubr)
library(sjmisc)

grades_portuguese=read.table("student-por.csv",sep=";",header=TRUE)

#a
#display the structure of the dataframe
str(grades_portuguese)
summary(grades_portuguese)
sapply(grades_portuguese, class)
sum(is.na(grades_portuguese))  #clean dataset
grades_portuguese <- grades_portuguese[grades_portuguese$G1 != 0, ]
grades_portuguese <- grades_portuguese[grades_portuguese$G2 != 0, ]
grades_portuguese <- grades_portuguese[grades_portuguese$G3 != 0, ]
grades_portuguese <- grades_portuguese[grades_portuguese$G3 != 1, ]
#exclude students with zero grades, either left school,either arrived during the year
#or data is missing,typo

# convert categorical vars to factors
#and then discrete numeric variables to factors

df <- lapply(grades_portuguese, function(x) {
  if (is.character(x)) {
    return(factor(x))
  } else {
    return(x)
  }
})


# Transforming the 'failures' column
df$traveltime<-factor(df$traveltime)
levels(df$traveltime)
df$studytime<-factor(df$studytime)
levels(df$studytime)
df$failures<-factor(df$failures)
levels(df$failures) <- c("No Failure",
                         "One Failure",
                         "Two Failures",
                         "Three or More Failures")
levels(df$failures)
df$famrel<-factor(df$famrel)
levels(df$famrel)
df$freetime<-factor(df$freetime)
levels(df$freetime)
df$goout<-factor(df$goout)
levels(df$goout)
df$Dalc<-factor(df$Dalc)
levels(df$Dalc)
df$Walc<-factor(df$Walc)
levels(df$Walc)
df$health<-factor(df$health)
levels(df$health)


grades_portuguese<- as.data.frame(df)
str(grades_portuguese)
rm(df)

attach(grades_portuguese)

#b
#let's now explore the more important categorical variables

#first how many students from each school answered
pie(table(school),
    labels = c("Gabriel Pereira","Mousinho da Silveira"),
    main = "School that students attend",
    font.main = 4,
    col = c("green","lightblue"))
legend("topright",         # Position of the legend
       legend = table(school),  # Labels in the legend
       fill = c("green","lightblue"),            # Colors in the legend
       title = "#of students")              # Title of the legend

#more data from GP school

#second we get a clearer picture about the gender of our sample

print(table(sex,school))
barplot(table(sex,school),
        font.main = 4,
        col=c("lightblue3","mediumpurple1"),
        beside=T,
        main="Gender per School",
        ylim=c(0,250),
        names.arg = c("Gabriel Pereira","Mousinho da Silveira"))
legend("topright",                    
       legend = c( "Female","Male"),  
       fill = c("lightblue3", "mediumpurple1"))

#same analogy of girls/boys ,girls slightly more 

#now the same for parents' cohabitation status

print(table(Pstatus))
pie(table(Pstatus),
    labels = c("apart","living together"),
    main = "Living situation of parents",
    font.main = 4,
    col = c("lightpink","lightgreen"))
legend("topright",         # Position of the legend
       legend = table(Pstatus),  # Labels in the legend
       fill = c("lightpink","lightgreen"),           # Colors in the legend
       title = "#of students")    

#only 0.12% have parents that live apart

#parents educational level

print(table(Medu))
mother_ed_level<-cut(Medu,
                     breaks=c(-Inf,3,4),
                     labels = c("Secondary or lower","Higher education"))
print(table(mother_ed_level))

print(table(Fedu))
father_ed_level<-cut(Fedu,
                     breaks=c(-Inf,3,4),
                     labels = c("Secondary or lower","Higher education"))
print(table(father_ed_level))

combined_ed_level <- as.integer(mother_ed_level == "Higher education")+ as.integer(father_ed_level == "Higher education")

# Convert to factor with labels
parents_higher_ed<- factor(combined_ed_level, levels = c(0, 1, 2),
                                   labels = c("None", "One", "Both"))
grades_portuguese<-cbind(grades_portuguese,parents_higher_ed)
grades_portuguese$Medu<-NULL
grades_portuguese$Fedu<-NULL

print(table(parents_higher_ed))
print(table(parents_higher_ed,school))
barplot(table(parents_higher_ed,school),
        font.main = 4,
        col=c("lightblue3","coral1","mediumpurple1"),
        ylim=c(0,300),
        beside=T,
        main="Number of parents with higher education",
        names.arg = c("Gabriel Pereira","Mousinho da Silveira"))
legend("topright",                    
       legend = c("None", "One", "Both"),  
       fill = c("lightblue3","coral1","mediumpurple1"))

frq(parents_higher_ed, title="Education level", out="v")

install.packages("knitr")
install.packages("kableExtra")

library(knitr)
library(kableExtra)
# Create a frequency table
education_freq <- table(parents_higher_ed)

# Convert to data frame
education_freq_df <- as.data.frame(education_freq)

# Calculate percentages
education_freq_df$Percentage <- round(education_freq_df$Freq / sum(education_freq_df$Freq) * 100, 2)

# Create a frequency table by school
education_freq_school <- table(parents_higher_ed, grades_portuguese$school)

# Convert to data frame
education_freq_school_df <- as.data.frame(education_freq_school)

# Calculate percentages for the breakdown by school
education_freq_school_df$Percentage <- round(education_freq_school_df$Freq / sum(education_freq_school_df$Freq) * 100, 2)

v_overall <- kable(education_freq_df, caption = "Overall Education Level", format = "html", table.attr = "style='width:70%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Creating a styled table for frequency by school
v_by_school <- kable(education_freq_school_df, caption = "Education Level by School", format = "html", table.attr = "style='width:70%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# To view the tables in RStudio Viewer
v_overall
v_by_school
# 68% have no parent with higher education,
#alsmost same percentage have one or both 15%

#now let's see the extra educational support variable
print(table(schoolsup))
barplot(table(schoolsup,school),
        font.main = 4,
        col=c("coral1","mediumpurple1"),
        ylim=c(0,400),
        beside=T,
        main="Students without support vs Students with support",
        names.arg = c("Gabriel Pereira","Mousinho da Silveira"))
legend("topright",                    
       legend = table(schoolsup),  
       fill = c("coral1","mediumpurple1"))

#only 10% have support

#now the same for activities variable

activities<-table(grades_portuguese$activities)
print(activities)
barplot(table(grades_portuguese$activities, grades_portuguese$school),
        font.main = 4,
        col=c("turquoise","yellow4"),
        ylim=c(0,250),
        beside=T,
        main="Students without activities vs Students with activities",
        legend= T,
        names.arg = c("Gabriel Pereira","Mousinho da Silveira"))
rm(activities)
#students with activities are around 50% ,both schools

#internet
print(table(internet))
barplot(table(internet,school),
        font.main = 4,
        col=c("turquoise3","yellow3"),
        ylim=c(0,500),
        beside=T,
        main="Students without internet vs Students with internet",
        legend= T,
        names.arg = c("Gabriel Pereira","Mousinho da Silveira"))

#only 23% do not have internet ,bigger differences in GP

#relationship
print(table(romantic))
barplot(table(romantic,school),
        font.main = 4,
        col=c("blue3","yellow3"),
        ylim=c(0,450),
        beside=T,
        main="Students without relationship vs Students with relationship",
        legend= T,
        names.arg = c("Gabriel Pereira","Mousinho da Silveira"))

#37% have relationship

#wants to take higher education
print(table(higher))
barplot(table(higher,school),
        font.main = 4,
        col=c("green3","blue3"),
        ylim=c(0,600),
        beside=T,
        main="Students don't want higher education vs Students that do",
        legend= T,
        names.arg = c("Gabriel Pereira","Mousinho da Silveira"))
#90% want to pursue higher education


#study time
table(studytime)
print(table(studytime,school))
barplot(table(studytime,school),
        main="Study time-School",
        font.main = 4,
        beside = T,
        col=3:6,
        names= c("Gabriel Pereira","Mousinho da Silveira"))
legend("topright",                    
       legend = c("<2 hours", "2-5 hours", "5-10",">10 hours"),  
       fill = 3:6)

#similar study time accross students and schools around the mean 2-5 hours,

#now numerical variables
#checking absences for outliers
par(mfrow=c(1,2))
hist(grades_portuguese$absences,main = "Histogram of absences")
boxplot(grades_portuguese$absences, main = "Boxplot for absences", ylab = "Values")
par(mfrow=c(1,1))
z_scores <- scale(grades_portuguese$absences)
outliers <- which(abs(z_scores) > 3)
outliers
print(grades_portuguese$absences)
summary(grades_portuguese$absences)
describe(grades_portuguese$absences)
#on paper there are values that seem to be outliers but they are  are legitimate values
#so i will keep them

#g3
pass_fail<-cut(G3,
               breaks=c(-Inf,9,20),
               labels = c("Fail","Pass"))
pass_fail<-table(pass_fail)
print(pass_fail)
pie(pass_fail,
    labels = c("Fail","Pass"),
    main = "Pie chart of successes and failures in Portuguese class",
    font.main = 4,
    col = c("red","green"))
legend("topright",
       legend = pass_fail,
       fill = c("red","green"),
       title = "#of students") 

#85% pass the class

boxplot(split(G3,school),
        main="Grades per school",
        font.main = 4,
        col=3:4,
        names= c("Gabriel Pereira","Mousinho da Silveira"),
        ylab = "G3 Scores", 
        ylim=c(0,20),
        yaxt = "n")
axis(side = 2, at = seq(0, 20, by = 1), las = 2)

output_G3<-describe(G3)
print(output_G3[c("mean", "sd","median","min","max")])
selected_stats <- output_G3[c("mean", "sd", "median", "min", "max")]
selected_stats_df <- as.data.frame(selected_stats)
selected_stats_df$mean <- round(selected_stats_df$mean, 2)
selected_stats_df$sd <- round(selected_stats_df$sd, 2)
styled_stats_table <- kable(selected_stats_df, caption = "Statistics of G3", format = "html", table.attr = "style='width:70%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
styled_stats_table

#mean around 12 , with GP having higher median 
#so students generally received higher grades
#Differences in the IQR show that GP students more consistent
#but with more outliers ,mostly on the lower end
#GP symmetric distribution MdS skewed dist on the right
#no normality from the plots


#exploring G1,G2,G3


df<-data.frame(G1,G2,G3)

p1 <-ggplot(df, aes(x = G1, y = G3)) + 
  geom_point(color = "darkorange") +
  ggtitle("Scatter Plot G1 over G3") +
  xlab("First period Scores") +
  ylab("Final Grades") +
  theme(panel.background = element_rect(fill = "lightblue1"))

p2 <-ggplot(df, aes(x = G2, y = G3)) + 
  geom_point(color = "darkblue") +
  ggtitle("Scatter Plot G2 over G3") +
  xlab("Second period Scores") +
  ylab("Final Grades") +
  theme(panel.background = element_rect(fill = "lightblue1"))
grid.arrange(p1, p2, ncol = 2)


#as we expected clear positive relationship between G1,G2 and G3
#as we will see in the correlation matrix as well

#c
#pairwise assosiations
G<-(G1+G2+G3)/3
grades_portuguese<-cbind(grades_portuguese,G)

# Create a correlation matrix for selected variables
cor_matrix <- cor(grades_portuguese[, c("age","absences",
                                        "G")], use = "complete.obs")



# Visualize the correlation matrix
#

corrplot(cor_matrix, method = "circle", addCoef.col = "black")

numeric_variables<-as.data.frame(grades_portuguese[, c("age","absences",
                                                "G")])

# Enhanced scatterplot matrix with GGally package

ggpairs(numeric_variables)

#check numeric vars for normality


long_df <- tidyr::gather(grades_portuguese, key = "variable", value = "value", absences, G,age)
ggplot(long_df, aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue1")) +  # Change background color here
  scale_fill_manual(values = c("blue", "green", "red")) +
  labs(title = "Histograms of Variables", x = "Value", y = "Frequency")



p3<-ggqqplot(absences, 
         title = "Q-Q Plot for Absences",
         ggtheme = theme_minimal(),
         color = "blue")
p4<-ggqqplot(G, 
         title = "Q-Q Plot for G",
         ggtheme = theme_minimal(),
         color = "red")
p5<-ggqqplot(age, 
         title = "Q-Q Plot for age",
         ggtheme = theme_minimal(),
         color = "orange")
grid.arrange(p3, p4, p5, nrow = 1, ncol = 3)

rm(df)
rm(long_df)
#given the fact that the numbers are discrete
#normality in G1,G2,G3 not in absences
#now normality test ,kolmogorov-smirnov
#and shapiro-willks

options(scipen=999)
# Assuming 'data' is your dataset
shapiro.test(G)
shapiro.test(age)
shapiro.test(absences)
#normality is rejected but this test is very sensitive  to large sample sizes
#and the values are discrete.


by(G,school,lillie.test)
by(age,school,lillie.test)
by(absences,school,lillie.test)
lillie.test(G)
lillie.test(age)
lillie.test(absences)
#again normality is rejected but this test is very sensitive  to large sample sizes
#and the values are discrete.

#so given the plots ,the size of the data and the sensitivity of the tests
#i will assume normality for G1,G2,G3

#checking the symmetry

# Calculate mean and median for G1
mean_G <- mean(grades_portuguese$G)
median_G <- median(grades_portuguese$G)

# Calculate mean and median for G2
mean_age <- mean(grades_portuguese$age)
median_age <- median(grades_portuguese$age)

# Calculate mean and median for G3
mean_absences <- mean(grades_portuguese$absences)
median_absences <- median(grades_portuguese$absences)

# Print the results
cat("G - Mean:", mean_G, "Median:", median_G, "\n")
cat("age - Mean:", mean_age, "Median:", median_age, "\n")
cat("absences - Mean:", mean_absences, "Median:", median_absences, "\n")

#mean close to median, pretty symmetric
#now i will check normality using the residuals

m1 <- lm(G ~ age)
m2 <- lm(G ~ absences)
par(mfrow=c(1,2))
hist(residuals(m1), main = "Histogram of Residuals m1", xlab = "Residuals")
hist(residuals(m2), main = "Histogram of Residuals m2", xlab = "Residuals")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
qqnorm(residuals(m1))
qqline(residuals(m1))
qqnorm(residuals(m2))
qqline(residuals(m2))
par(mfrow=c(1,1))
#very good residual plots

shapiro.test(residuals(m1))
shapiro.test(residuals(m2))
#nomrality rejected


#now i will check pairwise assosiation between categorical variables

chisq.test(school, address) #correlation is statistically significant
CrossTable(school, address)  #GP more Urban population, MS balanced
chisq.test(school, Pstatus) #no assosiation
chisq.test(school, parents_higher_ed)##correlation is statistically significant
CrossTable(school, parents_higher_ed)#GP has much more higher educated parents
chisq.test(school, studytime) #correlation is statistically significant
CrossTable(school, studytime) #bigger percentage of GP students study more time
chisq.test(sex, failures) #no assosiation
chisq.test(sex, famrel) #no assosiation
chisq.test(sex, goout) #no assosiation
chisq.test(sex, Dalc) #correlation is statistically significant
CrossTable(sex, Dalc)  #higher alcohol cosnumption in workday by males
chisq.test(sex, Walc)  #correlation is statistically significant
CrossTable(sex, Walc)  #higher alcohol cosnumption in weekend by males
chisq.test(parents_higher_ed, Pstatus)#no assosiation
chisq.test(parents_higher_ed, famrel) #no assosiation
chisq.test(famrel, Pstatus) #no assosiation
chisq.test(guardian, Pstatus) #correlation is statistically significant
CrossTable(guardian, Pstatus) #fathers more often guardians when together
                              #same rate for mothers, other more often when apart

#now pairwise assosiation between numeric and categorical variables

#G3,sex
boxplot(G ~ sex, 
        main = "Boxplot of G vs sex",
        col = c("pink","lightblue1"),
        xlab = "gender",
        ylab = "Grades")

leveneTest(G ~ sex) #similar variances

ks.test(G[sex=="F"],G[sex=="M"])
shapiro.test(G[sex == "F"])
shapiro.test(G[sex == "M"])
#normality is rejected ,so non parametric test better here
wilcox.test(G[sex=="F"],G[sex=="M"])#median differs significantly


#G,Pstatus
boxplot(G ~ Pstatus, 
        main = "Boxplot of G vs Pstatus",
        col = c("red","green"),
        xlab = "Parents Status",
        ylab = "Grades")
leveneTest(G ~ Pstatus) #similar variances

x3<-G[Pstatus=="A"]
print(x3)
x2<-G[Pstatus=="T"]
print(x2)

ks.test(x3,x2) #similarly distributed
shapiro.test(G[Pstatus=="A"])
shapiro.test(G[Pstatus=="T"])
lillie.test(x3)
lillie.test(x2)

 #i have normality
t.test(x3,x2,mu=0,var.equal = T,alternative = "two.sided")#equal means

#G,Parents higher ed level
boxplot(G ~ parents_higher_ed, 
        main = "G vs parents higher ed",
        col = c("white","purple","blue"),
        xlab = "Parents with college education",
        ylab = "Grades")
leveneTest(G ~ parents_higher_ed) #similar variances 
anova_result <- aov(G ~ parents_higher_ed) #Parents educational level stat significant
summary(anova_result) #small p value

lillie.test(residuals(anova_result))
shapiro.test(residuals(anova_result))
#normality is rejected ,so non parametric test better here,
kruskal.test(G ~ parents_higher_ed)  #parents higher ed affects the final grades
pairwise.t.test(G,parents_higher_ed,pool.sd = T) 
#there is difference between levels going from none to one ,or from none to both

#G,studytime
boxplot(G ~ studytime, 
        main = "Boxplot of G vs study time",
        col = 3:6,
        xlab = "Study time",
        ylab = "Grades")
leveneTest(G ~ studytime) #similar variances
anova_result <- aov(G ~ studytime) #study time stat significant
summary(anova_result) #small p value

lillie.test(residuals(anova_result))
shapiro.test(residuals(anova_result))
#normality is rejected ,so non parametric test better here,
kruskal.test(G ~ studytime)  #study time affects the final grades

pairwise.t.test(G,studytime,pool.sd = T) 
#differences going from the first level to the other
#meaning ,studying less than 2 hours per week affect the grades
#d
#regression models

grades_portuguese$G1 <- NULL
grades_portuguese$G2 <- NULL
grades_portuguese$G3 <- NULL

model_1<-lm(G~.,data = grades_portuguese)
summary(model_1)
anova(model_1)
#full model ,13 variables significant

m2<-lm(G~school+sex+age+Mjob+Fjob+reason+guardian+studytime+failures+schoolsup+higher+goout+absences+parents_higher_ed,data = grades_portuguese)
anova(m2)
summary(m2)

mfull<-lm(G~.,data = grades_portuguese)
mnull<-lm(G~1,data = grades_portuguese)
best_model <- step(model_1,scope = list(lower=mnull,upper=mfull),
                   direction = "both")
summary(best_model)

best_model1 <- step(model_1,scope = list(lower=mnull,upper=mfull),
                   direction = "backward")
summary(best_model1)

best_model2 <- step(model_1,scope = list(lower=mnull,upper=mfull),
                   direction = "forward")
summary(best_model2)

#i choose m2 model ,i keep the stat significantly variables 
#the more complex models do not explain the model significantly better

##ASSUMPTIONS
#1 homoscedasticity

library(car)
ncvTest(m2)
yhat.quantiles<-cut(fitted(m2), breaks=quantile(fitted(m2), probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(m2)~yhat.quantiles)
par(mfrow=c(2,2))
#reject equal variances
boxplot(rstudent(m2)~yhat.quantiles)

#2 non linearity

residualPlot(m2,type="rstudent") #linearity does not hold

#multicolinearity
vif(m2) #no multicolinearity

#3 independence of errors
plot(rstudent(m2), type='l') #i have independance of errors

#4 normality
plot(m2,which=2)  #normality exists
par(mfrow=c(1,1))
shapiro.test(rstudent(m2)) #reject normality

lillie.test(rstudent(m2)) #normality exists

#ridge regression for m2
library(MASS)
m3<-lm.ridge(G~school+sex+age+Mjob+Fjob+reason+guardian+studytime+failures+schoolsup+higher+goout+absences+parents_higher_ed,data = grades_portuguese)
rbind(coef(m2),coef(m3))

m4<-lm(log(G)~school+sex+age+Mjob+Fjob+reason+guardian+studytime+failures+schoolsup+higher+goout+absences+parents_higher_ed,data = grades_portuguese)
summary(m4)
anova(m4)

##ASSUMPTIONS
#1 homoscedasticity

ncvTest(m4)
yhat.quantiles<-cut(fitted(m4), breaks=quantile(fitted(m4), probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(m4)~yhat.quantiles)
par(mfrow=c(2,2))
#homoscedasticity exists
boxplot(rstudent(m4)~yhat.quantiles)

#2 non linearity

residualPlot(m4,type="rstudent") #linearity holds

#multicolinearity
vif(m4) #no multicolinearity

#3 independence of errors
plot(rstudent(m4), type='l') #i have independance of errors

#4 normality
plot(m4,which=2)  #normality exists
par(mfrow=c(1,1))
shapiro.test(rstudent(m4)) #normality exists

lillie.test(rstudent(m4)) #normality exists

#model 4 fitts the data well, all assumptions are met

