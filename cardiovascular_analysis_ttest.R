library(DBI)
library("dplyr")
library("ggpubr")
library(bit64)

# Connect to 'ahisantos' MySQL database
con <- DBI::dbConnect(odbc::odbc(),
                      Driver    = "MySQL",
                      Server    = "3.84.158.190",
                      Database  = "ahisantos",
                      UID       = "dba",
                      PWD       = "ahi2020",
                      Port      = "3306")

# This dataset consists of 70 000 records of patients data in 12 columns, 
# such as age, gender, systolic blood pressure, diastolic blood pressure, and etc. 
# The target class "cardio" equals to 1 when patient has cardiovascular disease (CVD), and 
# 0 if the patient is healthy.

# The task is to compare the AGE, SEX, WEIGHT, BMI of CVD and healthy patients [groups].


# Load "Cardiovascular_Disease" table FROM ahisantos database
df <- dbReadTable(con, "Cardiovascular_Disease")


# First, let's examine our dataset
head(df)
dim(df)
colnames(df)
str(df)
# All features are numerical, 12 integers and 1 decimal number (weight).
# Measurements: height=cm, weight=kg, age=days.


# Cleaning the data:     

# Let's change AGE to years.
counts <- table(df$cardio, df$age)
years <- (df[c("age")]/365)
years_rounded <- round(years)

# Smaller and cleaner database
cleandf_sample <- df %>% select (gender, height, weight, years, active, cardio, cholesterol, gluc)


# Renaming columns: gender to sex, years to age, cardio to CVD
cleandf_sample <- cleandf_sample %>% rename(sex = gender, age = years, CVD = cardio)


# It is unclear whether whether "1" and "2" stands for male or female in the gender column. 
# To determine, let's calculate the mean of height per gender assuming that the larger 
# height average is male.
cleandf_sample %>%                            #dataframe
  group_by(sex) %>%                           #group indicator
  summarise_at(vars(height),                  #column
               list(name = mean))             #function
# "1" equals to about 161cm while "2" equals to about 169cm, therefore "1"=female, "2"=male

# Let's change 1 and 2 to F and M
cleandf_sample$sex <- sub("1", "F", cleandf_sample$sex)
cleandf_sample$sex <- sub("2", "M", cleandf_sample$sex)

# Let's also change CVD to something more readible, 0=healthy 1=CVD
cleandf_sample$CVD <- sub("0", "healthy", cleandf_sample$CVD)
cleandf_sample$CVD <- sub("1", "CVD", cleandf_sample$CVD)

# Age group represented in this dataset.
counts = (cleandf_sample %>% group_by(age) %>% summarize(count=n()))
      # Results: This dataset include patients ages 30 to 65.

# Let's add BMI:
height_meters <- cleandf_sample$height/100                       # Convert from cm to m 
cleandf_sample$BMI <- (cleandf_sample$weight/(height_meters^2))  # BMI formula
head(cleandf_sample)                                             # check



# Analysis: Descriptive stat, distribution Plots, normality test, homogeneity of variance 

# Descriptive statistics of WEIGHT:
# Mean, median, 25th and 75th quartiles, min, max for weight
summary(cleandf_sample$weight, na.rm=TRUE)
          # Results:
          #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
          #   10.00   65.00   72.00   74.21   82.00  200.00 

# Another method is using stat.desc from the pastects library
library(pastecs)
weight_stats <- stat.desc(cleandf_sample$weight) # Results were in scientific notation
options(scipen=100)                              # Let's change the format of the display
options(digits=2)
stat.desc(cleandf_sample$weight, basic=F)
# Results:
#   median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#   72.000       74.206        0.054        0.107      207.238       14.396        0.194



# Distribution plots:

# Let's see the AGE distribution of CVD and healthy patients using a density plot 
library(ggpubr)                                              # Use ggdensity from package ggpubr.
ggdensity(cleandf_sample, x = "age", fill = "CVD",
          add = "mean", rug = TRUE)
# This density plot suggest that CVD affects older patients

# Using a different method, let's see the CVD and WEIGHT distribution in this dataset.
# First, calculate the mean of each group: 
library(plyr)                                     # This method uses plyr and ggplot2
mu <- ddply(cleandf_sample, "CVD", summarise, grp.mean=mean(weight))
head(mu)
          # Results:
          #      CVD    grp.mean
          # 1   CVD         77
          # 2   healthy     72
# Create density plot.
library(ggplot2)
ggplot(cleandf_sample, aes(x=weight, color=CVD)) +
  geom_density()
# Add mean lines
p<-ggplot(cleandf_sample, aes(x=weight, color=CVD)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=CVD),
             linetype="dashed")
p
# This density plot suggest that patients with CVD tend to weigh more than healthy patients.



# Normality tests:

# Shapiro normality test for WEIGHT:
library(nortest)
# This test rejects the hypothesis of normality when the p-value is ≤ 0.05.
# First we need to prepare our sample by converting our objects from character to numeric:
cleandf_sample$weight <- as.numeric(as.character(cleandf_sample$weight))     
# The Shapiro normality test caps at 5000 observations so let's make a smaller sample.
weight_sample <- sample_n(cleandf_sample, 5000)
shapiro.test(weight_sample$weight)               # Shapiro test
          # Result:
          # W = 1, p-value < 0.0000000000000002
# The p-value from Shapiro test suggests REJECTING the null hypothesis. 



# Since our data is so large, let's also try the Anderson Darling normality test to see if 
# there's a difference.
ad.test(cleandf_sample$weight)
          #Results
          # A = 654, p-value < 0.0000000000000002
# This test assumes that there are no parameters to be estimated in the distribution being 
# tested, in which case the test and its set of critical values is distribution-free.
# The p-value from Anderson test suggests REJECTING the null hypothesis.

# The p-value represented in both the Shapiro-Wilk and Anderson-Darling test falls below 0.05.
# The null hypothesis is REJECTED suggesting a NON-NORMALLY distributed population.



# Homogeneity of variance:

# Our data is non-normal so we will be using the Levene test. Levene's test is used to assess 
# the equality of variances for a variable calculated for two or more groups.

# First, let's set our numeric vectors: 
group <- cleandf_sample$CVD
y <- cleandf_sample$weight
# Now let's do our test.
library(car)                            # This test requires the car package
leveneTest(y, group, location=c("median", "mean", "trim.mean"), trim.alpha=0.25,
            bootstrap = FALSE, num.bootstrap=1000, kruskal.test=FALSE, 
            correction.method=c("none","correction.factor","zero.removal","zero.correction"))
          # Results (center = median: c("median", "mean", "trim.mean"))
          #         Df   F value              Pr(>F)    
          #group     1     426                <0.0000000000000002 ***
          #        69998

# The Levene test is used to assess the equality of variances for a variable calculated 
# for two or more groups asses the null hypothesis that the population variances are equal. 
# Since the resulting p-value of Levene's test is less than 0.05 significance level, 
# the obtained differences in sample variances are unlikely to have occurred based on random 
# sampling from a population with equal variances. 

# Thus, the null hypothesis of equal variances is rejected and it is concluded that there is 
# a difference between the variances in the population.


# One way ANOVA test:
# We have two groups to compare: CVD and healthy patients

# Comparing BMI of CVD and healthy patients: 
group_by(cleandf_sample, CVD) %>%
  summarise(
    count = n(),
    mean = mean(BMI, na.rm = TRUE),
    sd = sd(BMI, na.rm = TRUE)
  )
          # Results:
          #   CVD     count    mean       sd
          #  <chr>    <int>   <dbl>      <dbl>
          #  CVD      34979    28.6      6.38
          # healthy   35021    26.5      5.61

# This ANOVA test shows that, while there are more healthy patients in this database,
# the mean and standard deviation (SD) are higher for CVD patients. This suggest that CVD 
# patients are more likely to have higher BMIs. 

# Comparing age of CVD and healthy patients:
group_by(cleandf_sample, CVD) %>%
  summarise(
    count = n(),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE)
  )
          # Results:
          #   CVD     count    mean       sd
          #  <chr>    <int>   <dbl>      <dbl>
          #  CVD      34979    54.9      6.35
          # healthy   35021    51.7      6.79
# This result suggests that cardiovascular disease is more like to affect older patients.



# Visuals: Box plot, bar graph

CVD_count = cleandf_sample %>% filter(CVD == "CVD")   # Filtering CVD patients

# Box plots showing the shape of the distribution, central value, and variability. 
# of CVD and WEIGHT grouped by SEX
ggboxplot(cleandf_sample, x = "sex", y = "weight", 
          color = "CVD", palette = c("#FF1234", "#E7B800"),
          ylab = "weight in kg", xlab = "Sex")
# This box plot suggest that both male and female patients with CVD weigh more than healthy 
# patients.



# Bar graph showing AGE and WEIGHT distribution for CVD and healthy patients.
p<-ggplot(cleandf_sample, aes(x=age, y=weight, fill=CVD)) +
  geom_bar(stat="identity")+theme_minimal()
p
# This bar graph suggest that CVD is more prevalent in older patients.  


