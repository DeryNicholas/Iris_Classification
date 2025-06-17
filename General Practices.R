getwd()
setwd("C:/Users/NICHOLAS/Desktop/RFILES")
# How to concatenate in R.
N<-"You are a bad boy,"
M<-"your parents even said it"
K<-cat(N,M)
K<-paste(N,M)
paste(N,M)
  

# Basic Plotting 
N<-c(seq(from=3,to=9 , by=2))
N
M<-c(seq(2,11,3))
M
plot(N,M)
boxplot(N,M)
hist(N,M)
barplot(N,M)
# Creating a dataset for plotting.
# Create the data frame
df <- data.frame(
  Age = c(20, 47, 23, 53, 19, 5.0, 98, 86, 32, 52),
  Sex = c("Male", "Male", "Female", NA, "Female", "Female", NA, "Male", "Male", "Female"),
  Scores = c(40, 80, 29, 90, 98, 99, 38, 29, 47, 79),
  GPA = c(3.4, 4.0, NA, 2.5, 5.0, 3.8, NA, 5.0, 2.7, 2.1)
)

# Inspect the structure of the data frame
str(df)

# Plotting the data
plot(df$Age, df$Scores,
     type = "b",                       # Both points and lines
     main = "Plotting My Own Dataset", # Main title
     xlab = "Age",                     # X-axis label
     ylab = "Scores",                  # Y-axis label
     pch = 19,                         # Point character
     col = "blue")                     # Color of points and lines
# Remove rows with any NA values
df_cleaned <- na.omit(df)

# Display the cleaned data frame
print(df_cleaned)

# Remove rows with any NA values using complete.cases
df_cleaned <- df[complete.cases(df), ]

# Display the cleaned data frame
print(df_cleaned)
barplot(df$Age,df$Scores,
        type="b",
        main="Trying with my own dataset",
        xlab="Age",
        ylab ="Scores",
        col = "Green" )
library(tidyverse)
# Plotting Age vs Scores
plot(df$Age, df$Scores,
     type = "o",                   # Type "o" for both points and lines
     main = "Age vs Scores",       # Main title
     lwd = 2,                      # Line width
     col = "red",                  # Color of the points and lines
     cex = 1.5,                    # Size of the points (cex = character expansion)
     xlab = "Age",                 # X-axis label
     ylab = "Scores",              # Y-axis label
     lty = 2)                      # Line type (2 = dashed)
# Plotting GPA and Scores..
plot(df$GPA,df$Scores,
     type = "l",
     
     lwd=2.3,
     lty=1,
     cex=1.3,
     main = "GPAvsScores",
     col="Blue",
     xlab = "GPA",
     ylab = "Scores")
# loading the necessary libraries.
library(psych)
pairs(df%>%select("Age","Scores"))
summary(df)
describe(df)      
df1<-cor(df$Age,df$Scores,
         method = "kendall")
# Calculate the correlation matrix for the entire data frame
df2 <-df[sapply(df,is.numeric)]
df3<-cor(df2)
df3
df
# Removing NA from the dataset.
Mt<-na.omit(df)
Mt
# converting categorical variables to levels.
mt<-factor(Mt,levels = c("Male","Female"))
mt

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Trying another dataset.
# Load the necessary libraries
library(dplyr)
library(tidyr)

# Create a sample data frame
df <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David", "Eve","James","Kwame","Esther","Abena","Adjoa","John"),
  Sex = c("Female", "Male", "Male", "Female", "Female","Male","Male","Female","Female","Female","Male"),
  Age = c(25, 30, 35, 40, 45,34,12,25,4.5,NA,NA,389),
  Subject = c("Math", "Math", "English", "English", "Math"),
  Score1 = c(90, 85, 88, 92, 95,NA,NA,2.5,1111,NA,NA,4),
  Score2 = c(85, 80, 82, 88, 90,NA,NA,40,NA,3.4,1000,0.1)
)

# Filtering rows
filtered_data <- df %>% filter(Age > 30 & Sex == "Female")
filtered_data
# Selecting columns
selected_data <- df %>% select(Name, Age, Subject)
selected_data
# Mutating data
mutated_data <- df %>% mutate(Total_Score = Score1 + Score2)
mutated_data
# Summarizing data
summary_data <- df %>% summarize(Average_Age = mean(Age, na.rm = TRUE))
summary_data
# Grouping data
grouped_data <- df %>% 
  group_by(Sex, Subject) %>%
  summarize(Average_Score1 = mean(Score1, na.rm = TRUE),
            Average_Score2 = mean(Score2, na.rm = TRUE))
grouped_data
# Reshaping data (wide to long format)
long_data <- df %>%
  pivot_longer(cols = c(Score1, Score2),
               names_to = "Test",
               values_to = "Score")
long_data

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ

#  Data Wragling.
#Loading the necessary library.
library(mdsr)
library(lubridate)
library(dplyr)
Cancer<-read.csv("cancer.csv")
Cancer
str(Cancer)
names(Cancer)
# Checking for missing values.
is.na(Cancer)
sum(is.na(Cancer))
# Filtering the "M" means.
Mag<-Cancer%>%
  filter(diagnosis=="M")%>%
  select(ends_with("mean"))
Mag
View(Mag)
# Filtering the "B" means.
Bi<-Cancer%>%
  filter(diagnosis=="B")%>%
  select(ends_with("mean"))
Bi
str(Bi)
# Summary of the diagosis.
summary(Mag)
summary(Bi)
#Number of missing values in Mag
is.na(Mag)
sum(is.na(Mag))
# Checking the relationships between the means of these two numerical variables.
library(corrplot)
library(tidyverse)
selected<-Cancer%>%
  select(ends_with("mean"))
selected
view(selected)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
volcano<-read.csv("volcano.csv")
volcano
str(volcano)
view(volcano)
#Selecting some of the variables in the volcano dataset.
select_vl<-volcano %>%
  select(volcano_number,primary_volcano_type,region)
select_vl
view(select_vl)
# Counting the number of primary volcano type.
Count_vl<-volcano %>%
  count(primary_volcano_type,sort = TRUE)%>%
  view()
library(dplyr)
library(stringr)
Select_vl<-Count_vl %>%
  mutate(primary_volcano_type=str_replace_all(primary_volcano_type,"\\(",""))
view(Select_vl)

# Counting the number of regions.
Reg_count<-select_vl %>%
  count(region,sort = TRUE)%>%
  View()

# Ploting the volcano types.
library(ggplot2)

#Counting the primary volcano type.
volcano_type_count<-volcano %>%
  count(primary_volcano_type)

#Reordering the plots from the highest to the lowest.
volcano_type_count%>%
  mutate(primary_volcano_type=fct_reorder(primary_volcano_type,n))%>%
ggplot(volcano_type_count,aes(x=primary_volcano_type, y=n))+
  geom_col()+
  coord_flip()

# plotting volcano types by regions.
volcano_type_count<-select_vl %>%
  count(region,primary_volcano_type)%>%
  view()

volcano_type_count%>%
  ggplot(volcano_type_count,aes(x=primary_volcano_type,y=n))+
  geom_col() +
  coord_flip() +
  facet_wrap(~region)
#3####################################################################
######################################################################
airport<-read.csv("Airport.csv")
airport

#Counting the number of destination cities
library(dplyr)
count_dest<-airport %>%
  count(DEST_CITY_NAME,ORIGIN_CITY_NAME)%>%
  View()







df<-data.frame(
  Gender=c("Female","Female","Female","Male","Male","Female","Female","Male","Male","Female",
           "Male","Male","Female","Male","Female","Female","Male","Female","Male","Female"),
  race_ethnicity=c("group B","group C","group B","group A","group C","group B","group B","group D",
                   "group B","group B","group C","group D","group B","group A","group A","group C",
                   "group C","group B","group C","group C"),
  parental_level_of_education<-c("Bachelor's degree","Some college","Master's degree","Associate degree",
                                 "Some college","Associate degree","Some college","Some college","High school",
                                 "High school","Associate degree","Associate degree","High school","Some college",
                                 "Master's degree","Some High school","High school","Some High school","High schoo",
                                 "Master's degree"),
  lunch<-c("standard", "standard","standard","free/reduced","standard","standard","standard","free/reduced","free/reduced",
           "free/reduced","standard","standard","standard","standard","standard","standard","standard",
           "free/reduced","free/reduced","free/reduced"),
  test_preparation_course<-c("none","completed","none","none","none","none","completed","none","completed","none","none","none",
                             "completed","none","none","none","completed","none","none","completed"),
  maths_score<-c(72,69,90,47,76,71,88,40,64,38,58,40,65,78,50,69,88,18,46,34),
  reading_score<-c(72,90,95,57,78,83,95,43,64,60,54,52,81,72,53,75,89,32,42,58),
  writting_score<-c(74,88,93,44,75,78,92,38,67,50,52,43,73,70,58,78,86,28,46,61)
)
  
  
View(df)


# Sample data
scores <- c(72, 69, 90, 47, 76, 71, 88, 40, 64, 38, 58, 40, 65, 78, 50, 69, 88, 18, 46, 34)

# Calculate Q1, Q3, and IQR
Q1 <- quantile(scores, 0.25)
Q3 <- quantile(scores, 0.75)
IQR <- Q3 - Q1

# Determine lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- scores[scores < lower_bound | scores > upper_bound]

# Print outliers
print(outliers)






#Checking for students who completed the test preparation course.
Completed<-student%>%
  filter(test.preparation.course=="completed")
Completed

#Counting the number of students that completed their test preparation course
Number_of_comp<-nrow(Completed)


#checking for students who could not complete their test preparation course.
Could_Not<-student%>%
  filter(test.preparation.course=="none")
Could_Not
view(Could_Not)

#So counting those that could not complete their test preparation course.
count_student_none<-nrow(Could_Not)

#Putting those that completed and those that could not in one variable
Completed_and_none<-data.frame(
  group=c("Completed","Could_Not"),
  count=c(Number_of_comp,count_student_none))

Completed_and_none


#Barplotting the number of students that completed the test preparation course and
# those that could not.
library(ggplot2)
ggplot(Completed_and_none,aes(x=group,
                              y=count,fill = group))+
  geom_bar(stat = "identity",width = 0.5)+
  scale_fill_manual(values = c("Completed"="Red","Could_Not"="Blue"))+
  labs(
    title = "Number of students by test preparation course completion",
    x="Test Preparation Course",
    y="Number of Students",
    caption = "Dataset:StudentsPerformance")+
  theme_minimal()+
  geom_text(aes(label = count),vjust=-0.5)










#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

getwd()
setwd("C:/Users/NICHOLAS/Desktop/RFILES")
library(tidyverse)
library(dplyr)
library(psych)



eruption<-read.csv("eruptions.csv")
eruption
view(eruption)

#structure of the eruption data.
str(eruption)
glimpse(eruption)

#checking the volcano names.
unique(eruption$volcano_name)

#Checking for missing values.
colSums(is.na(eruption))

#Replacing the missing values in the categorical variables with their respective column mode.
#Checking the numerical values to replace them with their mean.
eruption%>%select_if(is.numeric)
eruption$vei[is.na(eruption$vei)]=mean(eruption$vei,na.rm = TRUE)

#Replacing the missing values in categorical variables.
eruption$area_of_activity[is.na(eruption$area_of_activity)]<-
  is.character(names(sort(table(eruption$area_of_activity),decreasing=TRUE[1])))

eruption$evidence_method_dating[is.na(eruption$evidence_method_dating)]<-
  is.character(names(sort(table(eruption$evidence_method_dating),decreasing = TRUE[1])))


#Replacing the missing years,months and days with the most appearing years,months and days respectively.
eruption$start_year[is.na(eruption$start_year)]<-
  is.character(names(sort(table(eruption$start_year),decreasing = TRUE[1])))
 

eruption$start_month[is.na(eruption$start_month)]<-
  is.numeric(names(sort(table(eruption$start_month),decreasing = TRUE[1])))


eruption$start_day[is.na(eruption$start_day)]<-
  is.numeric(names(sort(table(eruption$start_day),decreasing = TRUE[1])))


eruption$end_day[is.na(eruption$end_day)]<-
  is.numeric(names(sort(table(eruption$end_day),decreasing = TRUE[1])))

eruption$end_year[is.na(eruption$end_year)]<-
  is.character(names(sort(table(eruption$end_year),decreasing = TRUE[1])))

eruption$end_month[is.na(eruption$end_month)]<-
  is.numeric(names(sort(table(eruption$end_month),decreasing = TRUE[1])))


colSums(is.na(eruption))
 

str(eruption)

view(head(eruption))
