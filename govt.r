getwd()
#install.packages("taRifx")
#install.packages("lattice")
library(lattice)
library(taRifx)
library(ggplot2)
setwd("/home/pramod/Documents/Kaggle/govt/")

s.data <-read.csv("all.csv",header = T,stringsAsFactors = F)

colnames(s.data)
class(s.data$Persons)

class(s.data)
#Breaking the data with required components. 

#Education in India
s.edu <- s.data[,c(1,2,18:30,35:38)]
#class(s.edu$Persons..literate)

colnames(s.edu)
#aggregate( speed ~ dive, df, mean )

#Fucntion to calculate the aggregate for 
agg <- function(x,y){
  aggregate(x~y,s.edu,mean)
}

male <- agg(s.edu$Males..Literate,s.edu$State)

colnames(male)[2] <- c("Male")
# Male literatute rate
male.lit <- agg(s.edu$Males..Literatacy.Rate,s.edu$State)
colnames(male.lit)[2] <- c("Male_Lit_Rate")
#Female
female <- agg(s.edu$Females..Literate,s.edu$State)
colnames(female)[2] <- c("Female")
#Female lit rate
female.lit <- agg(s.edu$Females..Literacy.Rate,s.edu$State)
colnames(female.lit)[2] <- c("Female_Lit_Rate")

#Person
person <- agg(s.edu$Persons..literate,s.edu$State)
colnames(person)[2] <- c("Person")
#Person lit rate 
person.lit <- agg(s.edu$Persons..literacy.rate,s.edu$State)
colnames(person.lit)[2] <- c("Person_Lit_Rate")

#class(person.lit$Person_Lit_Rate)

#Total educated
total.educated <- agg(s.edu$Total.Educated,s.edu$State)
colnames(total.educated)[2] <- c("Total_Educated")

#Data without level 
data_level <- agg(s.edu$Data.without.level,s.edu$State)
colnames(data_level)[2] <- c("Data_Level")

#Below primary 
below_primary <- agg(s.edu$Below.Primary,s.edu$State)
colnames(below_primary)[2] <- c("Below_primary")

# Primary 
primary <- agg(s.edu$Primary,s.edu$State)
colnames(primary)[2] <- c("Primary")

#Middle 
middle <- agg(s.edu$Middle,s.edu$State)
colnames(middle)[2] <- c("Middle")

#Matric
matric <- agg(s.edu$Matric.Higher.Secondary.Diploma,s.edu$State)
colnames(matric)[2] <- c("Matric")

#Grad
graduate <- agg(s.edu$Graduate.and.Above,s.edu$State)
colnames(graduate)[2] <- c("Graduate")

#Workers
workers <- agg(s.edu$Total.workers,s.edu$State)
colnames(workers)[2] <- c("Workers")

#main work 
main_workers <- agg(s.edu$Main.workers,s.edu$State)
colnames(main_workers)[2] <- c("Main_Worker")

#marginal work 
marginal_work <- agg(s.edu$Marginal.workers,s.edu$State)
colnames(marginal_work)[2] <- c("Marginal_Worker")

#NON WORKERS
non_work <- agg(s.edu$Non.workers,s.edu$State)
colnames(non_work)[2] <- c("Non_Work")
      

#District growth 
district_growth <- agg(s.data$Growth..1991...2001.,s.data$State)
colnames(district_growth)[2] <- c("District_growth")

#Rural population 

#Combining the data 
combined.data <- cbind.data.frame(person$Person,
                               person.lit$Person_Lit_Rate,
                              female$Female,
                              female.lit$Female_Lit_Rate,
                              male$Male,
                              male.lit$Male_Lit_Rate,
                              data_level$Data_Level,
                              total.educated$Total_Educated,
                               below_primary$Below_primary,
                              primary$Primary,
                              middle$Middle,
                              matric$Matric,
                              graduate$Graduate,
                              workers$Workers,
                              non_work$Non_Work,
                              marginal_work$Marginal_Worker,
                              main_workers$Main_Worker,
                              district_growth$District_growth)




##############


#Changing the col names

#Final data to work on
combined.data <- lapply(combined.data,round,2)
final_data <- cbind.data.frame(state_uniq,combined.data)
colnames(final_data) <- c("State","Person",
                          "Person_lit_rate",
                          "Female",
                          "Female_lit_rate",
                          "Male",
                          "Male_lit_rate",
                          "Data_level",
                          "Total_Educated",
                          "Below_Primary",
                          "Primary",
                          "Middle",
                          "Matric",
                          "Graduate",
                          "Worker",
                          "Non_Worker",
                          "Marginal_Worker",
                          "Main_Worker",
                          "District_growth")





#Plotting

qplot(x = final_data$Male_lit_rate,final_data$Female_lit_rate)
qplot(final_data$Person_lit_rate,final_data$Male_lit_rate)
per_male <- lm(final_data$Person_lit_rate~final_data$Male_lit_rate,data = final_data)
summary(per_male)
per_female <- lm(final_data$Person_lit_rate~final_data$Female_lit_rate,data = final_data)
summary(per_female)

xyplot(resid(per_male)~fitted(per_male), 
panel = function(x, y, ...)
{
  panel.grid(h = -1, v = -1)
  panel.abline(h = 0)
  panel.xyplot(x, y, ...)
}
)


qqmath( ~ resid(per_male),
        xlab = "Theoretical Quantiles",
        ylab = "Residuals"
)


xyplot(resid(per_female)~fitted(per_female), 
       panel = function(x, y, ...)
       {
         panel.grid(h = -1, v = -1)
         panel.abline(h = 0)
         panel.xyplot(x, y, ...)
       }
)

 qqmath( ~ resid(per_female),
xlab = "Theoretical Quantiles",
ylab = "Residuals"
)




mean(female.lit$Female_Lit_Rate)
mean(male.lit$Male_Lit_Rate)

colnames(final_data)

person_rate_female <- glm(final_data$Person_lit_rate ~ 
                     final_data$Female_lit_rate+final_data$Female,data = final_data,
                     family=poisson())
summary(person_rate_female)

person_rate_female <- glm(final_data$Person_lit_rate ~ 
                            final_data$Female_lit_rate+final_data$Female,data = final_data,
                          family=poisson())



