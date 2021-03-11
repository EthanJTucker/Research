library(lme4)
library(nlme)
library(tidyverse)

##Graphs and Models

Test.Data <- Gen_Data
Test.Data$`Faculty Random ID` <- factor(Test.Data$`Faculty Random ID`)
Test.Data$Student.Term <- factor(Test.Data$Student.Term)

model1 <- lmer(`GPA Assigned` ~  Student.Term + (1|`Course Code`/`Faculty Random ID`) , data = Gen_Data)

summary(model1)

plot(model1)
quick.data <- subset(Gen_Data[,1:11], size = 1000, replace)

##Rough visualization of GPA Assigned by term for first 6 Student.Terms. Points get sparser because enrollement declines as a function of Student.Term
Gen_Data %>%
        group_by(`Student Random ID`) %>%
                slice(n()) %>%
                filter(Student.Term <= 6) %>%
                summarise(max_Student.Term = Student.Term, mean_by_term = mean(`GPA Assigned`, na.rm = TRUE)) %>%
                        ggplot(aes(x = max_Student.Term, y = mean_by_term)) + 
                        geom_point(position = position_jitter(width = 0.3, height = 0.1), alpha = 0.1)

##This is a graph that shows the number of terms that students were enrolled
Gen_Data %>%
        group_by(Student.Term) %>%
                filter(Student.Term <= 10) %>%
                count() %>%
                        ggplot(aes(x = factor(Student.Term), y = n, fill = factor(Student.Term))) +
                        geom_bar(stat = "identity") +
                        labs(x = "Term", y = "Number of students", title = "Number of students that took math/chemistry courses Vs. term after first instance") +
                        theme(legend.position = "none")




##We want to filter by some selection of Student.Term to create the models. Students that take many terms in the math/chemistry departments are more likely to get worse grades. This can be shown by choosing filter(Student.Term > 4) and analysing GPA Assigned slope and change in Delta

Fourterm_Data <- Gen_Data %>%
                        filter(!is.na(`GPA Assigned`), max.terms == 4)

Fourterm_Data %>%
        filter(`Student Random ID` < 10000) %>%
                mutate(`Student Random ID` = factor(`Student Random ID`)) %>%
                        ggplot(aes(x = Student.Term, y = Delta,  color = `Student Random ID`)) +
                        geom_point(position = position_jitter(width = 0.2)) +
                        theme(legend.position = "none") +
                        geom_smooth(method = "lm", se = FALSE)

Temp.Data <- Gen_Data %>%
                filter(!is.na(`GPA Assigned`))

max.term.model <- lmer(`GPA Assigned` ~ Student.Term + (1|max.terms) + (1|`Student Random ID`), data = Temp.Data)
plot(max.term.model)
max.term.coef <- generics::tidy(max.term.model, conf.int = TRUE)


max.term.coef %>%
        filter(effect == "fixed" & term != "(Intercept)") %>%
        ggplot(., aes(x = term, y = estimate,
                      ymin = conf.low, ymax = conf.high)) +
        geom_hline(yintercept = 0, color = 'red') + 
        geom_point() +
        geom_linerange() +
        coord_flip() +
        theme_bw() +
        ylab("Coefficient estimate and 95% CI") +
        xlab("Regression coefficient")

summary(max.term.model)


##Create Just_Math_First and Just_Chem_First variables and merge to Gen_Data



##Need to introduce first faculty for chem and math to do effects

for(i in min(Gen_Data$max.terms):max(Gen_Data$max.terms)){
n.term_Cum.GPA <- Gen_Data %>%
                        filter(max.terms == i)

n.term.model <- lmer(`Cum.GPA` ~ Student.Term + (1|`Student Random ID`), data = Gen_Data)

n.term.coef <- generics::tidy(n.term.model, conf.int = TRUE)

n.term.coef %>%
        filter(effect == "fixed" & term != "(Intercept)") %>%
        ggplot(., aes(x = term, y = estimate,
                      ymin = conf.low, ymax = conf.high)) +
        geom_hline(yintercept = 0, color = 'red') + 
        geom_point() +
        geom_linerange() +
        coord_flip() +
        theme_bw() +
        ylab("Coefficient estimate and 95% CI") +
        xlab("Regression coefficient")
}

##First course department generator


Temp_Data <-Gen_Data %>%
                arrange(`Student Random ID`) %>%
                        filter(Student.Term == 1) %>%
                                        mutate(First.Course.Department = "", First.Course.Faculty = 0, First.Course.Code = "")
                                                
##This line shows that 
length(unique(Temp_Data$`Student Random ID`)) - length(Temp_Data$`Student Random ID`)


current.student <- Temp_Data$`Student Random ID`[1]
previous.student <- 0
for(i in 1:n){
        current.student <- Temp_Data$`Student Random ID`[i]
        
        if(current.student != previous.student){
                if(current.student != Temp_Data$`Student Random ID`[i+1]){
                        Checker <- TRUE
                } else{ Checker <- FALSE}
                
                if(Checker == TRUE){
                        current.first.course <- Temp_Data$`Course Code`[i]
                        current.first.department <- Temp_Data$Department[i]
                        current.first.faculty <- Temp_Data$`Faculty Random ID`[i]
                } else{
                        total.first.courses <- Temp_Data
                }
                
        }
        
        Temp_Data$First.Course.Department[i] <- current.first.department
        Temp_Data$First.Course.Faculty[i] <- current.first.faculty
        Temp_Data$First.Course.Code[i] <- current.first.course
        
        previous.student <- Gen_Data$`Student Random ID`[i]
}








