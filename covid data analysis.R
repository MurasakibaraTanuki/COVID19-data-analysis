"""
Created in 20/02/2025
By: Anton Berezin 
GitHub Account: https://github.com/MurasakibaraTanuki

"""
###################################################### Description ------------- 
""" Description:The dataset was provided by the Mexican government. 
This dataset contains an enormous number of anonymized patient-related information including pre-conditions. 
The raw dataset consists of 21 unique features and 1,048,576 unique patients. In the Boolean features, 1 means 'yes' and 2 means 'no'. 
Values as 97 and 99 are missing data.
sex: 1 for female and 2 for male.
patient type: type of care the patient received in the unit. 1 for returned home and 2 for hospitalization.
age: of the patient.
classification: covid test findings. Values 1-3 mean that the patient was diagnosed with covid in different
degrees. 4 or higher means that the patient is not a carrier of covid or that the test is inconclusive.
patient type: type of care the patient received in the unit. 1 for returned home and 2 for hospitalization.
pneumonia: whether the patient already have air sacs inflammation or not.
pregnancy: whether the patient is pregnant or not.
diabetes: whether the patient has diabetes or not.
copd: Indicates whether the patient has Chronic obstructive pulmonary disease or not.
asthma: whether the patient has asthma or not.
inmsupr: whether the patient is immunosuppressed or not.
hypertension: whether the patient has hypertension or not.
cardiovascular: whether the patient has heart or blood vessels related disease.
renal chronic: whether the patient has chronic renal disease or not.
other disease: whether the patient has other disease or not.
obesity: whether the patient is obese or not.
tobacco: whether the patient is a tobacco user.
usmer: Indicates whether the patient treated medical units of the first, second or third level.
medical unit: type of institution of the National Health System that provided the care.
intubed: whether the patient was connected to the ventilator.
icu: Indicates whether the patient had been admitted to an Intensive Care Unit.
date died: If the patient died indicate the date of death, and 9999-99-99 otherwise.
"""




#-------------------------- Loading the libraries ------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(shiny)
library(caret)
library(car)
#----------------------------- Loading the data --------------
covid_data <- read.csv("C:\\Users\\Shizik\\Desktop\\Analysis Projects\\covid data project\\Covid Data.csv",header = TRUE,stringsAsFactors = FALSE)

#------------------------ Inspection and Cleaning ------------
# first, we will substitute missing values with NA 
apply(covid_data, MARGIN = 2, FUN = function(x) summary(factor(x)))

# (99,98,97,9999-99-99) list of decoded na values 
# function for substitution 
column_names <- colnames(covid_data)[which(!(colnames(covid_data) %in% c("DATE_DIED", "AGE")))]
covid_data <- covid_data %>%  
  mutate(across(all_of(column_names),~ ifelse(. %in% c(99,98,97,"9999-99-99"),NA,.)))  

# I want to work separately on some columns, preparind propper 0 1 values 
covid_data$DATE_DIED <- sapply(covid_data$DATE_DIED,FUN = function(x) ifelse (
  x=="9999-99-99",NA,x
))
covid_data$DATE_DIED <- as.Date(covid_data$DATE_DIED,format = "%d/%m/%Y")
covid_data$SEX <- sapply(covid_data$SEX,function(x) ifelse(x==2,1,0))
covid_data$PREGNANT <- sapply(covid_data$PREGNANT, function(x) ifelse(x==2,0,x))
which(covid_data$SEX == 1 & covid_data$PREGNANT == 1)
colnames(covid_data) <- stringr::str_to_lower(colnames(covid_data))
column_names <- covid_data %>% select(diabetes:tobacco) %>% colnames()
column_names <- c(column_names,"pneumonia")

covid_data <- covid_data %>% mutate(across(all_of(column_names),~ifelse(.==2,0,.)))

covid_data <- covid_data %>% 
  mutate(classiffication_final = ifelse(clasiffication_final>=4,0,clasiffication_final))
covid_data <- covid_data %>% select(-c(clasiffication_final))

# creating a column of dead or not 
covid_data$death_status <- ifelse(! is.na(covid_data$date_died), 1,0)
sum(covid_data$death_status) # 76942 people died
covid_data <- covid_data %>% select(-c(date_died))
# plot of NA values 
na_procent = apply(covid_data,MARGIN = 2,FUN= function(x) mean(is.na(x)))
na_procent = data_frame(colnames = names(na_procent), na_percent = as.numeric(na_procent)*100)
na_procent = na_procent[na_procent["na_percent"] > 0,]
ggplot(na_procent,aes(x = reorder(colnames,na_percent), y = na_percent)) + geom_bar(stat = "identity") + coord_flip() + 
  labs(
    title = "Percent of missing values",
    x = "Colum Names",
    y = "Percentage"
  )
## inspection of age column 
ggplot(covid_data,aes(age))+geom_boxplot()

# 0 of age patients may be a mistake in the data or infants. 
sum(covid_data$age>89)
# we have 3623 people older than 89 

# creation of age groups according with classification by world health organization
covid_data <- covid_data %>% mutate(age_group = cut(
  age,
  breaks = c(-Inf, 1, 3, 12, 18, 39, 59, 69,79,Inf),
  labels = c("Infancy", "Toddler", "Childhood", "Adolescence", 
             "Adulthood", "Middle age", "Young old","Middle old","Very Old"),
  right = TRUE
))
covid_data$patient_type <- ifelse(covid_data$patient_type==2,1,0) # 2 for hospitalization, convert to be 1 for hospitalization 0 for going home

# -------- grouping the data by age group and corona classification in order to calculate percentage of deaths and infected people.----------
First_graph_data <- covid_data %>% group_by(age_group,classiffication_final) %>%
  summarise(amount = n(),anount_died = sum(death_status),percentage_of_death = round((anount_died/amount)*100,digits = 2),.groups = "drop")
First_graph_data2 <- covid_data %>% group_by(age_group) %>% 
  summarise(percentage_of_infected_according_to_patients_in_the_age_group = n())

First_graph_data <- merge(First_graph_data,First_graph_data2 ,by = "age_group",all = TRUE)
First_graph_data$percentage_of_infected_according_to_patients_in_the_age_group = round((First_graph_data$amount / First_graph_data$percentage_of_infected_according_to_patients_in_the_age_group)*100,digits = 2)
rm(First_graph_data2)
# important note that 0 group is a group that the infection with coirona virus was not! confirmed, for the analysis we will treat them as "not infected"

# ----------- "Percentage of Corona Virus Infected (Divided by Age Groups and Severity) from all patients received"----------
ggplot(First_graph_data,aes(age_group,percentage_of_infected_according_to_patients_in_the_age_group,fill = age_group)) + facet_grid(vars(classiffication_final)) +geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("Infancy" = "#FFC0CB",
                               "Toddler years" = "#ADD8E6",
                               "Childhood" = "#FFFF00",
                               "Adolescence" = "#FFA500",
                               "Adulthood" = "#32CD32",
                               "Middle age" = "#008080",
                               "Young old" = "#9370DB",
                               "Middle old" = "#5F9EA0",
                               "Very Old" = "#D3D3D3"),name = "Age Groups",
                    labels = c(
                      "Infancy" = "Baby (0-1 yr)",
                      "Toddler years" = "Toddler (2-3 yrs)",
                      "Childhood" = "Childhood (4-12 yrs)",
                      "Adolescence" = "Teenager (13-18 yrs)",
                      "Adulthood" = "Adult (19-39 yrs)",
                      "Middle age" = "Midlife (40-59 yrs)",
                      "Young old" = "Young Senior (60-74 yrs)",
                      "Middle old" = "Middle Senior (75-89 yrs)",
                      "Very Old" = "Elder (90+ yrs)"))+
  theme(
    axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.95),
    legend.text = element_text(size=8,face = "bold")
  )+
  labs(
    title = "Percentage of Corona Virus Infected (Devided by Age Groups and Severity) from all patients received",
    y = "Number of Patients",
    x = "Age Groups"
  ) +  geom_text(aes(label=percentage_of_infected_according_to_patients_in_the_age_group),
                 position=position_stack(vjust = 0.5),size= 4,colour= "black")
                                   
                                   






# Using Shiny for dashboard -------
# Define UI
ui <- fluidPage(
  titlePanel("Corona Virus Data Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "classification",
        label = "Select Severity Classification:",
        choices = unique(First_graph_data$classiffication_final),
        selected = unique(First_graph_data$classiffication_final)[1]
      ),
      selectInput(
        inputId = "chart_type",
        label = "Select Chart Type:",
        choices = c("Bar Chart", "Line Chart", "Pie Chart"),
        selected = "Bar Chart"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", plotOutput("plot")),
        tabPanel("Summary", tableOutput("summary"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive data filtered by severity classification
  filtered_data <- reactive({
    First_graph_data %>%
      filter(classiffication_final == input$classification)
  })
  
  # Plot output
  output$plot <- renderPlot({
    data <- filtered_data()
    
    if (input$chart_type == "Bar Chart") {
      ggplot(data, aes(age_group, amount, fill = age_group)) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_y_continuous(labels = comma) +
        scale_fill_manual(
          values = c(
            "Infancy" = "#FFC0CB",
            "Toddler years" = "#ADD8E6",
            "Childhood" = "#FFFF00",
            "Adolescence" = "#FFA500",
            "Adulthood" = "#32CD32",
            "Middle age" = "#008080",
            "Young old" = "#9370DB",
            "Middle old" = "#5F9EA0",
            "Very Old" = "#D3D3D3"
          )
        ) +
        labs(
          title = paste("Severity:", input$classification),
          x = "Age Groups",
          y = "Number of Patients"
        ) +
        theme_minimal()
    } else if (input$chart_type == "Line Chart") {
      ggplot(data, aes(age_group, amount, group = 1, color = age_group)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_y_continuous(labels = comma) +
        labs(
          title = paste("Severity:", input$classification),
          x = "Age Groups",
          y = "Number of Patients"
        ) +
        theme_minimal()
    } else if (input$chart_type == "Pie Chart") {
      ggplot(data, aes(x = "", y = amount, fill = age_group)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        scale_fill_manual(
          values = c(
            "Infancy" = "#FFC0CB",
            "Toddler years" = "#ADD8E6",
            "Childhood" = "#FFFF00",
            "Adolescence" = "#FFA500",
            "Adulthood" = "#32CD32",
            "Middle age" = "#008080",
            "Young old" = "#9370DB",
            "Middle old" = "#5F9EA0",
            "Very Old" = "#D3D3D3"
          )
        ) +
        labs(
          title = paste("Severity:", input$classification),
          fill = "Age Groups"
        ) +
        theme_void()
    }
  })
  
  # Summary table output
  output$summary <- renderTable({
    filtered_data() %>%
      group_by(age_group) %>%
      summarise(Total = sum(amount)) %>%
      arrange(desc(Total))
  })
}

# Run the app
shinyApp(ui = ui, server = server)


# --------- plot of deaths by age and infection severity (percent of people in each infection severity group) -------------- 

ggplot(First_graph_data,aes(x=age_group,y = percentage_of_death,fill = age_group)) + 
  facet_grid(vars(classiffication_final)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("Infancy" = "#FFC0CB",
                               "Toddler years" = "#ADD8E6",
                               "Childhood" = "#FFFF00",
                               "Adolescence" = "#FFA500",
                               "Adulthood" = "#32CD32",
                               "Middle age" = "#008080",
                               "Young old" = "#9370DB",
                               "Middle old" = "#5F9EA0",
                               "Very Old" = "#D3D3D3"),name = "Age Groups",
                    labels = c(
                      "Infancy" = "Baby (0-1 yr)",
                      "Toddler years" = "Toddler (2-3 yrs)",
                      "Childhood" = "Childhood (4-12 yrs)",
                      "Adolescence" = "Teenager (13-18 yrs)",
                      "Adulthood" = "Adult (19-39 yrs)",
                      "Middle age" = "Midlife (40-59 yrs)",
                      "Young old" = "Young Senior (60-74 yrs)",
                      "Middle old" = "Middle Senior (75-89 yrs)",
                      "Very Old" = "Elder (90+ yrs)")) +
  theme(
    axis.text.x = element_text(angle = 45,hjust = 1,vjust = 0.95),
    legend.text = element_text(size=8,face = "bold")
  )+
  labs(
    title = "Percentage of Corona Virus Infected (Devided by Age Groups and Severity) from all patients received",
    y = "Number of Patients",
    x = "Age Groups"
  ) +  geom_text(aes(label=percentage_of_death),
                 position=position_stack(vjust = 0.5),size= 4,colour= "black")
# We see that patients who were diagnosed with second severity level all died. 



# --------- plot of chance to die only according to age --------------------
chances_to_die_by_age <- covid_data %>% group_by(age) %>% summarise(chance_to_die = mean(death_status)) %>%
  arrange(age)

ggplot(chances_to_die_by_age, aes(x = age, y = chance_to_die)) +
  geom_line(color = "blue", size = 1.2) +  # Smooth line for probability curve
  geom_point(color = "white", size = 2) +  # Points for actual data
  geom_smooth(method = "gam", color = "green", size = 1.2,alpha = 0.3, se = FALSE)+ # gam with qubic functio
  geom_vline(xintercept = mean(chances_to_die_by_age$age), 
             linetype = "dashed", color = "red", size = 1) +  # Mean age line
  labs(title = "Probability of Death by Age (COVID-19)",
       x = "Age",
       y = "Probability of Death") +
  theme_minimal(base_size = 14) + 
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white"))

#> as intermediate result we can see that most of the patients are older than 40, the chance 
#> for death rapidly going up after age 50, insufficient data for age after 100. Strange thing is that 
#> across all age groups the patients that were classified as 2 grade severity, all died. 
#> this needs deeper investigation 


# --------- logistic regression -------------------
dummy <- caret::dummyVars(data = covid_data,~age_group)
d<- predict(dummy,covid_data)
d <- as.data.frame(d)
covid_data <- cbind(covid_data,d)
covid_data <- covid_data %>%
  rename(
    Middle_Age = `age_group.Middle age`,
    Young_Old = `age_group.Young old`,
    Middle_Old = `age_group.Middle old`,
    Very_Old = `age_group.Very Old`
  ) # renaming to eliminate the use of ''
column_names <- c(column_names,covid_data %>%
                    select(age_group.Infancy:Very_Old) %>%
                    colnames())
# reference group will be age_group.Adolescence (12-18) !! 
which(column_names=="age_group.Adolescence")

column_names <- c(column_names[1:14],column_names[16:20])
formula_for_reg <- paste("death_status",paste0(column_names,collapse = "+"),sep = "~")

formula_for_reg<- as.formula(formula_for_reg)
logistic_regression <- glm(formula=formula_for_reg,data = covid_data,family = binomial)
summary(logistic_regression)

# library(car)
vif(glm(formula = as.formula(formula_for_reg), data = covid_data, family = binomial))

# according to VIF, age group 39~79 have very hight correlation, so, we can merge them for the next regression (Young_Old,Middle_Old,Middle_Age)
covid_data$age_group_combined <- ifelse(covid_data$Middle_Age ==1 | covid_data$Middle_Old==1 | covid_data$Young_Old==1,1,0)
which(column_names %in% c("Young_Old","Middle_Old","Middle_Age"))
column_names <- c(column_names[1:15],column_names[19])
column_names <- c(column_names,"age_group_combined")
covid_data$covid<- ifelse(covid_data$classiffication_final!=0,1,0)



variables <- c("diabetes", "copd", "asthma", "inmsupr", "hipertension", "other_disease",
               "cardiovascular", "obesity", "renal_chronic", "tobacco", "pneumonia",
               "age_group.Infancy", "age_group.Toddler", "age_group.Childhood",
               "age_group.Adulthood", "Very_Old", "age_group_combined")
sum(is.na(covid_data$icu))/length(covid_data$icu) *100 # wanted to include icu but 80% of data is missing
interaction_terms <- paste(variables, "covid", sep = ":")
formula_with_interactions <- paste("death_status ~ covid +", 
                                   paste(c(variables, interaction_terms), collapse = " + "))
formula_with_interactions <- as.formula(formula_with_interactions)
logistic_regression_interaction <- glm(formula=formula_with_interactions ,data = covid_data,family = binomial)
summary(logistic_regression_interaction )
# sum(covid_data$death_status == 1 & covid_data$covid == 0) was interesting how many patients died without covid19

# --------- regression visualization ---------
# Prepare data for visualization

disease_pred <- expand.grid(
  disease = c("diabetes", "copd", "asthma", "inmsupr", "hipertension", 
              "other_disease", "renal_chronic", "pneumonia"),
  covid = c(0, 1)
)

for (var in required_vars) {
  if (!(var %in% colnames(disease_pred))) {
    disease_pred[[var]] <- 0
  }
}

disease_pred$predicted_prob <- predict(logistic_regression_interaction, newdata = disease_pred, type = "response")
ggplot(disease_pred, aes(x = disease, y = predicted_prob, fill = factor(covid))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Effect of COVID on Death Probability by Disease",
       x = "Disease",
       y = "Predicted Probability of Death",
       fill = "COVID Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "red"))



# --------- investigation of classification number 2 -------------
summary(factor(covid_data$classiffication_final))
# in general, we have 1851 patients that classified with 2nd degree.
d = covid_data %>%group_by(medical_unit,classiffication_final) %>% summarize(number_of_patients = n())
d <-d %>% group_by(medical_unit) %>% mutate(percent_total_patients_in_the_unit = sum(number_of_patients))
d$percent_total_patients_in_the_unit <-d$number_of_patients / d$percent_total_patients_in_the_unit *100
d$percent_from_all_2_patients <- d$number_of_patients / sum(d[d$classiffication_final == 2,]$number_of_patients) * 100
# most of the patients classified as 2nd group ended up in 4th unit (60% from total)
# lets check the death rate in the medical units 
death_rate_med_unit <- covid_data %>% 
  group_by(medical_unit) %>% 
  summarise(death_rate = sum(death_status) / n() * 100,
            amount_of_patients_reseaved = n(),
            percent_from_all_patients = n()/1048575 * 100,
            percent_of_Infancy = sum(age_group.Infancy) / 8664 *100,
            percent_of_Toddler = sum(age_group.Toddler) / 5737 * 100,
            percent_of_Childhood = sum(age_group.Childhood) / 22072 * 100,
            percent_of_Adolescence = sum(age_group.Adolescence) / 23241 * 100,
            percent_of_Adulthood = sum(age_group.Adulthood) / 443887 * 100,
            percent_of_Middle_Age = sum(Middle_Age) / 388041 * 100,
            percent_of_Young_Old = sum(Young_Old) / 89516 * 100,
            percent_of_Middle_Old = sum(Middle_Old) / 45122 * 100,
            percent_of_Very_Old = sum(Very_Old) / 22295 * 100
  )
for (i in colnames(death_rate_med_unit)){
  print(sum(death_rate_med_unit[,i]))
}
# we can see that 4th and 12th units received the most patients 87%, as well, they combine the highest percentage of middle aged and old people 
# we can see that unit 4 
death_rate_med_unit[death_rate_med_unit$medical_unit==4,] + death_rate_med_unit[death_rate_med_unit$medical_unit==12,]
write.csv(d,file = "units summary.csv",row.names = FALSE)
getwd()
# Despite similar severe case proportions, Unit 4 patients die at a much higher rate!!!!
#--------- lets concentrate on the comparison between 4th and 12th units ------------
covid_data_12_4_units <- covid_data[covid_data$medical_unit==4 | covid_data$medical_unit == 12,]
# lets run regression on data from the units 
variables <- c("diabetes", "copd", "asthma", "inmsupr", "hipertension", "other_disease",
               "cardiovascular", "obesity", "renal_chronic", "tobacco", "pneumonia",
               "age_group.Infancy", "age_group.Toddler", "age_group.Childhood",
               "age_group.Adulthood", "Very_Old", "age_group_combined","sex")

interaction_terms <- paste(variables, "covid", sep = ":")
formula_with_interactions <- paste("death_status ~ covid +", 
                                   paste(c(variables, interaction_terms), collapse = " + "))
regression_model <- glm(family = binomial,data = covid_data_12_4_units[covid_data_12_4_units$medical_unit==4,],
                         formula = formula_with_interactions) # reg for 4th unit 
summary(regression_model)
regression_model_12 <- glm(family = binomial,data = covid_data_12_4_units[covid_data_12_4_units$medical_unit==12,],
                        formula = formula_with_interactions)
summary(regression_model_12)
"""
Unit 4 has a much higher baseline risk, even when controlling for factors
COVID had a strong impact in Unit 4 but not in Unit 12.
Pneumonia increased risk much more in Unit 12, suggesting a different case mix or treatment approach.
Asthma and COPD were much more dangerous in Unit 4.
COVID & Renal Disease was much riskier in Unit 12, which aligns with pneumonia’s stronger effect there.
Surprisingly, immunosuppression seemed protective in Unit 4—this could be due to treatment protocols, sample bias, or data recording issues.
--------------
This suggests either a more vulnerable patient population or worse COVID management in Unit 4.
Very old patients had much higher mortality in Unit 4 than in Unit 12.
Asthma was much deadlier in Unit 4. Were patients in Unit 4 given inappropriate treatments?/Did Unit 12 have better oxygen management?
Pneumonia had an extreme effect in Unit 12, but only moderate in Unit 4.
-------------
Were treatment protocols significantly different between the two units?
Did Unit 12 have better access to critical care (e.g., ventilators, ECMO, steroids)?
Were severe COVID cases misclassified in Unit 4?
Did Unit 4 have a resource shortage that led to worse outcomes?
"""

# -------- lets try and create importance rate for classification prediction -----
#only in the case that the classes were assigned not only by covid level


set.seed(313839375)
train_data <- covid_data %>%
  #filter(classiffication_final > 0) %>%  
  select(-c("pregnant", "sex", "medical_unit", "icu", "intubed", "age_group")) %>%
  sample_frac(0.7)


test_data <- anti_join(covid_data, train_data) %>%
  #filter(classiffication_final > 0) %>%  
  select(-c("pregnant", "sex", "medical_unit", "icu", "intubed", "age_group"))


train_data <- na.omit(train_data)
test_data <- na.omit(test_data)
# train_data$classiffication_final <- as.numeric(train_data$classiffication_final) - 1
# test_data$classiffication_final <- as.numeric(test_data$classiffication_final) - 1

train_labels <- train_data$death_status
train_matrix <- train_data %>%
  select(-death_status) %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = train_labels)


test_labels <- test_data$death_status  
test_matrix <- test_data %>%
  select(-death_status) %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = test_labels)


Parameters <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.05,
  max_depth = 9,
  subsample = 0.65,
  colsample_bytree = 0.6
  #num_class = 2
)


watchlist <- list(train = train_matrix, test = test_matrix)


model_xgb <- xgb.train(params = Parameters,
                       data = train_matrix,
                       nrounds = 200,
                       watchlist = watchlist,
                       verbose = 1,                    # Console output level
                       early_stopping_rounds = 30,     # Stop if no improvement for 10 rounds
                       gamma = 5)                      # Min loss reduction for a split
                       #missing = NA)
                      

min(model_xgb$evaluation_log$test_auc)

plot(model_xgb$evaluation_log$iter, model_xgb$evaluation_log$train_merror, 
     type = "l", col = "red", lwd = 2, ylim = range(c(model_xgb$evaluation_log$train_merror, 
                                                      model_xgb$evaluation_log$test_merror)), xlab = "Iteration", ylab = "Error Rate", main = "XGBoost Training vs Test Error")
lines(model_xgb$evaluation_log$iter, model_xgb$evaluation_log$test_merror, col = "blue", lwd = 2)
legend("topright", legend = c("Train Error", "Test Error"), col = c("red", "blue"), lwd = 2)

importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = model_xgb)
ImpData <- as.data.frame(importance_matrix)
factors_plot <- ggplot(ImpData, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_segment(aes(x = Feature, xend = Feature, y = 0, yend = Gain), color = "skyblue") +
  geom_point(aes(size = Frequency), color = "blue", alpha = 0.6) +
  theme_gray() +
  coord_flip() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 15),
    title = element_text(size = 17)
  ) +
  labs(title = "Variable Importance from XGBoost",
       x = "Features",
       y = "Gain (Importance)")

print(factors_plot)
# age patient_type and pneumonia contributed most for severity classification
# but its not relevant if the classification is done only by covid severity 
# if we predict the death, so the model is predicting with 71% acuracy 
# most important features are patient_type, pneumonia, classification of the covid. 

# forcing interaction term : 
variables <- c("diabetes", "copd", "asthma", "inmsupr", "hipertension", 
               "other_disease", "cardiovascular", "obesity", "renal_chronic", 
               "tobacco", "pneumonia")
for (var in variables) {
  covid_data[[paste0(var, "_covid")]] <- ifelse(covid_data[[var]] == 1 & covid_data$covid == 1, 1, 0)
}

# for interaction model, pneumonia and combination of pneumonia and covid playing megor role as well as age, and patient_type.

