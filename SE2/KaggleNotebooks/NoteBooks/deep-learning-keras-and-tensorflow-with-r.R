library(readxl)
library(tidyverse)
library(keras)
library(tensorflow)
library(dummies)
library(gmodels)

df <- read_excel("../input/bank-telemarketing-1/bank-additional1.xlsx")
glimpse(df)

### This data has 21 features and 41188 examples. One of the features is the target
###  feature (y) which we attempt to predict.
### First lets rename the feature and the look at the subscription rate.

df <- df %>%
  rename(Subscription = y)

df %>%
  ggplot(aes(x = Subscription, fill = Subscription)) +
  geom_bar() +
  ggtitle("Rate of Subscription")

round(df %>%
        select(Subscription) %>%
        table() %>%
        prop.table() * 100, 0)

### The rate of subscription is really low. Only 11 of 100 customers subscribe to
### term deposits.This could be improved if: 
### the bank knew the customers with a high propensity to subscribe and focus their
### marketing efforts on those. 
### But who's likely to subscribe? 
### We have data on customer specific attributes such as age, occupation, marital status, 
### education, credit default status, home and personal loan status.
### We also have data related with the last contact of the current campaign
### such as the number and duration of contact, the time of contact(the month and day
### of the week) and contact communication type. 
### The data is enriched by the addition of five new social and economic 
### features/attributes.
### Other features include: number of contacts performed during this campaign,
### number of days that passed by after the client was last contacted from a
### previous campaign, number of contacts performed before this campaign and 
### for this client and outcome of the previous marketing campaign.

### However, the model I build hear will utilise only customer specific attributes.
### That is age, occupation, marital status, education, credit default status, 
### home and personal loan status. The reason being I want to develop a minimalist
### model that assumes limitations on data availability. Note that this can be a 
### Robust model as you will see.
### Finally I'll input these seven features into an MLP neural network.
### This algorithm will output a binary output that denotes "yes" or "no" 
### reflecting the predicted outcomes the direct marketing campaign.
### Finally we use a simple hold out cross validation. Lets begin.

### DATA QUALITY AND DATA CLEANSING

### Missing observations
### The data does not have any missing observations. However, there are categorical 
### features where the information is denoted as either unknown/other. 
### We will deal with these as we come across them.

### lets evaluate the distribution of each of our features.

summary(df$age)

df %>%
  ggplot(aes(y = age)) +
  geom_boxplot(fill = "blue") +
  ggtitle("A Boxplot of Age") +
  theme_light()

df %>%
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue")+
  ggtitle("The Distribution of Age") +
  theme_light()

### There are customers who are as old as 98. The youngest is only 17. Most customers
##  are aged between 32 and 47. Customers with age in the range 3 sd away from
### the mean are indicated as dots in the box plot however we will not be 
### interprating these as outliers there could be some important information there.

unique(df$job)

df %>%
  filter(job == "unknown") %>%
   nrow()

### We have 8 classes for occupation. In 330 observations, the class in unknown. 
### We can be rid of these.

df_clean <- df %>%
  filter(job != "unknown")

df_clean %>%
  ggplot(aes(x = job)) +
  geom_bar(fill = "blue") +
  ggtitle("The Distribution of Occupation") +
  coord_flip() +
  theme_light() + ylab("Occupation")

### Most of the bank's customers are either in admin and blue colla occupations.
### The least are unemployed and some are housemaids and entrepreneurs.

unique(df_clean$marital)

df_clean %>%
  filter(marital == "unknown") %>%
  nrow()

### We have 4 classes for marital status. In 71 observations, the class is unknown. 
### We can be rid of these.

df_clean <- df_clean %>%
  filter(marital != "unknown")

df_clean %>%
  ggplot(aes(x = marital)) +
  geom_bar(fill = "blue") +
  ggtitle("The Distribution of marital Status") +
  theme_light()

### Most of the bank's customers are married. Only a few of them are divorced.
### The remaining are single.

unique(df_clean$education)

df_clean %>%
  filter(education == "unknown") %>%
  nrow()
### We have 8 classes for education level. In 1596 observations, the class is unknown. 
### We can be rid of these.

df_clean <- df_clean %>%
  filter(education != "unknown")

df_clean %>%
  ggplot(aes(x = education)) +
  geom_bar(fill = "blue") +
  ggtitle("The Distribution of education level") +
  coord_flip() +
  theme_light()

### Most of the bank's customers hold University degrees. Only a small
### proportion are illiterate (nearly none)

unique(df_clean$default)

df_clean %>%
  filter(default == "unknown") %>%
  nrow()

### default is a YES/NO logical feature. In 7964 observations, the class is unknown. 
### This is a lot of samples to throw away. Does education have any prediction power?. 
### If not, we may just throw away the feature itself.

df_clean %>%
  filter(default == "unknown")

df_clean %>%
  filter(default == "yes") %>%
  select(default, Subscription)

df_clean %>%
  ggplot(aes(x = default, fill = Subscription)) +
  geom_bar() +
  ggtitle("Credit default status & rate of Subscription") +
  theme_light()

### It is clear that customers with credit in default subscribe at a lower rate
### customers with no credit in default.

df_clean %>%
  group_by(default) %>%
  summarise(rate = (sum(Subscription == "yes")/
                      sum(Subscription == "yes" | Subscription == "no"))) %>%
  ggplot(aes(x = default, y = rate)) +
    geom_col(fill = "blue") +
  ggtitle("Rate of Subscription by Credit default status") +
  theme_light() + xlab("Status")

### The rate of subscription for people with credit in default is zero. There is likely
### to be some predictive power in this feature. But the model I want to train benefits 
### a lot from more training samples than from more training features.
### I'll just be rid of this feature.

df_clean <- df_clean %>%
  select(-default)

######################################
unique(df_clean$housing)

df_clean %>%
  filter(housing == "unknown") %>%
  nrow()

### Housing is also a YES/NO logical feature with 946 unknown observations.
### We can be rid of these.

df_clean <- df_clean %>%
  filter(housing != "unknown")


df_clean %>%
  ggplot(aes(x = housing)) +
  geom_bar(fill = "blue") +
  ggtitle("Status of housing loan") +
  xlab("Mortgage") +
  theme_light()

### The proportion of the bank's customers that have aMortgage loan is
### little higher than those that do not.

unique(df_clean$loan)

df_clean %>%
  ggplot(aes(x = loan)) +
  geom_bar(fill = "blue") +
  ggtitle("Status of personal loan") +
  xlab("Personal loan") +
  theme_light()

### Fewer customers have a personal loan.  

### Looks like our data is clean and tidy. We did remove a single feature,
### and our number of samples have decreased from 41188 to 38245. Still a 
### reasonably large number of examples.

### Lets proceed on to EDA.

### EXPLORATORY DATA ANALYSIS.
# AGE vs Subscription

glimpse(df_clean$age)

df_clean %>%
  ggplot(aes(x = age, fill = Subscription)) +
  geom_histogram(binwidth = 5) +
  theme_light()

### Age as a continuos variable is less informative. I want to model age as a factor with
### 5 levels.

df_clean <- df_clean %>%
  mutate(Age_group = cut(df_clean$age, 
                         breaks = c(0, 20, 40, 60, 80, 100),
                         labels = c("0-20", "20-40", "40-60", 
                                    "60-80", "80-100"))) %>%
  select(age, Age_group, everything())

### Summarise Subscrition rate per age group.

df_clean %>%
  group_by(Age_group) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                      sum(Subscription == "no" | Subscription == "yes"))
            *100, 0)
  ) %>%
  ggplot(aes(Age_group, rate, fill = Age_group)) +
  geom_col() +
  ggtitle("Rate of Subscription by Age group") +
  ylab("Rate (%)") + xlab("Age") +
  geom_text(aes(label = rate), vjust = 1, size = 3) +
  theme_light()

### Clearly, the bank should target the group under 20 years and those over 60.
### Lets look at occupation subscription.

df_clean <- df_clean %>%
  rename(Occupation = job)

df_clean %>%
  group_by(Occupation) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                          sum(Subscription == "no" | Subscription == 'yes'))
                          *100, 0)) %>% 
  ggplot(aes(Occupation, rate, fill = Occupation)) +
  geom_col() +
  ggtitle("Rate of Subscription by Occupation") +
  ylab("Rate (%)") + xlab("Occupation") +
  geom_text(aes(label = rate), vjust = 0.5, hjust = 2, size = 3) +
  coord_flip() +
  theme_light() 

### Target students and the retired. This plot seems to support the relation we
### found between age and subscription. Students are generally the younger population
### while the retired are the older citizens. 
### Lets look at subscription by both Age and Occupation.

df_clean %>%
  group_by(Occupation, Age_group) %>%
  summarise(rate = (sum(Subscription == "yes")/
                    sum(Subscription == "no" | Subscription == "yes"))
                    *100) %>%
  ggplot(aes(Occupation, rate)) +
  geom_col(fill = "blue") +
  ggtitle("Rate of Subscription by Occupation & Age") +
  ylab("Rate (%)") + xlab("Occupation") +
  facet_wrap(~Age_group) +
  coord_flip() +
  theme_light()
### It is clear that in the age group (0-20) only technicians and sudents subscribe
### to term deposits. The age group 20- 60 generally consists of non subscribers
### although students seem to be the main subscribers in the lower end of the group.
### In the 60-80 group the main subscribers are entreprenuers, management, admin and retirees.
### Subscribers in the 80 - 100 group consist only of retirees and old age housemaids.
### Note that people is services industries generally dont subscribe, it deosnt matter 
### their age group. Even the unemployed customers tend to subscribe at a higher 
### rate than them.

#### Now lets look at Marital status.
df_clean %>%
  ggplot(aes(x = marital, fill = Subscription)) +
  geom_bar()

round(df_clean$marital %>%
  table() %>%
  prop.table() * 100, 0)

### The distribution is skewed to those who are married.
### I'll create another feature on that distinguishes between married and non married
### customers. Maybe this can help reduce the skewness in the distribution
### and hopefull a clearer pattern will emerge.

df_clean$married <- ifelse(df_clean$marital == "married", "yes", "no")

df_clean <- df_clean %>%
  select(age, Age_group, Occupation, marital, married, everything())

df_clean %>%
  ggplot(aes(x = married, fill = Subscription)) +
  geom_bar()

### Lets summarise Subscription per group.

df_clean %>%
  group_by(married) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                    sum(Subscription == "no" | Subscription == "yes"))
                    *100, 0)) %>%
  ggplot(aes(married, rate, fill = married)) +
  geom_col() +
  ggtitle("Rate of Subscription by marital status") +
  ylab("Rate (%)") + xlab("Married") +
  geom_text(aes(label = rate), vjust = 0.5, size = 5) +
  theme_light()

### Looks like non married people, be it divorced or single, generally subscribe
### at a slightly higher rate than their married counterparts.
### Marital status & Age vs Subscription.

df_clean %>%
  group_by(married, Age_group) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                          sum(Subscription == "no" | Subscription == "yes"))
                         *100, 0)) %>%
  ggplot(aes(Age_group, rate, fill = Age_group)) +
  geom_col() +
  ggtitle("Rate of Subscription by Marital status & Age") +
  ylab("Rate (%)") + xlab("Age") +
  facet_wrap(~married) +
  geom_text(aes(label = rate), vjust = 1, size = 3) +
  theme_light()

### Subscription still occurs among the young and old. Also note that for the middle aged
### non subscribers marital status plays a slight role in determining whether or not 
### they subscribe. While for the married middle aged 20-60s there is no difference between 
### the lower and the higher end, both 9%, we see that if they happen to be non maried
### the lower end 20-40s tend to have a higher propensity to subscribe, 13% vs 9%.
### The subscribers here could be the proportion of students in that group ofcoarse.

### Marital status, Occupation and Age vs Subscription.

df_clean %>%
  group_by(married, Occupation, Age_group) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                    sum(Subscription == "no" | Subscription == "yes"))
                    *100, 0)) %>%
  ggplot(aes(Occupation, rate, fill = married)) +
  geom_col() +
  ggtitle("Rate of Subscription by Marital status,Occupation & Age") +
  ylab("Rate (%)") + xlab("Occupation") +
  facet_wrap(married ~ Age_group, nrow = 2) +
  geom_text(aes(label = rate), vjust = 0.5, hjust = 1, size = 3) +
  coord_flip() +
  theme_light()

### First we see that students dominate the non married 0-20 & the 20-40 age groups.
### This isnt surprising. Remember in the previous plot we hypothesied that it could be
### students who are responsible for the 13% vs 9% difference in subscription rates
### between non married 0_20 & 20-40 age groups. Also note that there are little or no 0-20 
### year olds, which explains the silence in the bottom left grid. There's only a 
### small number, who are in blue collar professions and they are all non subscribers.

### The non married 60-80 yr old group is dominated by technicians. Their married counterparts
### are dominated by entrepreneurs, people in Admin, management and retirees. The non married 80-100
### are dominated mosltly by retirees and then housemaids. The opposite is tru of their married counter-
### parts.

#Housing loan

df_clean <- df_clean %>%
  rename(Mortgage = housing)


df_clean %>%
  ggplot(aes(x = Mortgage, fill = Subscription)) +
  geom_bar()

### Mortgage & Marital status vs Subscription
df_clean %>%
  group_by(married, Mortgage) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                          sum(Subscription == "no" | Subscription == "yes"))
                          *100, 0)) %>%
  ggplot(aes(Mortgage, rate, fill = Mortgage)) +
  geom_col() +
  ggtitle("Rate of Subscription by Marital & Mortgage status") +
  ylab("Rate (%)") + xlab("Mortgage") +
  geom_text(aes(label = rate), vjust = 1, size = 4) +
  facet_wrap(~married) +
  theme_light()

### Among married customers, whether or not they have a mortgage their propensity 
### to subscribe to term deposits is the same. The only clear pattern here is the one 
### of Marital status, of which we already identified.



df_clean %>%
  group_by(Mortgage, Age_group, Occupation) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                          sum(Subscription == "no" | Subscription == "yes"))
                          *100, 0)) %>%
  ggplot(aes(Mortgage, rate, fill = Occupation)) +
  geom_col() +
  ggtitle("Rate of Subscription by Mortgage status & Age") +
  ylab("Rate (%)") + xlab("Mortgage") +
  facet_wrap(~Occupation) +
  theme_light()

### Lets take two example grids to analyse this fig. Take the entreprenuer grid 
### 2nd top right corner and the student grin bottom left corner. It seems like 
### for an entreprenuer, whether they subscribe to a term deposit does depend
### on whether or not they have a mortgage. For the student however, the rate of
### subscription is the same whether the have the mortgage or not.
### Lets move on.

df_clean %>%
  group_by(education) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                          sum(Subscription == "no" | Subscription == "yes"))
                          *100, 0)) %>%
  ggplot(aes(education, rate, fill = education)) +
  geom_col() +
  ggtitle("Rate of Subscription by Education level") +
  ylab("Rate (%)") + xlab("Education") +
  geom_text(aes(label = rate), hjust = 2, size = 4) +
  coord_flip() +
  theme_light()

df_clean %>%
  filter(education == "illiterate") %>%
  nrow()

### We won't go on that rate for illiterate customers. The reason is: there's only 
### 18 illiterate customers. I suspect the sample may be too small to make such
### inference. It is clear however that the illterate and those with university degrees
### have higher propensities to subscribe.
### Personal loan

df_clean %>%
  ggplot(aes(x = loan, fill = Subscription)) +
  geom_bar()

df_clean %>%
  group_by(loan) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                          sum(Subscription == "no" | Subscription == "yes"))
                          *100, 0)) %>%
  ggplot(aes(loan, rate, fill = loan)) +
  geom_col() +
  ggtitle("Rate of Subscription by Personal loan status") +
  ylab("Rate (%)") + xlab("Loan status")+
  geom_text(aes(label = rate), vjust = 1, size = 4) +
  theme_light()

### Individuals with personal loans tend to subscribe at the same rate as those
### without personal loans.
### Personal loan & mortgage vs Subscription.

df_clean %>%
  group_by(Mortgage, loan) %>%
  summarise(rate = round((sum(Subscription == "yes")/
                          sum(Subscription == "no" | Subscription == "yes"))
                          *100, 0)) %>%
  ggplot(aes(loan, rate, fill = loan)) +
  geom_col(position = "dodge") +
  ggtitle("Rate of Subscription by Mortgage & loan status") +
  ylab("Rate (%)") + xlab("Loan") +
  facet_wrap(~Mortgage) +
  theme_light()

### Indiviaduals with mortgages and no personal loans subscribe at slightly higher
### rates than individuals with both mortgage and personal loans. Also, individuals with
### no mortgage subscribe at the same rates irrespective of whether they have personal
### loans or not.

df_clean %>%
  group_by(loan, Occupation) %>%
  summarise(rate = (sum(Subscription == "yes")/
                      sum(Subscription == "no"))
            *100,
            n = n()
  ) %>%
  ggplot(aes(loan, rate, fill = loan)) +
  geom_col() +
  ggtitle("Rate of Subscription by Personal loan status & Occupation") +
  ylab("Rate (%)") + xlab("Personal loan status") +
  facet_wrap(~Occupation)

### Looking at the bottom left corner grid. Students tend to subscribe more to
### term deposits when they have personal loans. As for the other occupations, only
### so much can be said. 

###  Now that we've answered some of the questions about who the bank marketing
### campaign should target, shall we proceed onto building a neural network model 
###  for the purpose of classification.

##Data preposessing

data <- df_clean %>%
  select(Age_group, Occupation, married, education, Mortgage, loan, Subscription)
  
### Create dummies for Age group, Occupation and education.

data <- as_tibble(cbind(data, dummy(data$Age_group) %>%
                      cbind(dummy(data$Occupation)) %>%
                          cbind(dummy(data$education))
                 ) %>%
                   select(-Age_group, -Occupation, -education)
              )
  
### Encode married, mortgage, loan and Subscription.

data$married <- ifelse(data$married == "yes", 1, 0)
data$Mortgage <- ifelse(data$Mortgage == "yes", 1, 0)
data$loan <- ifelse(data$loan == "yes", 1, 0)
data$Subscription<- ifelse(data$Subscription == "yes", 1, 0)
#######################################################
### Make the data a matrix

data <- as.matrix(data)
dimnames(data) <- NULL

### Split data into training and testing set

set.seed(1234)
ind <- sample(nrow(data), 0.70 * nrow(data))

train_x <- data[ind,1:26]
train_y <- data[ind,27]

test_x <- data[-ind, 1:26]
test_y <- data[-ind, 27]

###
summary(train_x)

### Build the model.

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = 26) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

### Compile the model

model %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )

### Fit the model

history <- model %>%
  fit(
    train_x,
    train_y,
    epochs = 14,
    batch_size = 128,
    validation_split = 0.2
  )
plot(history)
### The model trained really quick. from the first to the second iteration
### its accuracy increased to nearly 100%. both on the training set and the 
### validation set. And the loss function was nearly at zero, correspondingly.
### Lets just evaluate the model.

model %>%
  evaluate(test_x, test_y)

### This is pretty good for a simple and quick model we just built.
### Get the probabilities on test data

prob <-model %>%
  predict_proba(test_x)

### Simple hold out cross validation
### Get the predictions

pred <- model %>%
  predict_classes(test_x)

### Tabulate
table(Predicted = pred, Actual = test_y)
### OR
CrossTable(pred, test_y)

### 100% accuracy on data the model hasn't seen before.
### It is unbelievable.
###################################################
### :):):):):):):):):):)





