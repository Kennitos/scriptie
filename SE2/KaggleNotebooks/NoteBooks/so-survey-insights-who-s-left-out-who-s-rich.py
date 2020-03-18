---
title: "Inclusiveness-Focused Gender-Based Stack Overflow Developer Survey Analysis in R"
output: 
    html_document:
        toc: yes
        theme: cosmo
        highlight: tango
        code_folding: hide
---

```{r}
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

```


# Philosophy 

The philosophy of this analysis is to uncover certain not-so-obvious insights reg. **Inclusiveness, Community Companionship and Gender** of Stack Overflow Members and Developers in General. 

**Focus Areas:**

* Salary and Salary Gap

* Community Inclusiveness

* Companionship

* Gender-focused Dimensions Analysis


# About the dataset

This is the response data of Stack Overflow 2018 Developer Survey.

Number of Qualified Respondents: 98,855
Number of Completed Respondents: 67,441

More details [here](https://www.kaggle.com/stackoverflow/stack-overflow-2018-developer-survey)

# Highlights

* Stack Overflow (overall) boasts an **NPS of 86** that's off the chart in the world of Net Promoter Scores - to make some sense, Amazon's NPS according to npsbenchmarks.com is 69. 

* **Female Developers** are insanely less with just **7%** of the entire respondents being female against 92% Male Developers. 
 
* **58% of Computer Science/Engineering and Health Science** Undergraduate Majors consider themselves to be part of Stack Overflow but only **44% of Fine Arts and 45% of Humanities & Social Science** Undergrad Majors consider to be part of Stack Overflow.

* **Mobile Developers** and **Students** are on the opposite spectrum of Feeling inclusive in Stack Overflow with only less than 50% of students feeling inclusive. 

* Developers of **Python, Scala, Perl, Bash/Shell** are in the middle of this spectrum of Feeling Inclusive and Participating Frequently. 

* On an index of No_to_Yes Ratio for feeling Inclusive, **Finland, Norway, New Zealand, Canada and UK** lead the chart. 

* **~19K developers** either disagree or neigher disagree nor agree, which is a significant number that there’s still a decent room to improve the community companionship.

* **35% of developers** Agree thinking that they are competing while **38% disagree**.

* Overall, The effect of **imposter syndrome** seems to be minimal among the respondents with **38K respondents** either disagreeing or strongly disagreeing it. 

* Imposter Syndrome (Feeling of not good at programming) looks like that this effect is more prevelant among Female Develpers than their male counterparts comparatively.

* Female Developers in **Product manager, Full-stack developer, System administrator** domains are getting almost equal salary compared to Male.

*  The first mover advantage for Male Respondents could be seen here. **~20% of Male in the age bracket of 18-24** are **independent contractors / freelancer/self-employed** while It’s **~16% of Female** in that range.

*  Among Indie Contractors/Freelancers, It is **Female with Bachelor’s 43% and Master’s 26% lead** their group than their equivalent **Male counterpart whose Bachelor’s is 38% and Master’s is 23%. This could be possibly because of the fact that college-educated female develpers tend to take a freelancer role or a contractual role due the **flexibility **it provides.


# Potential Actionables 

* Starting a Community / Private Room for New Developers - focused on improving Gender Diversity
* Improving Flexibility of communities (Q & A) where Feeling Inclusive is less - like among **Rust, Haskell, Lua and Go**, learning from **Objecive-C, Swift, VB 6, VB.Net, Kotlin**.
* Identifying the types of questions / participation Developers with no formal degree - **School & Univeristy study without a degree** ask and educating experienced / educated Community members to tolerate such instances / questions. 
* Improving Communities / Languages associated with **Humanities and Social Science** Domains to help them feel more inclusive and participate frequently. 
* Designing Gamification strategies to **reduce the competition mindset** among Developers who feel they're competing among their peers.
* Helping New comers and Female Developers overcome **Imposter Syndrome of not feeling good at programming** by either starting a new **Podcast** hosting similar background individuals excelling in coding. 
* In Survey Design (for 2019), Adding a **Free Text Response Box ** associated with NPS question and Inclusiveness question could help us get more clarity on the reasons and improve the community better. 



**Loading required Libraries**

```{r}
library(tidyverse)
library(highcharter)
library(ggrepel)
library(scales)
library(plotly)
library(cowplot)
library(knitr)
library(ggalluvial)

```

# Reading Input dataset 

```{r}
survey18 <- read_csv("../input/survey_results_public.csv")
```


# Who's part of the survey?

## Countries

### Most Represented Countries

```{r}
survey18 %>% 
  filter(!is.na(Country)) %>% 
  group_by(Country) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  #head(10) %>% 
  hchart('treemap',hcaes(x = Country, value = n, color = n)) %>% 
  hc_title(text = 'Countries from where overall respondents come from') 
  
```

## Female to Male Ratio

### Female-to-Male Ratio - Better Countries

Imagine, we live in a world where for Countries with more than 200 developers (from the respondents), **0.16** is the best Female to Male Ratio.

```{r}

survey18 %>%  group_by(Country) %>% mutate(count = n()) %>% 
  filter(count > 200) %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country,Gender) %>% count() %>% 
  spread(Gender, n) %>% mutate(F2M = Female/Male) %>% arrange(desc(F2M)) %>% 
  head(30) %>% 
  hchart('column', hcaes(x = 'Country', y = 'F2M')) %>% 
  hc_title(text = 'Top 30 Countries ordered with better Female-to-Male Ratio') %>%
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_xAxis(title = list(text = "Countries"))

```

### F2M Ratio - Worse Countries

```{r}

survey18 %>%  group_by(Country) %>% mutate(count = n()) %>% 
  filter(count > 200) %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country,Gender) %>% count() %>% 
  spread(Gender, n) %>% mutate(F2M = Female/Male) %>% arrange(F2M) %>% 
  head(30) %>% 
  hchart('column', hcaes(x = 'Country', y = 'F2M')) %>% 
  hc_title(text = 'Top 30 Countries ordered with worse Female-to-Male Ratio') %>%
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_xAxis(title = list(text = "Countries"))

```


## Age

### Male vs Female by Age

```{r}
survey18 %>% 
 filter(!is.na(Gender)) %>%
  filter(!is.na(Age)) %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  select(Gender,  Age) %>%
  mutate(Gender = str_split(Gender, pattern = ";")) %>%
  unnest(Gender) %>%
  group_by(Gender, Age) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Gender = reorder(Gender,Count)) %>%
  hchart('column',hcaes('Gender','Count', group = 'Age')) %>% 
  #hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Male vs Female by Age - Stack Overflow Members') %>% 
  hc_yAxis(type = 'logarithmic')
```

### Age by Male vs Female 

```{r}
survey18 %>% 
 filter(!is.na(Gender)) %>%
  filter(!is.na(Age)) %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  select(Gender,  Age) %>%
  mutate(Gender = str_split(Gender, pattern = ";")) %>%
  unnest(Gender) %>%
  group_by(Gender, Age) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Gender = reorder(Gender,Count)) %>%
  hchart('column',hcaes('Age','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Age by Male vs Female - Stack Overflow Members') %>% 
  hc_yAxis(type = 'logarithmic')
```


## Gender


With no doubt, the respondents are primarily Male with 92% and Female being just 7%

```{r}
survey18 %>% 
 filter(!is.na(Gender)) %>%
  select(Gender) %>%
  mutate(Gender = str_split(Gender, pattern = ";")) %>%
  unnest(Gender) %>%
  group_by(Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% mutate(perc = round((Count / sum(Count)) * 100)) %>% arrange(desc(perc)) %>% kable()
```


```{r}
survey18 %>% 
 filter(!is.na(Gender)) %>%
  select(Gender) %>%
  mutate(Gender = str_split(Gender, pattern = ";")) %>%
  unnest(Gender) %>%
  group_by(Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Gender = reorder(Gender,Count)) %>%
  hchart('waterfall',hcaes('Gender','Count')) %>% 
    hc_colors('darkorange') %>% 

  hc_title(text = 'Gender - Stack Overflow Members')
  
```


### Sexual Orientation

While the majority of 55K users identified themselves as Straight or heterosexual, there's still around 4K that has identified otherwise which also implies that SO members are opening themselves up.
```{r}
survey18 %>% 
 filter(!is.na(SexualOrientation)) %>%
  select(SexualOrientation) %>%
  mutate(SexualOrientation = str_split(SexualOrientation, pattern = ";")) %>%
  unnest(SexualOrientation) %>%
  group_by(SexualOrientation) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(SexualOrientation = reorder(SexualOrientation,Count)) %>%
  hchart('waterfall',hcaes('SexualOrientation','Count')) %>% 
  hc_colors('darkorange') %>% 
  hc_title(text = 'Sexual Orientation - Stack Overflow Members')  
```

## Students

### Student vs Others

```{r fig.width= 14, fig.height=7}

survey18 %>% 
  group_by(Student) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(Student,n), stat = 'identity', fill = "#FFB935") +
  labs(
    title = 'Student vs Others'
  ) -> s1


survey18 %>% 
  group_by(Employment) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(Employment,n), stat = 'identity', fill = "#FFB935") +
  labs(
    title = 'Type of Employement'
  ) +
  coord_flip() -> s2

plot_grid(s2,
          s1,
          #labels = c('Student vs Others','Type of Employement'),
          label_x = 0.2,
          ncol = 2,
          rel_widths = c(1.5,1))
```

As unexpected, Non-Students form a major part of the survey respondents. 

```{r}
survey18 %>% 
  group_by(Student) %>% 
  count() 
```
### Employment Type

While Full-Time Employees form the major chunk, it's also amazing to see **Retired** developers on Stack overflow. 

```{r}
survey18 %>% 
  group_by(Employment) %>% 
  count() %>%
  kable()
```


# Let's talk $Salary

One of the most important reasons why a large majority of us work is to earn money but we hardly get to know what's the salary of someone sitting beside us and most of the time we believe what we make is not what we deserve.

Thus, this plot can tell us something important - **Where we stand**.

Only full time employees have been considered while making the below plot to keep the analysis base standard. 

## Annual Salary (USD) 

### Overall Distribution

```{r fig.width= 9, fig.height=6}
options(scipen=999)
survey18 %>% filter(Employment %in% 'Employed full-time') %>% ggplot() +
  geom_histogram(aes(ConvertedSalary),fill = "#FFB935")  +
  scale_x_log10() +
  labs(x = "Log of Annual Salary in USD", 
       y = "Frequency / Count",
       title = "with Log") -> sal_log

survey18 %>% filter(Employment %in% 'Employed full-time') %>% ggplot() +
  geom_histogram(aes(ConvertedSalary),fill = "#FFB935")  +
  #scale_x_log10() +
  labs(x = "Annual Salary in USD", 
       y = "Frequency / Count",
       title = "Annual Salary in USD - Distribution") -> sal_nolog



plot_grid(sal_nolog,
          sal_log,
          #labels = c('Student vs Others','Type of Employement'),
          label_x = 0.2,
          ncol = 2,
          rel_widths = c(1,1))

```

### Annual Salary (USD) Distribution - by Country with more than 500 respondents

Looks like **United States, India, China** have got the major variance - which also means the variety of jobs available and a huge supply of talent pool ready to take up any job.

And, only a very few countries cross the **1M USD** mark. While the unsurprising small players are **Israel** and **Canada**, the unexpected entrant here is **Mexico**. Probably a message to US President that building the wall may not be a good idea ;) 
```{r fig.width = 9, fig.height = 9}
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 500) %>% 
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  select(Country) %>% mutate(Country = factor(Country)) -> countries_salary


options(scipen=999)
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 500) %>% 
  #arrange(desc(ConvertedSalary)) %>% 
  ungroup(Country) %>% 
  ggplot() +
  geom_violin(aes(Country,ConvertedSalary), fill = "#FFB935")  +
  scale_x_discrete(limits = countries_salary$Country) +
  coord_flip() +
  
  scale_y_log10() + 
  labs(x = "Country", 
       y = "Log of Annual Salary in USD",
       title = "Annual Salary in USD - Distribution by Country - More than 500 respondents") -> p2

plotly::ggplotly(p2)
```


### Annual Salary (USD) Distribution - by Country - Male vs Female 


```{r fig.width = 9, fig.height = 9}



survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 500) %>% 
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  select(Country) %>% mutate(Country = factor(Country)) -> countries_salary



options(scipen=999)
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 500) %>% 
  #arrange(desc(ConvertedSalary)) %>% 
  ungroup(Country) %>% 
  ggplot() +
  geom_boxplot(aes(Country,ConvertedSalary, fill = Gender))  +
  scale_x_discrete(limits = countries_salary$Country) +
  coord_flip() +
  #facet_grid('Gender') +
  scale_y_log10() + 
  labs(x = "Country", 
       y = "Log of Annual Salary in USD",
       title = "Annual Salary in USD - Male vs Female - by Country") 


```



### Annual Salary (USD) Distribution - by Country b/w 200 & 500 respondents

```{r fig.width = 9, fig.height = 12}


survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 100 & n < 500) %>% 
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  select(Country) %>% mutate(Country = factor(Country)) -> countries_salary



options(scipen=999)
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 100 & n < 500) %>% 
  #arrange(desc(ConvertedSalary)) %>% 
  ungroup(Country) %>% 
  ggplot() +
  geom_violin(aes(Country,ConvertedSalary), fill = "#FFB935")  +
  scale_x_discrete(limits = countries_salary$Country) +
  coord_flip() +
  
  scale_y_log10() +
  labs(x = "Country", 
       y = "Log of Annual Salary in USD",
       title = "Annual Salary in USD - Distribution by Country - b/w 200 & 500 respondents") -> p2

plotly::ggplotly(p2)

```


### Annual Salary (USD) Distribution - by Country - Male vs Female 


```{r fig.width = 9, fig.height = 12}



survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 100 & n < 500) %>% 
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  select(Country) %>% mutate(Country = factor(Country)) -> countries_salary



options(scipen=999)
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 100 & n < 500) %>% 
  #arrange(desc(ConvertedSalary)) %>% 
  ungroup(Country) %>% 
  ggplot() +
  geom_boxplot(aes(Country,ConvertedSalary, fill = Gender))  +
  scale_x_discrete(limits = countries_salary$Country) +
  coord_flip() +
  #facet_grid('Gender') +
  scale_y_log10() + 
  labs(x = "Country", 
       y = "Log of Annual Salary in USD",
       title = "Annual Salary in USD - Male vs Female - by Country") 


```


### Annual Salary (USD) Difference

This very well separates the developing nations and developed nations, with **South Africa** changing the sides. 

```{r}
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 500) %>% 
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  mutate(med_sal = med_sal - median(med_sal)) %>% 
  ggplot() +
  geom_bar(aes(reorder(Country,med_sal),med_sal), stat = 'identity', fill = "#FFB935") +
  coord_flip() + 
  labs(x = 'Country',
       y = 'Median Salary difference with Median of Median Salaries of Top Countries',
       title = 'Salary Difference') 

```


```{r}
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n >100 & n < 500) %>% 
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  mutate(med_sal = med_sal - median(med_sal)) %>% 
  ggplot() +
  geom_bar(aes(reorder(Country,med_sal),med_sal), stat = 'identity', fill = "#FFB935") +
  coord_flip() + 
  labs(x = 'Country',
       y = 'Median Salary difference with Median of Median Salaries of 2nd bracket Countries',
       title = 'Salary Difference') 
```



### By Developer Type 


```{r}
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(!is.na(DevType)) %>%
  #filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  select(DevType,ConvertedSalary,Gender) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  group_by(DevType) %>%
  summarise(Median_Salary = median(ConvertedSalary,na.rm = TRUE)) %>%
  arrange(desc(Median_Salary)) %>%
  ungroup() %>%
  mutate(DevType = reorder(DevType,Median_Salary)) %>%
  head(20) %>% 
  hchart('column',hcaes('DevType','Median_Salary')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>%  
  hc_title(text = "Median Salary by Developer Type")
  
```



### By Developer Type - wrt Gender

It is quite ironic to see that the data show Female CXO executives make more than Male CXOs which is too good ot be true. 

```{r}
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(!is.na(DevType)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  select(DevType,ConvertedSalary,Gender) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  group_by(DevType,Gender) %>%
  summarise(Median_Salary = median(ConvertedSalary,na.rm = TRUE)) %>%
  arrange(desc(Median_Salary)) %>%
  ungroup() %>%
  mutate(DevType = reorder(DevType,Median_Salary)) %>%
  head(20) %>% 
  hchart('column',hcaes('DevType','Median_Salary', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>%  
  hc_title(text = "Median Salary by Developer Type wrt Gender")
  
```

So, it is clear that we are talking 23 Female Executives, such a low base number gives a false impression that Female CXOs more than Male counterparts. Surely, You can lie with Statistics ;) 

```{r}
survey18 %>% filter(Employment %in% 'Employed full-time') %>% 
  filter(!is.na(DevType)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  select(DevType,ConvertedSalary,Gender) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  group_by(DevType,Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(DevType = reorder(DevType,Count)) %>%
  #head(20) %>% 
  hchart('column',hcaes('DevType','Count',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>%  
  hc_title(text = "Developer Type Count by Gender") %>% 
  hc_yAxis(type = 'logarithmic')
  
```


# Community Inclusion

## Undergraduate Major

### Undergraduate Major - Feeling Part of SO Community

It is important to note that **Health Science** Undergrads feel almost as inclusive as **Computer Science** Undergrads.

```{r}
#code courtesy: Julia Silge
survey18 %>%
  select(UndergradMajor, StackOverflowConsiderMember) %>%
  filter(!is.na(UndergradMajor),
         !UndergradMajor %in% c("I never declared a major")) %>%
  group_by(UndergradMajor) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            n = n()) %>%
  mutate(UndergradMajor = reorder(UndergradMajor, ConsiderMember)) %>%
  ggplot(aes(UndergradMajor, ConsiderMember, label = paste0(UndergradMajor," - ",round(ConsiderMember * 100),"%"))) +
  geom_col(show.legend = FALSE, fill = "#FFB935") +
  geom_text(aes(UndergradMajor, 0.01), hjust = 0,
            color = "white", size = 4) +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  labs(x = NULL, y = "% who consider themselves part of the Stack Overflow community",
       title = "Who considers themselves part of the Stack Overflow community?",
       subtitle = "Undergraduate major")
```

### Undergrad Majors with Self-reported Participation

While **web developers** have less participation and most inclusive, **Humanities discipline** is in the opposite spectrum.

```{r fig.width= 10, fig.height=9}
survey18 %>%
  select(UndergradMajor, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(UndergradMajor = str_split(UndergradMajor, pattern = ";")) %>%
  unnest(UndergradMajor) %>%
  filter(!is.na(UndergradMajor)) %>%
  
  group_by(UndergradMajor) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            Participation = mean(StackOverflowParticipate %in% c("Multiple times per day",
                                                                 "Daily or almost daily",
                                                                 "A few times per week",
                                                                 "A few times per month or weekly"),
                                 na.rm = TRUE),
            n = n()) %>%
  filter(n > 1500) %>%
  ggplot(aes(Participation, ConsiderMember, label = UndergradMajor)) +
  geom_smooth(method = "lm") +
  geom_text_repel(size = 3, point.padding = 0.25) +
  geom_point(aes(size = n), alpha = 1,colour = "#FFB935") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = percent_format()) +
  scale_size_continuous(labels = comma_format()) +
  labs(x = "% who participate at least weekly", 
       y = "% who consider themselves part of the Stack Overflow community",
       title = "Undergrad Majors Who considers themselves part of the SO community?",
       subtitle = "Self-reported participation frequency and feeling of belonging",
       size = "Number of respondents")

  #Code Courtesy: Julia Silge
```
## Formal Education 

### Formal Education - Feeling Belonged

While almost more than 50% of formal degree holders believe they are part of SO, there is still some real work to be done with those **without formal degree and Schools.**

```{r}
#code courtesy: Julia Silge
survey18 %>%
  select(FormalEducation, StackOverflowConsiderMember) %>%
  filter(!is.na(FormalEducation),
         !FormalEducation %in% c("I never completed any formal education")) %>%
  group_by(FormalEducation) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            n = n()) %>%
  mutate(FormalEducation = reorder(FormalEducation, ConsiderMember)) %>%
  ggplot(aes(FormalEducation, ConsiderMember, label = paste0(FormalEducation," - ",round(ConsiderMember * 100),"%"))) +
  geom_col(show.legend = FALSE, fill = "#FFB935") +
  geom_text(aes(FormalEducation, 0.01), hjust = 0,
            color = "white", size = 4) +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  labs(x = NULL, y = "% who consider themselves part of the Stack Overflow community",
       title = "Who considers themselves part of the Stack Overflow community?",
       subtitle = "FormalEducation")
```

### Formal Education - Correlation with Weekly Participation


```{r fig.width= 10, fig.height=8}
survey18 %>%
  select(FormalEducation, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(FormalEducation = str_split(FormalEducation, pattern = ";")) %>%
  unnest(FormalEducation) %>%
  filter(!is.na(FormalEducation)) %>%
  
  group_by(FormalEducation) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            Participation = mean(StackOverflowParticipate %in% c("Multiple times per day",
                                                                 "Daily or almost daily",
                                                                 "A few times per week",
                                                                 "A few times per month or weekly"),
                                 na.rm = TRUE),
            n = n()) %>%
  filter(n > 1500) %>%
  ggplot(aes(Participation, ConsiderMember, label = FormalEducation)) +
  geom_smooth(method = "lm") +
  geom_text_repel(size = 3, point.padding = 0.25) +
  geom_point(aes(size = n), alpha = 1,colour = "#FFB935") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = percent_format()) +
  scale_size_continuous(labels = comma_format()) +
  labs(x = "% who participate at least weekly", 
       y = "% who consider themselves part of the Stack Overflow community",
       title = "Undergrad Majors Who considers themselves part of the SO community?",
       subtitle = "Self-reported participation frequency and feeling of belonging",
       size = "Number of respondents")

  #Code Courtesy: Julia Silge
```



## Developer Type

### Developer Type - Feeling part of the SO Community

```{r}

survey18 %>%
  select(DevType, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  filter(!is.na(DevType)) %>%
  mutate(DevType = case_when(str_detect(DevType, "Data scientist") ~ "Data scientist",
                             str_detect(DevType, "academic") ~ "Academic researcher",
                             TRUE ~ DevType)) %>%
  group_by(DevType) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            Participation = mean(StackOverflowParticipate %in% c("Multiple times per day",
                                                                 "Daily or almost daily",
                                                                 "A few times per week",
                                                                 "A few times per month or weekly"),
                                 na.rm = TRUE),
            n = n()) %>%
  filter(n > 1500) %>%
  mutate(DevType = reorder(DevType, ConsiderMember)) %>%

  ggplot(aes(DevType, ConsiderMember, label = DevType)) +
  geom_col(show.legend = FALSE, fill = "#FFB935") +
  geom_text(aes(DevType, 0.01), hjust = 0,
            color = "white", size = 4) +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  labs(x = NULL, y = "% who consider themselves part of SO",
       title = "Who considers themselves part of SO?",
       subtitle = "DevType") 

#code courtesy : Julia Silge
```


### Developer Type - Correlation with Weekly Participation

**Students** clearly stand out here. With less 50% considering them to be part of Stack Overflow Community they also visit SO less frequently. 


Others with a decent amount of respondents who follow the leauge are:

* System Administrator
* Desktop Enteprise Application Developer
* QA Test Developer

```{r fig.width= 10, fig.height=8}
survey18 %>%
  select(DevType, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  filter(!is.na(DevType)) %>%
  
  group_by(DevType) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            Participation = mean(StackOverflowParticipate %in% c("Multiple times per day",
                                                                 "Daily or almost daily",
                                                                 "A few times per week",
                                                                 "A few times per month or weekly"),
                                 na.rm = TRUE),
            n = n()) %>%
  filter(n > 1500) %>%
  ggplot(aes(Participation, ConsiderMember, label = DevType)) +
  geom_smooth(method = "lm") +
  geom_text_repel(size = 3, point.padding = 0.25) +
  geom_point(aes(size = n), alpha = 1,colour = "#FFB935") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = percent_format()) +
  scale_size_continuous(labels = comma_format()) +
  labs(x = "% who participate at least weekly", 
       y = "% who consider themselves part of the Stack Overflow community",
       title = "Developer Type Who considers themselves part of the SO community?",
       subtitle = "Self-reported participation frequency and feeling of belonging",
       size = "Number of respondents")

  #Code Courtesy: Julia Silge
```



## Language worked Type

### Language worked Type - Feeling Part of the Community

```{r}

survey18 %>%
  select(LanguageWorkedWith, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
  unnest(LanguageWorkedWith) %>%
  filter(!is.na(LanguageWorkedWith)) %>%
  mutate(LanguageWorkedWith = case_when(str_detect(LanguageWorkedWith, "Data scientist") ~ "Data scientist",
                             str_detect(LanguageWorkedWith, "academic") ~ "Academic researcher",
                             TRUE ~ LanguageWorkedWith)) %>%
  group_by(LanguageWorkedWith) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            Participation = mean(StackOverflowParticipate %in% c("Multiple times per day",
                                                                 "Daily or almost daily",
                                                                 "A few times per week",
                                                                 "A few times per month or weekly"),
                                 na.rm = TRUE),
            n = n()) %>%
  filter(n > 1500) %>%
  mutate(LanguageWorkedWith = reorder(LanguageWorkedWith, ConsiderMember)) %>%

  ggplot(aes(LanguageWorkedWith, ConsiderMember, label = LanguageWorkedWith)) +
  geom_col(show.legend = FALSE, fill = "#FFB935") +
  geom_text(aes(LanguageWorkedWith, 0.01), hjust = 0,
            color = "white", size = 4) +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  labs(x = NULL, y = "% who consider themselves part of SO",
       title = "Who considers themselves part of SO?",
       subtitle = "LanguageWorkedWith") 

#code courtesy : Julia Silge
```


### Language worked Type - Correlation with Weekly Participation

* Clearly, the most matured communities that feel inclusive are **Objective-c** and **Swift** 

* **Rust** community needs a serious attention to see why its users feel they aren't part of SO COmmunity

* **Go,Lua, Haskell** are also below **50% ** inclusion level even with better weekly participation. 

* Communities like **Python, Scala and Bash** have barely crossed the line but also at the same time with a huge base.

```{r fig.width= 10, fig.height=8}
survey18 %>%
  select(LanguageWorkedWith, StackOverflowConsiderMember, StackOverflowParticipate) %>%
  mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
  unnest(LanguageWorkedWith) %>%
  filter(!is.na(LanguageWorkedWith)) %>%
  
  group_by(LanguageWorkedWith) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            Participation = mean(StackOverflowParticipate %in% c("Multiple times per day",
                                                                 "Daily or almost daily",
                                                                 "A few times per week",
                                                                 "A few times per month or weekly"),
                                 na.rm = TRUE),
            n = n()) %>%
  filter(n > 1500) %>%
  ggplot(aes(Participation, ConsiderMember, label = LanguageWorkedWith)) +
  geom_smooth(method = "lm") +
  geom_text_repel(size = 3, point.padding = 0.25) +
  geom_point(aes(size = n), alpha = 1,colour = "#FFB935") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = percent_format()) +
  scale_size_continuous(labels = comma_format()) +
  labs(x = "% who participate at least weekly", 
       y = "% who consider themselves part of the Stack Overflow community",
       title = "Developer Type Who considers themselves part of the SO community?",
       subtitle = "Self-reported participation frequency and feeling of belonging",
       size = "Number of respondents")

  #Code Courtesy: Julia Silge
```


### EthicsChoice

That's slightly confusing that more than 62% of people who'd be okay to write an unethical consideres to be member of Stack Overflow. While this is also because of the small base number that it holds. 

```{r}
survey18 %>%
  select(EthicsChoice, StackOverflowConsiderMember) %>%
  filter(!is.na(EthicsChoice)) %>%
  group_by(EthicsChoice) %>% 
  count() %>% 
  kable()
```


```{r}
#code courtesy: Julia Silge
survey18 %>%
  select(EthicsChoice, StackOverflowConsiderMember) %>%
  filter(!is.na(EthicsChoice)) %>%
  group_by(EthicsChoice) %>%
  summarise(n = n()) %>%
  mutate(EthicsChoice = reorder(EthicsChoice, n)) %>%
  ggplot(aes(EthicsChoice, n, label = paste0(EthicsChoice," - ",n))) +
  geom_col(show.legend = FALSE, fill = "#FFB935") +
  geom_text(aes(EthicsChoice, 0.01), hjust = 0,
            color = "white", size = 4) +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  #scale_y_continuous(labels = percent_format(),
  #                   expand = c(0,0)) +
  labs(x = NULL, y = "Number of respondents",
       title = "Ethics Choics",
       subtitle = "Total Respondents") -> p1


survey18 %>%
  select(EthicsChoice, StackOverflowConsiderMember) %>%
  filter(!is.na(EthicsChoice)) %>%
  group_by(EthicsChoice) %>%
  summarise(ConsiderMember = mean(StackOverflowConsiderMember == "Yes", na.rm = TRUE),
            n = n()) %>%
  mutate(EthicsChoice = reorder(EthicsChoice, ConsiderMember)) %>%
  ggplot(aes(EthicsChoice, ConsiderMember, label = paste0(EthicsChoice," - ",round(ConsiderMember * 100),"%"))) +
  geom_col(show.legend = FALSE, fill = "#FFB935") +
  geom_text(aes(EthicsChoice, 0.01), hjust = 0,
            color = "white", size = 4) +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  scale_y_continuous(labels = percent_format(),
                     expand = c(0,0)) +
  labs(x = NULL, y = "% who consider themselves part of SO",
       title = "Who considers themselves part of SO?",
       subtitle = "EthicsChoice") -> p2

plot_grid(p1,
          p2,
          #labels = c('Student vs Others','Type of Employement'),
          label_x = 0.2,
          ncol = 2,
          rel_widths = c(1,1.5))

```


# Feeling Left out

## Countries 

The idea here is to have a normalized index that could represent the crowd based on Country they belong to, feeling left out (not considering to be part of Stack Overflow Member). With taking entries from the countries that had minimum 150 respondents, Top 5 countries containing respondents feeling left out are:

* Finland
* Norway
* New Zealeand
* Canada
* United Kingdom


```{r}
survey18 %>% 
  filter(!is.na(Country),
         !is.na(StackOverflowConsiderMember), 
         StackOverflowConsiderMember %in% c("Yes","No" )) %>% 
  group_by(Country) %>% 
  mutate(n = n())  %>% 
  filter(n > 150) %>% 
  ungroup() %>% 
  select(Country,StackOverflowConsiderMember) %>% 
  group_by(Country,StackOverflowConsiderMember) %>% 
  count()  %>% 
  #filter(n > 150) %>% 
  spread(StackOverflowConsiderMember,n) %>% 
  mutate(No_to_Yes = round((No/Yes) * 100)) %>% 
  arrange(desc(No_to_Yes)) -> no_to_yes 
```

```{r}
head(no_to_yes,10) %>% 
  kable()
```


```{r}
data(worldgeojson, package = "highcharter")

highchart() %>%
hc_add_series_map(worldgeojson, no_to_yes, value = "No_to_Yes", joinBy = c('name','Country'))  %>% 
  #hc_colors(c("darkorange", "darkgray")) %>% 
    hc_colorAxis(stops = color_stops()) %>% 
    hc_title(text = "Countries - Ratio of No to Yes (feeling part of SO)") %>% 
    hc_subtitle(text = "Less is Better")


```



# Jobs

### Do you know SO Jobs?

The below plot answers an obvious answer that Stack Overflow Jobs as a Job searching platform is yet to be used by a lot of SO users.

```{r}

survey18 %>% 
  filter(!is.na(StackOverflowJobs)) %>% 
  group_by(StackOverflowJobs) %>% 
  count() %>% 
  hchart('column',hcaes('StackOverflowJobs','n')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Stack Overflow Jobs Awareness')
```


### Net Promoter Score

```{r}
library(highcharter)
survey18 %>% 
  filter(!is.na(StackOverflowJobsRecommend)) %>% 
 mutate(StackOverflowJobsRecommend = case_when(
    str_detect(StackOverflowJobsRecommend, "Not Likely") ~ "0",
    str_detect(StackOverflowJobsRecommend, "Very Likely") ~ "10",
    TRUE ~ StackOverflowJobsRecommend)) %>% 
group_by(StackOverflowJobsRecommend) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(StackOverflowJobsRecommend = reorder(StackOverflowJobsRecommend,as.numeric(StackOverflowJobsRecommend))) -> nps


  nps %>% 
 ggplot() + 
   geom_bar(aes(StackOverflowJobsRecommend,n), stat = 'identity', fill = "#FFB935")  +
   labs(x = NULL, y = "Count of respondents",
        title = "Net Promoter Score - Recommend SO Jobs?",
        subtitle = "How likely is it that you would recommend \n SO Jobs to a friend or colleague? ") 
 
  
  nps %>% 
    mutate(NPS = ifelse(as.numeric(StackOverflowJobsRecommend)>=9, "Promoter",
           ifelse(as.numeric(StackOverflowJobsRecommend)<7, "Detractor",'Passive'))) %>% 
    group_by(NPS) %>% 
    summarise(n = sum(n)) %>% 
    mutate(percent = (n/sum(n))*100 ) -> nps2
    
  
```


**Spread of Net Promoter Score Compontents**

```{r}

  nps2 %>%  hchart('column',hcaes('NPS','percent')) %>% 
  hc_colors(c("darkorange")) %>% 
  hc_title(text = 'Net Promoter Score - Stack Overflow Jobs')
```


**Net Promoter Score for Stack Overflow Jobs is:** 
```{r} 

nps2$percent[nps2$NPS=='Promoter'] - nps2$percent[nps2$NPS=='Detractor']

```

As long as we have got Positive NPS, It means there are people to promote Stack Overflow Jobs among their friends, but an NPS of **18** is no way near to Wow, Hence it sends out a message to SO team to improve SO Jobs visibility and make it more relevant that people can share. 

# Adblocker

## Do you have?

Being a Tech Savvy Group, **72%** of SO Survey Respondents use Adblocker

```{r}

survey18 %>%  
  filter(!is.na(AdBlocker)) %>% 
  group_by(AdBlocker) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100)
```


```{r}

survey18 %>%  
  filter(!is.na(AdBlocker)) %>% 
  group_by(AdBlocker) %>% 
  count() %>% 
  hchart('column',hcaes('AdBlocker','n')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Adblocker Usage')
```

### Adblocker usage - Male vs Female


```{r}

survey18 %>%  
filter(!is.na(AdBlocker)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AdBlocker,Gender) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100)
```


```{r}

survey18 %>%  
  filter(!is.na(AdBlocker)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AdBlocker,Gender) %>% 
  count() %>% 
  hchart('column',hcaes('AdBlocker','n',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Adblocker Usage') %>% 
  hc_yAxis(type = 'logarithmic', title = list(text = "Log Count") )

```
### Adblocker Disable Reasons

It is so great to see the second top reason why the respondents disabled Adblocker is because they wanted to support the site (knowing Ad revenue is a thing for them!) while still the top most reason is forceful unblocking requirement by few websites.

```{r}
survey18 %>% 
 filter(!is.na(AdBlockerReasons)) %>%
  select(AdBlockerReasons) %>%
  mutate(AdBlockerReasons = str_split(AdBlockerReasons, pattern = ";")) %>%
  unnest(AdBlockerReasons) %>%
  group_by(AdBlockerReasons) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(AdBlockerReasons = reorder(AdBlockerReasons,Count)) %>%
  hchart('bar',hcaes('AdBlockerReasons','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Adblocker Unblocking Top Reasons') 
```




### Online advertising can be valuable when it is relevant to me - Do you agree? 


```{r}
survey18 %>% 
 filter(!is.na(AdsAgreeDisagree1)) %>%
  select(AdsAgreeDisagree1) %>%
  group_by(AdsAgreeDisagree1) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Perc = round((Count / sum(Count)) * 100)) %>% 
  mutate(AdsAgreeDisagree1 = reorder(AdsAgreeDisagree1,Perc)) %>%
  hchart('bar',hcaes('AdsAgreeDisagree1','Perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Online advertising can be valuable when it is relevant to me - Do you agree?') 
```


###  I enjoy seeing online updates from companies that I like - Do you agree? 


```{r}
survey18 %>% 
 filter(!is.na(AdsAgreeDisagree2)) %>%
  select(AdsAgreeDisagree2) %>%
  group_by(AdsAgreeDisagree2) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Perc = round((Count / sum(Count)) * 100)) %>% 
  mutate(AdsAgreeDisagree2 = reorder(AdsAgreeDisagree2,Perc)) %>%
  hchart('bar',hcaes('AdsAgreeDisagree2','Perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' I enjoy seeing online updates from companies that I like - Do you agree?') 
```

###   I fundamentally dislike the concept of advertising - Do you agree? 


```{r}
survey18 %>% 
 filter(!is.na(AdsAgreeDisagree3)) %>%
  select(AdsAgreeDisagree3) %>%
  group_by(AdsAgreeDisagree3) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Perc = round((Count / sum(Count)) * 100)) %>% 
  mutate(AdsAgreeDisagree3 = reorder(AdsAgreeDisagree3,Perc)) %>%
  hchart('bar',hcaes('AdsAgreeDisagree3','Perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = '  I fundamentally dislike the concept of advertising - Do you agree?') 
```

###   Conclusion

From the above statements regarding Adblockers and Ads, We can understand that:

* Respondents are not categorically against Advertising
* Respondents are okay to disable Adblocker for the sites that they'd like to support
* Respondents are happy with Online Ads if they are relevant
* Respondents are happy with getting updates from the websites they like

Now, that in nutshell could form a conceptual idea of how Online Ads should be optimized. 

And for Stack Overflow, their platform to monetize / benefit from ads, these things should be strictly accounted not to disappoint their users. 


# Technology

## IDE

### Overall

Considering the strong domination of Web Developers' presence on SO, It's no doubt that tools to develop Apps are on the top. 


```{r}
survey18 %>% 
 filter(!is.na(IDE)) %>%
  select(IDE) %>%
  mutate(IDE = str_split(IDE, pattern = ";")) %>%
  unnest(IDE) %>%
  group_by(IDE) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(IDE = reorder(IDE,Count)) %>%
  hchart('bar',hcaes('IDE','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'IDE Preference') 
```


### by Gender

```{r}
survey18 %>% 
 filter(!is.na(IDE)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>%
  select(IDE, Gender) %>%
  mutate(IDE = str_split(IDE, pattern = ";")) %>%
  unnest(IDE) %>%
  group_by(IDE, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(IDE = reorder(IDE,Count)) %>%
  hchart('bar',hcaes('IDE','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'IDE Preference') %>% 
  hc_yAxis(type = 'logarithmic')
```

## OperatingSystem

### Overall

It's a no brainer to see Windows on the top, while Mac and Linux-based OS are not very far apart. 

```{r}
survey18 %>% 
 filter(!is.na(OperatingSystem)) %>%
  select(OperatingSystem) %>%
  mutate(OperatingSystem = str_split(OperatingSystem, pattern = ";")) %>%
  unnest(OperatingSystem) %>%
  group_by(OperatingSystem) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(OperatingSystem = reorder(OperatingSystem,Count)) %>%
  hchart('bar',hcaes('OperatingSystem','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'OperatingSystem Preference') 
```


### by Gender

```{r}
survey18 %>% 
 filter(!is.na(OperatingSystem)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>%
  select(OperatingSystem, Gender) %>%
  mutate(OperatingSystem = str_split(OperatingSystem, pattern = ";")) %>%
  unnest(OperatingSystem) %>%
  group_by(OperatingSystem, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(OperatingSystem = reorder(OperatingSystem,Count)) %>%
  hchart('bar',hcaes('OperatingSystem','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'OperatingSystem Preference') %>% 
  hc_yAxis(type = 'logarithmic')
```


## Which of the languages have you done extensive development work

### Overall

Again, It's evident SO is primarily Web development domainated crowd with Javascript and HTML on the top. Also, it could be seen that despite any claim - **SQL** still there widely used. 
```{r}
survey18 %>% 
 filter(!is.na(LanguageWorkedWith)) %>%
  select(LanguageWorkedWith) %>%
  mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
  unnest(LanguageWorkedWith) %>%
  group_by(LanguageWorkedWith) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(LanguageWorkedWith = reorder(LanguageWorkedWith,Count)) %>%
  hchart('bar',hcaes('LanguageWorkedWith','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'LanguageWorkedWith Preference') 
```


### by Gender

```{r}
survey18 %>% 
 filter(!is.na(LanguageWorkedWith)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>%
  select(LanguageWorkedWith, Gender) %>%
  mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
  unnest(LanguageWorkedWith) %>%
  group_by(LanguageWorkedWith, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(LanguageWorkedWith = reorder(LanguageWorkedWith,Count)) %>%
  hchart('bar',hcaes('LanguageWorkedWith','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'LanguageWorkedWith Preference - by Gender') %>% 
  hc_yAxis(type = 'logarithmic')
```



## Which language do you want to work in over the next year?

### Overall

Of the languages that respondents desire to learn, Javascript and Python lead the table. Thanks to the buzz of Machine Learning and AI and Apps Apps Apps! 

At the same time, we could also see desire in Ruby which is kind of thought to have lost its lust.
```{r}
survey18 %>% 
 filter(!is.na(LanguageDesireNextYear)) %>%
  select(LanguageDesireNextYear) %>%
  mutate(LanguageDesireNextYear = str_split(LanguageDesireNextYear, pattern = ";")) %>%
  unnest(LanguageDesireNextYear) %>%
  group_by(LanguageDesireNextYear) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(LanguageDesireNextYear = reorder(LanguageDesireNextYear,Count)) %>%
  hchart('bar',hcaes('LanguageDesireNextYear','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'LanguageDesireNextYear Preference') 
```


### by Gender

```{r}
survey18 %>% 
 filter(!is.na(LanguageDesireNextYear)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>%
  select(LanguageDesireNextYear, Gender) %>%
  mutate(LanguageDesireNextYear = str_split(LanguageDesireNextYear, pattern = ";")) %>%
  unnest(LanguageDesireNextYear) %>%
  group_by(LanguageDesireNextYear, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(LanguageDesireNextYear = reorder(LanguageDesireNextYear,Count)) %>%
  hchart('bar',hcaes('LanguageDesireNextYear','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'LanguageDesireNextYear Preference - by Gender') %>% 
  hc_yAxis(type = 'logarithmic')
```



## Which Framework have you worked in the past one year?

### Overall

**Node.js** seems to be the one ruling in this considering how popular MEAN stack was. But with  **React.js** catching up faster, It'd be an interesting thing to watch next year. 


```{r}
survey18 %>% 
 filter(!is.na(FrameworkWorkedWith)) %>%
  select(FrameworkWorkedWith) %>%
  mutate(FrameworkWorkedWith = str_split(FrameworkWorkedWith, pattern = ";")) %>%
  unnest(FrameworkWorkedWith) %>%
  group_by(FrameworkWorkedWith) %>% summarise(Count = n()) %>% 
  mutate(perc = round((Count / sum(Count)) * 100),2) %>% arrange(desc(perc)) %>% 
  kable() 
```

```{r}
survey18 %>% 
 filter(!is.na(FrameworkWorkedWith)) %>%
  select(FrameworkWorkedWith) %>%
  mutate(FrameworkWorkedWith = str_split(FrameworkWorkedWith, pattern = ";")) %>%
  unnest(FrameworkWorkedWith) %>%
  group_by(FrameworkWorkedWith) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(FrameworkWorkedWith = reorder(FrameworkWorkedWith,Count)) %>%
  hchart('bar',hcaes('FrameworkWorkedWith','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'FrameworkWorkedWith Preference') 
```


### by Gender

```{r}
survey18 %>% 
 filter(!is.na(FrameworkWorkedWith)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>%
  select(FrameworkWorkedWith, Gender) %>%
  mutate(FrameworkWorkedWith = str_split(FrameworkWorkedWith, pattern = ";")) %>%
  unnest(FrameworkWorkedWith) %>%
  group_by(FrameworkWorkedWith, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(FrameworkWorkedWith = reorder(FrameworkWorkedWith,Count)) %>%
  hchart('bar',hcaes('FrameworkWorkedWith','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'FrameworkWorkedWith Preference - by Gender') %>% 
  hc_yAxis(type = 'logarithmic')
```




## Which Framework you would like to work next year?

### Overall

**Node.js** still seems to be the one ruling while **React** has pushed **Angular** next. Meanwhile **Tensorflow** leads **PyTorch** in the ML world. 

```{r}
survey18 %>% 
 filter(!is.na(FrameworkDesireNextYear)) %>%
  select(FrameworkDesireNextYear) %>%
  mutate(FrameworkDesireNextYear = str_split(FrameworkDesireNextYear, pattern = ";")) %>%
  unnest(FrameworkDesireNextYear) %>%
  group_by(FrameworkDesireNextYear) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(FrameworkDesireNextYear = reorder(FrameworkDesireNextYear,Count)) %>%
  hchart('bar',hcaes('FrameworkDesireNextYear','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'FrameworkDesireNextYear Preference') 
```


### by Gender

```{r}
survey18 %>% 
 filter(!is.na(FrameworkDesireNextYear)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>%
  select(FrameworkDesireNextYear, Gender) %>%
  mutate(FrameworkDesireNextYear = str_split(FrameworkDesireNextYear, pattern = ";")) %>%
  unnest(FrameworkDesireNextYear) %>%
  group_by(FrameworkDesireNextYear, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(FrameworkDesireNextYear = reorder(FrameworkDesireNextYear,Count)) %>%
  hchart('bar',hcaes('FrameworkDesireNextYear','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'FrameworkDesireNextYear Preference - by Gender') %>% 
  hc_yAxis(type = 'logarithmic')
```

## What version control systems do you use regularly?

### Overall

**Git** Unanimously rules the Version Control world.
```{r}
survey18 %>% 
 filter(!is.na(VersionControl)) %>%
  select(VersionControl) %>%
  mutate(VersionControl = str_split(VersionControl, pattern = ";")) %>%
  unnest(VersionControl) %>%
  group_by(VersionControl) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(VersionControl = reorder(VersionControl,Count)) %>%
  hchart('bar',hcaes('VersionControl','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'VersionControl Preference') 
```


### by Gender

```{r}
survey18 %>% 
 filter(!is.na(VersionControl)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>%
  select(VersionControl, Gender) %>%
  mutate(VersionControl = str_split(VersionControl, pattern = ";")) %>%
  unnest(VersionControl) %>%
  group_by(VersionControl, Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(VersionControl = reorder(VersionControl,Count)) %>%
  hchart('bar',hcaes('VersionControl','Count', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'VersionControl Preference - by Gender') %>% 
  hc_yAxis(type = 'logarithmic')
```

# Feeling Towards Fellow Developers

## To what extend developers feel a sense of kinship or connection to other developers

### Kinship - Overall


The strength of websites like Stack overflow is that they are not just websites but actually have evolved to be a strong community which is just exactly confirmed by this response where a huge majority hav agreed to feel Kinship towards the community. 

But still around ~19K developers either disagree or neigher disagree nor agree, which is a significant number that there's still a decent room to improve the community companionship.


```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree1)) %>% 
  
  group_by(AgreeDisagree1) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100)

agree_order = factor(c('Strongly disagree','Disagree',
                            'Neither Agree nor Disagree','Agree',
                            'Strongly Agree'), levels = c('Strongly disagree','Disagree',
                            'Neither Agree nor Disagree','Agree',
                            'Strongly Agree'))
```


```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree1)) %>% 
  mutate(AgreeDisagree1 = case_when(str_detect(AgreeDisagree1, "Strongly disagree") ~ "1_Strongly disagree",
                                    str_detect(AgreeDisagree1, 'Neither Agree nor Disagree') ~ "3_Neither Agree nor Disagree",
                                    str_detect(AgreeDisagree1, 'Disagree') ~ '2_Disagree',
                                    
                                    str_detect(AgreeDisagree1, 'Agree') ~ "4_Agree",
                                    str_detect(AgreeDisagree1, 'Strongly agree') ~ "5_Strongly Agree")) %>% 
  group_by(AgreeDisagree1) %>% 
  count() %>% 
  hchart('column',hcaes('AgreeDisagree1','n')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Kinship towards Fellow Developers') %>% 
  hc_xAxis(categories = levels(agree_order))
```

### Kinship - Male vs Female


```{r}

survey18 %>%  
filter(!is.na(AgreeDisagree1)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AgreeDisagree1,Gender) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100)
```


```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree1)) %>% 
  mutate(AgreeDisagree1 = case_when(str_detect(AgreeDisagree1, "Strongly disagree") ~ "1_Strongly disagree",
                                    str_detect(AgreeDisagree1, 'Neither Agree nor Disagree') ~ "3_Neither Agree nor Disagree",
                                    str_detect(AgreeDisagree1, 'Disagree') ~ '2_Disagree',
                                    
                                    str_detect(AgreeDisagree1, 'Agree') ~ "4_Agree",
                                    str_detect(AgreeDisagree1, 'Strongly agree') ~ "5_Strongly Agree")) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AgreeDisagree1,Gender) %>% 
  count() %>% 
  hchart('column',hcaes('AgreeDisagree1','n',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Kinship - by Gender') %>% 
  hc_yAxis(type = 'logarithmic', title = list(text = "Log Count") )  %>% 
  hc_xAxis(categories = levels(agree_order))

```


## Do you think of yourself competing with your peers?

### Competing Feeling - Overall

Now this is a mixed result with a 35% of developers thinking that they are competing while 38% disagree. 

```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree2)) %>% 
  group_by(AgreeDisagree2) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100)
```


```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree2)) %>% 
  mutate(AgreeDisagree2 = case_when(str_detect(AgreeDisagree2, "Strongly disagree") ~ "1_Strongly disagree",
                                    str_detect(AgreeDisagree2, 'Neither Agree nor Disagree') ~ "3_Neither Agree nor Disagree",
                                    str_detect(AgreeDisagree2, 'Disagree') ~ '2_Disagree',
                                    
                                    str_detect(AgreeDisagree2, 'Agree') ~ "4_Agree",
                                    str_detect(AgreeDisagree2, 'Strongly agree') ~ "5_Strongly Agree")) %>% 
  group_by(AgreeDisagree2) %>% 
  count() %>% 
  hchart('column',hcaes('AgreeDisagree2','n')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Competing Feeling towards Fellow Developers')  %>% 
  hc_xAxis(categories = levels(agree_order))
```

### Competing Feeling - Male vs Female


```{r}

survey18 %>%  
filter(!is.na(AgreeDisagree2)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AgreeDisagree2,Gender) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100)
```


```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree2)) %>% 
  mutate(AgreeDisagree2 = case_when(str_detect(AgreeDisagree2, "Strongly disagree") ~ "1_Strongly disagree",
                                    str_detect(AgreeDisagree2, 'Neither Agree nor Disagree') ~ "3_Neither Agree nor Disagree",
                                    str_detect(AgreeDisagree2, 'Disagree') ~ '2_Disagree',
                                    
                                    str_detect(AgreeDisagree2, 'Agree') ~ "4_Agree",
                                    str_detect(AgreeDisagree2, 'Strongly agree') ~ "5_Strongly Agree")) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AgreeDisagree2,Gender) %>% 
  count() %>% 
  hchart('column',hcaes('AgreeDisagree2','n',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Competing Feeling - by Gender') %>% 
  hc_yAxis(type = 'logarithmic', title = list(text = "Log Count") )  %>% 
  hc_xAxis(categories = levels(agree_order))

```


## Imposter Syndrome - Do you think you are not good at programming as your peers

### Not good at programming - Overall

Overall, The effect of imposter syndrome seems to be minimal among the respondents with 38K respondents either disagreeing or strongly disagreeing it. But it looks like that this effect is more prevelant among Female Develpers than their male counterparts comparatively. 
```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree3)) %>% 
  group_by(AgreeDisagree3) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100)
```


```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree3)) %>% 
  mutate(AgreeDisagree3 = case_when(str_detect(AgreeDisagree3, "Strongly disagree") ~ "1_Strongly disagree",
                                    str_detect(AgreeDisagree3, 'Neither Agree nor Disagree') ~ "3_Neither Agree nor Disagree",
                                    str_detect(AgreeDisagree3, 'Disagree') ~ '2_Disagree',
                                    
                                    str_detect(AgreeDisagree3, 'Agree') ~ "4_Agree",
                                    str_detect(AgreeDisagree3, 'Strongly agree') ~ "5_Strongly Agree")) %>%  
  group_by(AgreeDisagree3) %>% 
  count() %>% 
  hchart('column',hcaes('AgreeDisagree3','n')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Feeling not good at programming as Peers')  %>% 
  hc_xAxis(categories = levels(agree_order))
```

### Imposter Syndrome - Male vs Female


While only 13.7% Male believe they feel they aren't good at programming, 22.5% Female feel so. On the contrary, 23.2% Male Strongly disagree this only 13.7% Female do so. This is clearly an issue of self-belief in the industry where Female developers don't get motivated.   

```{r}

survey18 %>%  
filter(!is.na(AgreeDisagree3)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AgreeDisagree3,Gender) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100)
```


```{r}

survey18 %>%  
  filter(!is.na(AgreeDisagree3)) %>% 
  mutate(AgreeDisagree3 = case_when(str_detect(AgreeDisagree3, "Strongly disagree") ~ "1_Strongly disagree",
                                    str_detect(AgreeDisagree3, 'Neither Agree nor Disagree') ~ "3_Neither Agree nor Disagree",
                                    str_detect(AgreeDisagree3, 'Disagree') ~ '2_Disagree',
                                    
                                    str_detect(AgreeDisagree3, 'Agree') ~ "4_Agree",
                                    str_detect(AgreeDisagree3, 'Strongly agree') ~ "5_Strongly Agree")) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(AgreeDisagree3,Gender) %>% 
  count() %>% 
  hchart('column',hcaes('AgreeDisagree3','n',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Feeling not good at programming as Peers - by Gender') %>% 
  hc_yAxis(type = 'logarithmic', title = list(text = "Log Count") )  %>% 
  hc_xAxis(categories = levels(agree_order))

```



# Hypothetical Tools on Stack Overflow

## A Peer Mentoring System

### A Peer Mentoring System - Overall 

* While 21% Female respondents are very interested, only 19% Male respondents feel so. 

* On the flip side, 20% of Male respondents are not at all interested, while only 17% Female respondents feel so.

```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools1)) %>% 
  group_by(HypotheticalTools1) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools1','perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'A Peer Mentoring System')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))
```

### A Peer Mentoring System - Male vs Female 


```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools1)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(HypotheticalTools1,Gender) %>% 
  count() %>%  
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools1','perc',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'A Peer Mentoring System - by Gender')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))

```


##  A private area for people new to programming

### A private area for people new to programming - Overall 

* Overall 29% Respondents are not at all interested.

* 16% Female are Extremely Interested but only 10% Male feel so. 

The above stats make it clear that It's Female Developers **who are new to programming** need to be incubated and nurtured and there's a possibility of them being lost in the crowd hence they are interested in a Private area for new programmers.

```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools2)) %>% 
  group_by(HypotheticalTools2) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools2','perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' A private area for people new to programming')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))
```

### A private area for people new to programming - Male vs Female 


```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools2)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(HypotheticalTools2,Gender) %>% 
  count() %>%  
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools2','perc',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' A private area for people new to programming - by Gender')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))

```



##  A programming-oriented blog platform

### A programming-oriented blog platform - Overall 

* Only 18% is not at all interested which makes this hypothetical programming-oriented blog a nice candidate to be real. 

```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools3)) %>% 
  group_by(HypotheticalTools3) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools3','perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' A programming-oriented blog platform')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))
```

### A programming-oriented blog platform - Male vs Female 


```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools3)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(HypotheticalTools3,Gender) %>% 
  count() %>%  
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools3','perc',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' A programming-oriented blog platform - by Gender')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))

```



##  An employer or job review system

### An employer or job review system - Overall 

* Only 13.5% is not at all interested which ensures that the space of recruitment is still a good domain to explore for Stack Overflow.

```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools4)) %>% 
  group_by(HypotheticalTools4) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools4','perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' An employer or job review system')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))
```

### An employer or job review system - Male vs Female 


```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools4)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(HypotheticalTools4,Gender) %>% 
  count() %>%  
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools4','perc',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' An employer or job review system - by Gender')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))

```


##  An area for Q&A related to career growth

### An area for Q&A related to career growth - Overall 

* Once again Only 13.65% is not at all interested and also 25% are very interested along with 18% of Extremely Interested, thus making this a powerful wanting feature/tool. 

```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools5)) %>% 
  group_by(HypotheticalTools5) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools5','perc')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' An area for Q&A related to career growth')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))
```

### An area for Q&A related to career growth - Male vs Female 


```{r}

survey18 %>%  
  filter(!is.na(HypotheticalTools5)) %>% 
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  group_by(HypotheticalTools5,Gender) %>% 
  count() %>%  
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (n / sum(n))*100) %>% 
  hchart('column',hcaes('HypotheticalTools5','perc',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' An area for Q&A related to career growth - by Gender')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))

```



# Independent Contractor Analysis

```{r}
indie18 <- survey18 %>% 
  filter(Employment %in% "Independent contractor, freelancer, or self-employed")
```


## Age by Gender

The first mover advantage for Male Respondents could be seen here. ~20% of Male in the age bracket of 18-24 are independent contractors / freelancer/self-employed while It's ~16% of Female in that range. 

```{r}
indie18 %>% 
 filter(!is.na(Gender)) %>%
  filter(!is.na(Age)) %>% 
  filter(Gender %in% c('Male','Female')) %>% 
  select(Gender,  Age) %>%
  mutate(Gender = str_split(Gender, pattern = ";")) %>%
  unnest(Gender) %>%
  group_by(Gender, Age) %>%
  summarise(Count = n()) %>%
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = (Count / sum(Count))*100) %>%  
  hchart('column',hcaes('Gender','perc', group = 'Age')) %>% 
  #hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Male vs Female by Age - Independent Contractor/Freelancer Analysis') %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))

```

## Salary Distribution 

Known for a better lifestyle and cool working culture, Europe leads US in rewarding Indie Contractors / Freelancers / Self-employed. 

```{r}
indie18 %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 200) %>% 
  summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
  arrange(med_sal) %>% 
  select(Country) %>% mutate(Country = factor(Country)) -> countries_salary



options(scipen=999)
indie18 %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n > 200) %>% 
  #arrange(desc(ConvertedSalary)) %>% 
  ungroup(Country) %>% 
  ggplot() +
  geom_violin(aes(Country,ConvertedSalary), fill = "#FFB935")  +
  scale_x_discrete(limits = countries_salary$Country) +
  coord_flip() +
  
  scale_y_log10() + 
  labs(x = "Country", 
       y = "Log of Annual Salary in USD",
       title = "Indie Contractor Salary - by Country -  \n More than 200 respondents")

```

## Formal Education

Among Indie Contractors/Freelancers, It is Female with Bachelor's 43% and Master's 26% lead their group than their equivalent Male counterpart whose Bachelor's is 38% and Master's is 23%. This could be possibly because of the fact that college-educated female develpers tend to take a freelancer role or a contractual role due the flexibility it provides. 

```{r}


indie18 %>%  
  filter(!is.na(FormalEducation)) %>% 
    filter(Gender %in% c('Male','Female')) %>% 
  group_by(FormalEducation,Gender) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = round((n / sum(n))*100),2) %>% 
  hchart('column',hcaes('FormalEducation','perc', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' FormalEducation')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))
```


## Tools 

Are you one of those who involved in the discussion that *PHP* is dead in the era of *MEAN stack* ? This might make you rethink your stand if it was against PHP. PHP that was in the 9th position for all developers, is in the 5th position for Indie Contractors - Behold, PHP is alive even much before your Python - probably because it's incredibly easier to build a PHP solution all by yourself and deploy it. 

```{r}
indie18 %>% 
 filter(!is.na(LanguageWorkedWith)) %>%
  select(LanguageWorkedWith) %>%
  mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
  unnest(LanguageWorkedWith) %>%
  group_by(LanguageWorkedWith) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(LanguageWorkedWith = reorder(LanguageWorkedWith,Count)) %>%
  hchart('bar',hcaes('LanguageWorkedWith','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Indie Contractors - LanguageWorkedWith Preference') 
```


## Frameworks 

### Worked in the past

Frameworks are inline with overall developers - with interest in Tensorflow jumping up to work in the next year. 
```{r}
indie18 %>% 
 filter(!is.na(FrameworkWorkedWith)) %>%
  select(FrameworkWorkedWith) %>%
  mutate(FrameworkWorkedWith = str_split(FrameworkWorkedWith, pattern = ";")) %>%
  unnest(FrameworkWorkedWith) %>%
  group_by(FrameworkWorkedWith) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(FrameworkWorkedWith = reorder(FrameworkWorkedWith,Count)) %>%
  hchart('bar',hcaes('FrameworkWorkedWith','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Indie Contractors - FrameworkWorkedWith Preference') 
```

### Desire to work in the future

```{r}
indie18 %>% 
 filter(!is.na(FrameworkDesireNextYear)) %>%
  select(FrameworkDesireNextYear) %>%
  mutate(FrameworkDesireNextYear = str_split(FrameworkDesireNextYear, pattern = ";")) %>%
  unnest(FrameworkDesireNextYear) %>%
  group_by(FrameworkDesireNextYear) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(FrameworkDesireNextYear = reorder(FrameworkDesireNextYear,Count)) %>%
  hchart('bar',hcaes('FrameworkDesireNextYear','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Indie Contractors - FrameworkDesireNextYear Preference') 
```

## Operating System

Mac OS is more popular among Independent Contractors / Freelanders than Overall Developers.


```{r}
indie18 %>% 
 filter(!is.na(OperatingSystem)) %>%
  select(OperatingSystem) %>%
  mutate(OperatingSystem = str_split(OperatingSystem, pattern = ";")) %>%
  unnest(OperatingSystem) %>%
  group_by(OperatingSystem) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(OperatingSystem = reorder(OperatingSystem,Count)) %>%
  hchart('bar',hcaes('OperatingSystem','Count')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Indie Contractors - OperatingSystem Preference') 
```

## Ethics Choice

```{r}
indie18 %>%  
  filter(!is.na(EthicsChoice)) %>% 
    filter(Gender %in% c('Male','Female')) %>% 
  group_by(EthicsChoice,Gender) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(Gender) %>% 
  mutate(perc = round((n / sum(n))*100),2) %>% 
  hchart('column',hcaes('EthicsChoice','perc', group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = ' Ethics Choice - Independent Contractors')  %>% 
  hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))
```



## Salary w.r.t Age

```{r}

indie18 %>% 
  filter(!is.na(Age)) %>% 
  filter(!is.na(ConvertedSalary),ConvertedSalary > 10) %>% 
  mutate(age_number = parse_number(Age)) %>% 
  ggplot() + geom_point(aes(ConvertedSalary,age_number), color = "#FFB935") +
    scale_x_log10() +

  labs(y = "Age Bracket", 
       x = "Log of Annual Salary in USD",
       title = "Age vs Salary - Independent Contractors") 
  
  
```

## Salary w.r.t YearsCoding

```{r}

indie18 %>% 
  filter(!is.na(YearsCoding)) %>% 
  filter(!is.na(ConvertedSalary),ConvertedSalary > 10) %>% 
  mutate(yrs_coding = parse_number(YearsCoding)) %>% 
  ggplot() + geom_point(aes(ConvertedSalary,yrs_coding), color = "#FFB935") +
  scale_x_log10() +
  labs(y = "YearsCoding - Bracket", 
       x = "Log of - Annual Salary in USD",
       title = "YearsCoding vs Salary - Independent Contractors") 
  
  
```

# Net Promoter Score

### What is NPS?

The Net Promoter Score is an index ranging from -100 to 100 that measures the willingness of customers to recommend a company's products or services to others. It is used as a proxy for gauging the customer's overall satisfaction with a company's product or service and the customer's loyalty to the brand.

Source: https://www.medallia.com/net-promoter-score/

## Stack Overflow NPS


```{r}
#library(highcharter)
survey18 %>% 
  filter(!is.na(StackOverflowRecommend)) %>% 
 mutate(StackOverflowRecommend = case_when(
    str_detect(StackOverflowRecommend, "Not Likely") ~ "0",
    str_detect(StackOverflowRecommend, "Very Likely") ~ "10",
    TRUE ~ StackOverflowRecommend)) %>% 
group_by(StackOverflowRecommend) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(StackOverflowRecommend = reorder(StackOverflowRecommend,as.numeric(StackOverflowRecommend))) -> nps


  nps %>% 
 ggplot() + 
   geom_bar(aes(StackOverflowRecommend,n), stat = 'identity', fill = "#FFB935")  +
   labs(x = NULL, y = "Count of respondents",
        title = "Net Promoter Score - Recommend Stack Overflow ",
        subtitle = "How likely is it that you would recommend \n Stack Overflow to a friend or colleague? ") 
 
  
  nps %>% 
    mutate(NPS = ifelse(as.numeric(StackOverflowRecommend)>=9, "Promoter",
           ifelse(as.numeric(StackOverflowRecommend)<7, "Detractor",'Passive'))) %>% 
    group_by(NPS) %>% 
    summarise(n = sum(n)) %>% 
    mutate(percent = (n/sum(n))*100 ) -> nps2
    
  
```

**Spread of Net Promoter Score Compontents**

```{r}
  nps2 %>%  hchart('column',hcaes('NPS','percent')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = 'Net Promoter Score - Stack Overflow')
```


**Net Promoter Score for Stack Overflow Jobs is:** 
```{r} 

nps2$percent[nps2$NPS=='Promoter'] - nps2$percent[nps2$NPS=='Detractor']

```

## NPS ~ Visit Frequency + Inclusiveness 

It can been seen from the below plot that even most of those who don't feel prat of the community still would like to recommend Stack Oveflow to their friends/family. 

```{r}


survey18 %>% 
  #filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  filter(!is.na(StackOverflowVisit), !StackOverflowVisit %in% "I have never visited Stack Overflow (before today)") %>% 
  mutate(StackOverflowVisit = ifelse(grepl("Daily or almost daily",StackOverflowVisit),"Daily",StackOverflowVisit )) %>% 
 mutate(StackOverflowRecommend = case_when(
    str_detect(StackOverflowRecommend, "Not Likely") ~ "0",
    str_detect(StackOverflowRecommend, "Very Likely") ~ "10",
    TRUE ~ StackOverflowRecommend)) %>% 
    mutate(NPS = ifelse(as.numeric(StackOverflowRecommend)>=9, "Promoter",
           ifelse(as.numeric(StackOverflowRecommend)<7, "Detractor",'Passive'))) %>%   filter(!is.na(StackOverflowConsiderMember)) %>% 
  filter(!is.na(StackOverflowRecommend)) %>% 
  group_by(StackOverflowVisit, StackOverflowConsiderMember, NPS) %>% 
  count() -> majors2
 
  
  ggplot(as.data.frame(majors2),
       aes(weight = n,
           axis2 = StackOverflowConsiderMember, axis1 = StackOverflowVisit, axis3 = NPS)) +
  geom_alluvium(aes(fill = StackOverflowConsiderMember),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE, 
            hjust = "inward", show.legend = T, check_overlap = T) +
  scale_x_continuous(breaks = 1:3, labels = c("Visit", "Inclusive", "NPS")) +
 # coord_flip() +
  ggtitle("Understanding Relation between Visit, Inclusiveness, NPS Score") 
```


## NPS ~ Age + Inclusiveness

```{r}


survey18 %>% 
  #filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  filter(!is.na(Age), !StackOverflowVisit %in% "I have never visited Stack Overflow (before today)") %>% 
 # mutate(StackOverflowVisit = ifelse(grepl("Daily or almost daily",StackOverflowVisit),"Daily",StackOverflowVisit )) %>% 
 mutate(StackOverflowRecommend = case_when(
    str_detect(StackOverflowRecommend, "Not Likely") ~ "0",
    str_detect(StackOverflowRecommend, "Very Likely") ~ "10",
    TRUE ~ StackOverflowRecommend)) %>% 
    mutate(NPS = ifelse(as.numeric(StackOverflowRecommend)>=9, "Promoter",
           ifelse(as.numeric(StackOverflowRecommend)<7, "Detractor",'Passive'))) %>%   filter(!is.na(StackOverflowConsiderMember)) %>% 
  filter(!is.na(StackOverflowRecommend)) %>% 
  group_by(Age, StackOverflowConsiderMember, NPS) %>% 
  count() -> majors2
 
  
  ggplot(as.data.frame(majors2),
       aes(weight = n,
           axis2 = StackOverflowConsiderMember, axis1 = Age, axis3 = NPS)) +
  geom_alluvium(aes(fill = StackOverflowConsiderMember),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE, 
            hjust = "inward", show.legend = T, check_overlap = T) +
  scale_x_continuous(breaks = 1:3, labels = c("Age", "Inclusive", "NPS")) +
 # coord_flip() +
  ggtitle("Understanding Relation between Age, Inclusiveness, NPS Score") 
```


That is an amazing number NPS of 86 shows how Stack Overflow is loved and recommendted and promoted by its own users.

# Insights Summary

* Stack Overflow (overall) boasts an **NPS of 86** that's off the chart in the world of Net Promoter Scores - to make some sense, Amazon's NPS according to npsbenchmarks.com is 69. 

* **Philippines, Taiwan, Malaysia** have the best  Female to Male Ratio and **Slovakia, Italy, Lithuania** have the worst Female to Male Ratio (in Countries with more than 200 respondents)

* Male Developers definitely **start at early age** (under 18 years) than Female Developers and also go on coding until later part of their life (now, this could have confirmation bias because there wasn't much Female developers in the past hence less reported numbers on SO)

* **Female Developers** are insanely less with just **7%** of the entire respondents being female against 92% Male Developers. 

* Countries like **United States, India, China** that have got more opportunities have got major variance in terms of Salary. 

* Apart from big players, **Israel, Canada and Mexico** are the countries that have crossed a Median Salary of more than 1M USD

* In **Nigeria**, Female Developers go well past Male Developers in terms of Median Salary. 

* While mostly Male Developers get paid more than Female Developers, **Netherlands** is one country where the difference is very minimal (among the major countries)

* Among countries with more than 500 developers, Spain and Italy being the Average - **United States, Switzerland, Israel, Australia, Germany** are the countries exceeding far above the average while **Pakistan, India, Iran, Mexico and Ukraine** are far below the average.

* **DevOps Specialists**, **Data Scientists**, **Product Managers** are the roles that make close to what Managers and C-suite Executives earn.

* **58% of Computer Science/Engineering and Health Science** Undergraduate Majors consider themselves to be part of Stack Overflow but only **44% of Fine Arts and 45% of Humanities & Social Science** Undergrad Majors consider to be part of Stack Overflow.

* **Computer Science/Engineering** Undergrad Majors participate in the site frequently while also feeling most inclusive while **Web Development Majors** particpate least but also feel most inclusive. On the other hand,  **Humanities** feel less inclusive and also particpate less while **Natural Science and Mathematics / Statistics** Undergrad Majors particpiate very frequently (more than CS) but feeling less inclusive. 

* **58% of Bacherlor's, Master's and Doctoral Degree** Formal Education consider themselves to be part of Stack Overflow but only **47% of School & 48% of Univeristy study without a degree** feel inclusive. 

* **Bacherlor's, Master's and Doctoral Degree** Formal Education participate frequently and also feel more inclusive but those with **School & University study without a degree** participate less and feel less inclusive too.

* **Mobile Developers** and **Students** are on the opposite spectrum of Feeling inclusive in Stack Overflow with only less than 50% of students feeling inclusive. 

* Developers who code in **Rust, Haskell, Lua and Go** feel less inclusive  while most of **Objecive-C, Swift, VB 6, VB.Net, Kotlin** feel inclusive. 

* Developers of **Python, Scala, Perl, Bash/Shell** are in the middle of this spectrum of Feeling Inclusive and Participating Frequently. 

* On an index of No_to_Yes Ratio for feeling Inclusive, **Finland, Norway, New Zealand, Canada and UK** lead the chart. 

* Still a huge chunk of Respondents (27K) don't use or never visited Stack Overflow Jobs

* Stack Overflow Jobs has a Net promoter Score of 18 which is positive but far from being a good score. 

* 74% of Male Survey Respondents and 66% of Female Respondents use Adblocker. 

* While Forcing users to disable Adblocker is the top reason why Adblocker was disabled, the second top reason is because Developers wnated to support the website they visited, which is very humane. 

* **40% of Somewhat agree* that Online Advertising can be valuable if it's relevant - this comment is coming in the age of Targeted Ads, which means Targeted ads are really not so relevant. 

* **Only 42% of developers Strongly agree and Somewhat agree** that they dislike the concept of advertising which means Advertising itself isn't hated as a concept. 

* **~19K developers** either disagree or neigher disagree nor agree, which is a significant number that there’s still a decent room to improve the community companionship.

* **Male Developers** feel more Kinship towards others than **Female developers**.

* **35% of developers** thinking that they are competing while **38% disagree**.

* Overall, The effect of **imposter syndrome** seems to be minimal among the respondents with **38K respondents** either disagreeing or strongly disagreeing it. 

* Imposter Syndrome (Feeling of not good at programming) looks like that this effect is more prevelant among Female Develpers than their male counterparts comparatively.

*  Considering the strong domination of Web Developers’ presence on SO, It’s no doubt that tools to develop Apps are on the top. 

*  It’s a no brainer to see **Windows** on the top, while Mac and Linux-based OS are not very far apart. 

* SO is primarily Web development domainated crowd with **Javascript** and **HTML** on the top. Also, it could be seen that despite any claim - **SQL** still there widely used. 

*  Of the languages that respondents desire to learn, **Javascript** and **Python** lead the table. 

*  **Node.js** seems to be the one ruling in this considering how popular MEAN stack was. But with **React.js** catching up faster, It’d be an interesting thing to watch next year. 

*  **Node.js** still seems to be the one ruling while React has pushed Angular next. Meanwhile **Tensorflow** leads PyTorch in the ML world. 

*  **Git** Unanimously rules the Version Control world. 

* While 21% Female respondents are very interested in a **Peer Mentoring System**, only 19% Male respondents feel so and on the flip side, 20% of Male respondents are not at all interested, while only 17% Female respondents feel so.

* For **A private area for people new to programming**, Overall 29% Respondents are not at all interested and 16% Female are Extremely Interested but only 10% Male feel so, that makes it clear that It’s Female Developers who are **new to programming need to be incubated and nurtured** and there’s a possibility of them being lost in the crowd hence they are interested in a Private area for new programmers.

* Only 18% is not at all interested which makes this hypothetical **programming-oriented blog** a nice candidate to be real.

* For an **Employer or Job Review system**, Only 13.5% is not at all interested which ensures that the space of recruitment is still a good domain to explore for Stack Overflow.

*  Only 13.65% is not at all interested and also 25% are very interested along with 18% of Extremely Interested, thus making **An area for Q&A related to career growth** a powerful wanting feature/tool.

*  The first mover advantage for Male Respondents could be seen here. **~20% of Male in the age bracket of 18-24** are **independent contractors / freelancer/self-employed** while It’s **~16% of Female** in that range.

*  Known for a better lifestyle and cool working culture, Europe leads US in rewarding Indie Contractors / Freelancers / Self-employed.

*  Among Indie Contractors/Freelancers, It is **Female with Bachelor’s 43% and Master’s 26% lead** their group than their equivalent **Male counterpart whose Bachelor’s is 38% and Master’s is 23%**. This could be possibly because of the fact that college-educated female develpers tend to take a freelancer role or a contractual role due the **flexibility **it provides.

*  **PHP** that was in the 9th position for all developers, is in the 5th position for Indie Contractors - Behold, PHP is alive even much before your **Python** - probably because it’s incredibly easier to build a PHP solution all by yourself and deploy it.

*  Frameworks are inline with overall developers - with interest in **Tensorflow** jumping up to work in the next year.

*  **Mac OS** is more popular among Independent Contractors / Freelancers than Overall Developers.


# Misc

## AI


### Whose Responsibility?

Close to half of the respondents feel, Developers and organizations who create AI products and solutions must take the ownership of responsibility in building ethical AI, while more than a quarter supports a body or Government to do the same. 

```{r}
survey18 %>% 
  filter(!is.na(AIResponsible)) %>% 
  count(AIResponsible) %>% 
  hchart("pie",hcaes(x = "AIResponsible", y = "n")) %>% 
  hc_add_theme(hc_theme_darkunica()) %>% 
  hc_title(text = 'What developers think whose responsibility to consider the ramifications of increasingly advanced AI technology?')
```
# Languages

### What Languages Israel use and want to learn?

Below is the interactive graph showing what language users want to learn what are all langauges, particularly for Israel being a Technology hub.

```{r}

library(igraph)
library(visNetwork)


survey18 %>% 
  filter(Country %in% "Israel") %>% 
  select(LanguageWorkedWith,LanguageDesireNextYear) %>% 
    mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
  unnest(LanguageWorkedWith) %>%
  filter(!is.na(LanguageWorkedWith)) %>% 
    mutate(LanguageDesireNextYear = str_split(LanguageDesireNextYear, pattern = ";")) %>%
  unnest(LanguageDesireNextYear) %>%
  filter(!is.na(LanguageDesireNextYear)) %>% 
  filter(!LanguageWorkedWith == LanguageDesireNextYear) -> network

net <- igraph::graph.data.frame(network)

visNetwork::visIgraph(net)



```