#load packages
library(tidyverse)
library(readr)
library(janitor)

#import data
qlf_principles_affiliates_clean <- read_csv("data/qlf_principles_affiliates_clean.csv") %>% 
           clean_names()

#partnership analysis
partners_lead <- qlf_principles_affiliates_clean %>% 
  select(neighborhood, contains("lead")) %>% 
pivot_longer(cols = -neighborhood,
               names_to = "partner",
               values_to = "lead")

partners_lead %>% 
  filter(partner == "cbo_lead")

partners_lead %>% 
  filter(partner == "fbo_lead")

partners_lead %>% 
  filter(partner == "residents_lead")
  
partners_plan <- qlf_principles_affiliates_clean %>% 
  select(neighborhood, contains("plan")) %>% 
  pivot_longer(cols = -neighborhood,
               names_to = "partner",
               values_to = "plan")

partners_implement <- qlf_principles_affiliates_clean %>% 
  select(neighborhood, contains("implement")) %>% 
  pivot_longer(cols = -neighborhood,
               names_to = "partner",
               values_to = "implement")

qlf_principles_affiliates_clean %>% 
  count(cbo_lead, cbo_plan, cbo_implement, cbo_na)

qlf_principles_affiliates_clean %>% 
  count(fbo_lead)

qlf_principles_affiliates_clean %>% 
  count(fbo_plan)

qlf_principles_affiliates_clean %>% 
  count(fbo_implement)

qlf_principles_affiliates_clean %>% 
  tabyl(neighborhood, cbo_lead)

partners_all <- qlf_principles_affiliates_clean %>% 
  select(neighborhood, contains("_implement"), contains("plan"), contains("lead")) %>%  
         pivot_longer(cols = -neighborhood,
               names_to = "partner",
               values_to = "values")

partners_all %>% 
  filter(neighborhood == "Southwood (Charlottesville)") %>% 
 view()

qlf_principles_affiliates_clean %>% 
  select(neighborhood, contains("residents_lead"), contains("residents_plan"), contains("residents_imp")) %>% 
  view()
 

#coalition_diversity
diversity_questions <- qlf_principles_affiliates_clean %>% 
  select(neighborhood, coalition_diverstiy , resident_diversity) %>%  
  mutate(coal_div_num = case_when(
    coalition_diverstiy == "Not at all" ~ "1",
    coalition_diverstiy == "To a small extent" ~ "2",
    coalition_diverstiy == "To some extent" ~ "3",
    coalition_diverstiy == "To a moderate extent" ~ "4",
    coalition_diverstiy == "To a large extent" ~ "5")) %>% 
  mutate(coal_div_num = as.numeric(coal_div_num))

diversity_questions <- diversity_questions %>% 
  mutate(res_div_num = case_when(
    resident_diversity == "Not at all" ~ "1",
    resident_diversity == "To a small extent" ~ "2",
    resident_diversity == "To some extent" ~ "3",
    resident_diversity == "To a moderate extent" ~ "4",
    resident_diversity == "To a large extent" ~ "5")) %>% 
  mutate(res_div_num = as.numeric(res_div_num))

diversity_pivot <- diversity_questions %>%
  select(neighborhood, contains ("_num")) %>% 
  pivot_longer(cols = -neighborhood,
               names_to = "group",
               values_to = "value")


#overall questions
#implementing principles question
#recode as numeric
implement_principles <- qlf_principles_affiliates_clean %>% 
  select(neighborhood, contains("implement_")) %>% 
  mutate(impl_part_num = case_when(
    implement_partnerships == "Not at all" ~ "1",
    implement_partnerships == "To a small extent" ~ "2",
    implement_partnerships == "To some extent" ~ "3",
    implement_partnerships == "To a moderate extent" ~ "4",
implement_partnerships == "To a large extent" ~ "5")) %>% 
  mutate(impl_part_num = as.numeric(impl_part_num))
  
implement_principles <- implement_principles %>% 
  mutate(impl_build_num = case_when(
    implement_build_power == "Not at all" ~ "1",
    implement_build_power == "To a small extent" ~ "2",
    implement_build_power == "To some extent" ~ "3",
    implement_build_power == "To a moderate extent" ~ "4",
    implement_build_power == "To a large extent" ~ "5")) %>% 
  mutate(impl_build_num = as.numeric(impl_build_num))

implement_principles <- implement_principles %>% 
  mutate(impl_acct_num = case_when(
    implement_accountability == "Not at all" ~ "1",
    implement_accountability == "To a small extent" ~ "2",
    implement_accountability == "To some extent" ~ "3",
    implement_accountability == "To a moderate extent" ~ "4",
    implement_accountability == "To a large extent" ~ "5")) %>% 
  mutate(impl_acct_num = as.numeric(impl_acct_num))

implement_principles <- implement_principles %>% 
  mutate(impl_learn_num = case_when(
    implement_learn == "Not at all" ~ "1",
    implement_learn == "To a small extent" ~ "2",
    implement_learn == "To some extent" ~ "3",
    implement_learn == "To a moderate extent" ~ "4",
    implement_learn == "To a large extent" ~ "5")) %>% 
  mutate(impl_learn_num = as.numeric(impl_learn_num))

implement_principles <- implement_principles %>% 
  mutate(impl_systems_num = case_when(
    implement_systems == "Not at all" ~ "1",
    implement_systems == "To a small extent" ~ "2",
    implement_systems == "To some extent" ~ "3",
    implement_systems == "To a moderate extent" ~ "4",
    implement_systems == "To a large extent" ~ "5")) %>% 
  mutate(impl_systems_num = as.numeric(impl_systems_num))

implement_principles_pivot <- implement_principles %>%
  select(neighborhood, contains ("impl_")) %>% 
  pivot_longer(cols = -neighborhood,
                             names_to = "principle",
                             values_to = "value")

implement_principles_avg <- implement_principles_pivot %>% 
 group_by(principle) %>% 
  summarize(mean = mean(value)) %>% 
  arrange(desc(mean)) %>% 
  mutate(principle = fct_reorder(principle, mean))

  
  ggplot(data = implement_principles_avg, 
       mapping = aes(x = principle, 
                     y = mean,
                     fill = principle
                     )) +
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = mean),
            vjust = 1.5,
            color = "black")
  

#Useful resources
useful_resources <- qlf_principles_affiliates_clean %>% 
    select(neighborhood, contains("useful_"), -contains("other")) %>% 
  pivot_longer(cols = -neighborhood,
                names_to = "resource",
                values_to = "value")

useful_resources <- useful_resources %>% 
    mutate(value_num = case_when(
      value == "Not at all" ~ "1",
      value == "To a small extent" ~ "2",
      value == "To some extent" ~ "3",
      value == "To a moderate extent" ~ "4",
      value == "To a large extent" ~ "5")) %>% 
    mutate(value_num = as.numeric(value_num))

summary_useful_resources <- useful_resources%>% 
  group_by(resource) %>% 
  summarise(mean = mean(value_num, na.rm = TRUE)) %>% 
  arrange(mean) %>% 
  mutate(resource = fct_reorder(resource, mean))
  
  ggplot(data = summary_useful_resources,
           mapping = aes(x = resource, 
                       y = mean)) +
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = mean),
            vjust = 1.5,
            color = "white") +
    theme_minimal()

#Principles Questions
Principles_questions <- qlf_principles_affiliates_clean %>% 
  select(neighborhood, contains("equity_"), contains("learn_"), contains("account_"), contains("systems_")) %>% 
  pivot_longer(cols = -neighborhood,
               names_to = "question",
               values_to = "response")

Principles_questions <- Principles_questions %>% 
  mutate(value_num = case_when(
    response == "Novice" ~ "1",
    response == "Intermediate" ~ "2",
    response == "Advanced" ~ "3",
    response == "Expert" ~ "4",
    response == "Not at all" ~ "1",
    response == "To a small extent" ~ "2",
    response == "To some extent" ~ "3",
    response == "To a moderate extent" ~ "4",
    response == "To a large extent" ~ "5")) %>% 
  mutate(value_num = as.numeric(value_num)) 

Principles_questions_summary <- Principles_questions%>% 
  group_by(question) %>%
  summarise(mean = mean(value_num)) %>% 
  mutate(principle = case_when(
    question == "account_knowledgeable_racism_context" ~ "account",
    question == "account_processes_transparency" ~ "account",
    question == "account_support_resident_access" ~ "account",
    question == "equity_center" ~ "equity",
    question == "equity_reflect" ~ "equity",
    question == "equity_reflect_power_dynamics" ~ "equity",
    question == "equity_training_barriers" ~ "equity",
    question == "equity_training_cultural_competence" ~ "equity",
    question == "equity_understand_barriers" ~ "equity",
    question == "equity_understand_root_causes" ~ "equity",
    question == "learn_develop_ideas" ~ "learn",
    question == "learn_failure_asopporutnity" ~ "learn",
    question == "learn_invest_new_approaches" ~ "learn",
    question == "learn_support_new_thinking" ~ "learn",
    question == "systems_engage_residents_systems" ~ "systems",
    question == "systems_support_systems_efforts" ~ "systems"))
  
Principles_questions_summary %>% 
  ggplot(mapping = aes(x = mean, 
                         y = 1,
                       fill = principle)) +
      geom_point(show.legend = FALSE) +
      theme_minimal() 

  

#Export data
write_rds(qlf_principles_affiliates_clean,
          file = "data/qlf_principles_affiliates_for_R")
            