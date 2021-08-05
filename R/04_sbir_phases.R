# This script builds a topic model on SBIR docs and then updates it to include STTR

# load libraries
library(tidyverse)
library(textmineR)
library(tidylda)
library(tidytext)


# load cleaned data
load("data-derived/sbir-clean.RData")

# try to match p1 to p2
phase1 <- sbir %>%
  filter(phase == "Phase I")

phase2 <- sbir %>%
  filter(phase == "Phase II")

phase_aligned <- 
  phase1 %>% 
  mutate(
    phase_id = paste(agency_tracking_number, program, contract) # still results in dups
  )%>%
  select(
    sbir_id_p1 = sbir_id,
    contract_p1 = contract,
    agency_tracking_p1 = agency_tracking_number,
    solicitation_number_p1 = solicitation_number,
    award_title_p1 = award_title,
    abstract_p1 = abstract,
    award_date_p1 = proposal_award_date,
    contract_end_date_p1 = contract_end_date,
    topic_code_p1 = topic_code,
    solicitation_year_p1 = solicitation_year,
    award_year_p1 = award_year,
    award_amount_p1 = award_amount,
    phase_id
  ) %>%
  inner_join(
    phase2 %>%
      mutate(
        phase_id = paste(agency_tracking_number, program, contract) # still results in dups
      ) %>%
      select(
        sbir_id_p2 = sbir_id,
        contract_p2 = contract,
        agency_tracking_p2 = agency_tracking_number,
        solicitation_number_p2 = solicitation_number,
        award_title_p2 = award_title,
        abstract_p2 = abstract,
        award_date_p2 = proposal_award_date,
        contract_end_date_p2 = contract_end_date,
        topic_code_p2 = topic_code,
        solicitation_year_p2 = solicitation_year,
        award_year_p2 = award_year,
        award_amount_p2 = award_amount,
        phase_id
      ),
    by = c("phase_id" = "phase_id")
  )

phase_aligned <- phase_aligned[, sort(names(phase_aligned))]


phase1_text <- 
  phase_aligned %>%
  select(
    phase_id,
    award_title_p1,
    abstract_p1
  ) %>%
  unnest_tokens(
    output = word,
    input = abstract_p1,
    token = "words",
    stopwords = stop_words$word
  ) 

phase1_dtm <- 
  phase1_text %>% 
  count(
    phase_id,
    word
  )  %>%
  filter(
    n > 1
  ) %>%
  cast_sparse(phase_id, word, n)



phase2_text <- 
  phase_aligned %>%
  select(
    phase_id,
    award_title_p2,
    abstract_p2
  ) %>%
  unnest_tokens(
    output = word,
    input = abstract_p2,
    token = "words",
    stopwords = stop_words$word
  ) 

phase2_dtm <- 
  phase2_text %>% 
  count(
    phase_id,
    word
  )  %>%
  filter(
    n > 1
  ) %>%
  cast_sparse(phase_id, word, n)


phase1_model <- 
  tidylda(
    data = phase1_dtm,
    k = 150,
    iterations = 200,
    burnin = 150,
    calc_likelihood = TRUE,
    calc_r2 = TRUE,
    verbose = TRUE
  )


phase2_model <- 
  refit(
    object = phase1_model,
    new_data = phase2_dtm,
    iterations = 200,
    burnin = 150,
    calc_likelihood = TRUE,
    calc_r2 = TRUE,
    beta_as_prior = TRUE,
    verbose = TRUE
  )

p2_model_noprior <- 
  refit(
    object = phase1_model,
    new_data = phase2_dtm,
    iterations = 200,
    burnin = 150,
    calc_likelihood = TRUE,
    calc_r2 = TRUE,
    beta_as_prior = FALSE,
    verbose = TRUE
  )

save(
  phase_aligned,
  phase1_text,
  phase1_dtm,
  phase1_model,
  phase2_text,
  phase2_dtm,
  phase2_model,
  p2_model_noprior,
  file = "data-derived/phase-models.RData"
)

tidy_beta <- phase1_model %>%
  tidy(matrix = "beta") %>%
  full_join(
    phase2_model %>% tidy(matrix = "beta"),
    by = c("topic" = "topic", "token" = "token")
  ) %>%
  full_join(
    p2_model_noprior %>% tidy(matrix = "beta"),
    by = c("topic" = "topic", "token" = "token")
  ) %>%
  select(
    topic,
    token,
    beta_p1 = beta.x,
    beta_p2 = beta.y,
    beta_p2_noprior = beta
  ) %>%
  replace_na(list(beta_p1 = 0, beta_p2 = 0, beta_p2_noprior = 0))

tidy_beta <- 
  tidy_beta %>%
  mutate(
    diff_p1p2 = beta_p2 - beta_p1,
    diff_p1p2_noprior = beta_p2_noprior - beta_p1
  )

tidy_beta %>%
  group_by(topic) %>%
  summarize(sum_diff = sum(abs(diff_p1p2))) %>%
  arrange(desc(sum_diff))

tidy_beta %>%
  group_by(topic) %>%
  summarize(sum_diff = sum(abs(diff_p1p2_noprior))) %>%
  arrange(desc(sum_diff))

skee <- 
  phase1_model %>% 
  augment(
    data = phase1_text %>% select(document = phase_id, term = word),
    type = "prob"
  )

skee <- 
  skee %>%
  pivot_longer(
    cols = -c(document, term),
    names_to = "topic",
    values_to = "theta"
    ) %>%
  mutate(
    topic = as.numeric(topic)
  ) %>%
  left_join(
    phase_aligned %>%
      select(
        phase_id,
        award_year = award_year_p1
      ),
    by = c("document" = "phase_id")
  )


skee %>%
  select(award_year, topic, theta) %>%
  filter(topic == 72) %>%
  group_by(award_year) %>%
  summarise(prevalence = sum(theta, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(aes(x = award_year, y = prevalence))

