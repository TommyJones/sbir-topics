# load libraries
library(tidyverse)
library(textmineR)
library(tidylda)

# load cleaned data
load("data-derived/sbir-clean.RData")

# Create a TCM with 10-degree skipgrams
# Creating titles and abstracts as separate documents so that titles are handled
# in isolation when constructing skipgrams
sbir_tcm <- CreateTcm(
  doc_vec = c(sbir_text$award_title, sbir_text$abstract),
  skipgram_window = 10, # arbitrary but standard
  stopword_vec = stopwords::stopwords("en"),
  verbose = TRUE
)

# vocab pruning round 1
vocab_keep <- colnames(sbir_tcm)[colSums(sbir_tcm) > 1]

sbir_tcm <- sbir_tcm[vocab_keep, vocab_keep]

# completing a step that textmineR should've done
sbir_tcm <- sbir_tcm + t(sbir_tcm) 

# vocabulary pruning
sbir_tf <- TermDocFreq(sbir_tcm) %>%
  as_tibble()

vocab_keep <- sbir_tf$term[sbir_tf$doc_freq > 20]

sbir_tcm <- sbir_tcm[vocab_keep, vocab_keep]

save(
  sbir_tcm,
  file = "data-derived/sbir-tcm.RData"
)

# train an LDA model off of it
sbir_embedding <- tidylda(
  data = sbir_tcm,
  k = 100, # arbitrary and arguably should be much bigger
  iterations = 200,
  burnin = 175,
  calc_likelihood = TRUE,
  calc_r2 = TRUE,
  verbose = TRUE,
  threads = 4
)

save(
  sbir_embedding,
  file = "data-derived/sbir-embedding-v65k.RData"
)

batches <- seq(1, nrow(title_dtm), by = 500)

batches <- batches %>%
  map(function(x){
    x:min(x + 499, nrow(title_dtm))
  })


title_theta <- batches %>%
  map(function(x){
    predict(
      object = sbir_embedding,
      new_data = title_dtm[x, ],
      iterations = 100,
      burnin = 50,
      threads = 5,
      verbose = TRUE
    )
  })

title_theta <- 
  title_theta %>%
  map(tidy, matrix = "theta") %>%
  bind_rows()

save(
  title_theta, 
  file = "data-derived/embedded-titles.RData"
)
