library(tidyverse)
library(phewasHelper)
library(here)
library(magrittr)
library(furrr)

plan(multiprocess, workers = 10)

#comorbidity strength of vandy, mgh and ukbb
combined_associations <- read_rds("/home/siwei/cross_institutions_multimorbidity/intermediate_data/combined_associations.rds")
load("~/cseToolkit/R/combined_vandy250K_mgh250K_ukbb.rda")
load("~/cseToolkit/R/phecode_def.rda")

all_dat = all_dat %>% dplyr::select(z_avg_ukbb,overlap_ukbb,a,b) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(a=description),by="a") %>%
  dplyr::select(-a) %>%
  dplyr::rename(a=phecode) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(b=description),by="b") %>%
  dplyr::select(-b) %>%
  dplyr::rename(b=phecode)

combined_associations = combined_associations %>%
  left_join(., all_dat, by=c("a","b")) %>%
  rename(ukbb = z_avg_ukbb)

common_codes <- combined_associations %>%
  filter(!is.na(vandy), !is.na(mgh), !is.na(ukbb)) %$%
  unique(c(a,b))

# Filter the associations to only ones involving codes present in both systems
common_code_associations <- combined_associations %>%
  filter(a %in% common_codes, b %in% common_codes) %>%
  select(-overlap_mgh, -overlap_vandy,-overlap_ukbb)

print("Setting up associations datastructure")

phecode_to_associations <- bind_rows(
  common_code_associations,
  common_code_associations %>% rename(a = b, b = a)
) %>%
  mutate(b = furrr::future_map_int(b, ~which(common_codes == .))) %>%
  group_by(a) %>%
  nest() %$%
  set_names(data, a)
print("Finished setting up associations datastructure")

get_similarity_score <- function(code_a, code_b){
  combined_associations <- inner_join(
    phecode_to_associations[[code_a]],
    phecode_to_associations[[code_b]],
    by = "b",
    suffix = c("_a", "_b")
  )

  get_sim_cor <- function(pairs){

    if(nrow(pairs) < 2){
      return(NA)
    } else {
      return(cor(pairs$a, pairs$b))
    }
  }

  vandy_pairs <- combined_associations %>%
    select(a = vandy_a, b = vandy_b) %>%
    filter(!is.na(a), !is.na(b))

  mgh_pairs <- combined_associations %>%
    select(a = mgh_a, b = mgh_b) %>%
    filter(!is.na(a), !is.na(b))

  ukbb_pairs <- combined_associations %>%
    select(a = ukbb_a, b = ukbb_b) %>%
    filter(!is.na(a), !is.na(b))

  tibble(
    a = code_a,
    b = code_b,
    vandy = get_sim_cor(vandy_pairs),
    mgh = get_sim_cor(mgh_pairs),
    ukbb = get_sim_cor(ukbb_pairs),
    vandy_size = nrow(vandy_pairs),
    mgh_size = nrow(mgh_pairs),
    ukbb_size = nrow(ukbb_pairs)
  )
}



# Setup indices for all possible pairs of n values
n <- length(phecode_to_associations)
rep_counts <- (n:1) - 1
a_i <- rep(1:n, times = rep_counts)
b_i <- unlist(lapply(rep_counts, function(x){utils::tail(1:n, x)}))


print("Running associations pairs")

similarity_pairs <- mutate(
  furrr::future_map2_dfr(a_i, b_i, get_similarity_score),
  a = names(phecode_to_associations)[a],
  b = names(phecode_to_associations)[b]
)

print("Finished running pairs")
#
# print("Filtering to successful pair results")
# no_errors <- similarity_pairs %>%
#   # head(1500) %>%
#   filter(no_error)
#
# final_sims <- bind_cols(
#   select(no_errors, a, b),
#   future_map_dfr(no_errors$similarities, "result") %>%
#     select(vandy_sim, mgh_sim)
# )

print("Saving to disk")
write_rds(similarity_pairs, "/home/siwei/cseToolkit/R/combined_similarities.rds")
similarity_pairs = readRDS("/home/siwei/cseToolkit/R/combined_similarities.rds")

similarity_pairs = similarity_pairs %>%
  # dplyr::select(a,b,ukbb,ukbb_size) %>%
  # rename(sim_ukbb=ukbb) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(a=phecode),by="a") %>%
  dplyr::select(-a) %>%
  rename(a=description) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(b=phecode),by="b") %>%
  dplyr::select(-b) %>%
  rename(b=description)

combined_sim <- read_rds("/home/siwei/cross_institutions_multimorbidity/intermediate_data/combined_similarities.rds")
combined_sim = combined_sim %>%
  # dplyr::select(a,b,ukbb,ukbb_size) %>%
  # rename(sim_ukbb=ukbb) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(a=phecode),by="a") %>%
  dplyr::select(-a) %>%
  rename(a=description) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(b=phecode),by="b") %>%
  dplyr::select(-b) %>%
  rename(b=description) %>%
  rename(sim_vandy=vandy,sim_mgh=mgh)

all_dat = all_dat %>%
  left_join(.,combined_sim,by=c("a","b")) %>%
  left_join(.,similarity_pairs,by=c("a","b"))
save(all_dat,file="all_dat.rda")

com_sim = all_dat %>%
  dplyr::select(z_vandy=z_avg_vandy,p_val_vandy,overlap_vandy,
                z_ukbb=z_avg_ukbb,p_val_ukbb,overlap_ukbb,
                z_mgh=z_avg_mgh,p_val_mgh,overlap_mgh,
                p_vandy_ukbb_max,p_vandy_mgh_max,p_mgh_ukbb_max,
                p_vandy_mgh_ukbb_max,
                z_vandy_ukbb=z_vandy_ukbb_combined,
                z_vandy_mgh=z_vandy_mgh_combined,
                z_mgh_ukbb=z_mgh_ukbb_combined,
                z_vandy_mgh_ukbb=z_combined_vandy_mgh_ukbb,
                a,b,sim_vandy,sim_mgh,sim_ukbb)
save(com_sim,file="combined_vandy250K_mgh250K_ukbb.rda")

com_sim = com_sim %>%
  mutate(sim_vandy_mgh = (sim_vandy*overlap_vandy+sim_mgh*overlap_mgh)/(overlap_vandy+overlap_mgh)) %>%
  mutate(sim_vandy_ukbb = (sim_vandy*overlap_vandy+sim_ukbb*overlap_ukbb)/(overlap_vandy+overlap_ukbb)) %>%
  mutate(sim_mgh_ukbb = (sim_mgh*overlap_mgh+sim_ukbb*overlap_ukbb)/(overlap_mgh+overlap_ukbb))
