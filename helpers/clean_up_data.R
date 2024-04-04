#comorbidity strength of vandy, mgh and ukbb
vumc_mgb = readRDS("data/combined_comorbidity.rds")
load("/home/siwei/paper_draft_prepare/PheMIME/data/combined_vandy250K_mgh250K_ukbb.rda")
ukb = aws.s3::s3readRDS(object="phenome/UKBB_regression_results_total_cleanup.rds",
                        bucket = "ukb.tbilab")
all_dat = vumc_mgb %>%
  left_join(.,ukb %>% dplyr::rename(a=phecode_a,b=phecode_b) %>%
              dplyr::select(a,b,z_ukbb=z_avg_ukbb,p_val_ukbb,overlap_ukbb)
              ,by=c("a","b")) %>%
  mutate(z_vandy_ukbb = (z_vandy*overlap_vandy+z_ukbb*overlap_ukbb)/(overlap_vandy+overlap_ukbb), 
         z_mgh_ukbb = (z_mgh*overlap_mgh+z_ukbb*overlap_ukbb)/(overlap_mgh+overlap_ukbb),
         z_vandy_mgh_ukbb = (z_vandy*overlap_vandy+z_mgh*overlap_mgh+z_ukbb*overlap_ukbb)/(overlap_vandy+overlap_ukbb+overlap_mgh),
         p_vandy_ukbb_max = pmax(p_val_vandy,p_val_ukbb),
         p_mgh_ukbb_max = pmax(p_val_mgh,p_val_ukbb),
         p_vandy_mgh_ukbb_max = pmax(p_val_vandy,p_val_mgh,p_val_ukbb)) %>%
  dplyr::select(a,b,z_vandy,z_mgh,z_ukbb,z_vandy_ukbb,z_vandy_mgh,z_mgh_ukbb,z_vandy_mgh_ukbb,
                p_val_vandy,p_val_mgh,p_val_ukbb,p_vandy_mgh_max,p_vandy_ukbb_max,p_mgh_ukbb_max,p_vandy_mgh_ukbb_max,
                overlap_vandy,overlap_mgh,overlap_ukbb)
##replace phecode with description, add similarity
all_dat = all_dat %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(a=phecode),by="a") %>%
  dplyr::rename(a_phecode = a) %>%
  dplyr::rename(a=description) %>%
  left_join(.,phecode_def %>% dplyr::select(phecode,description) %>% rename(b=phecode),by="b") %>%
  dplyr::rename(b_phecode = b) %>%
  dplyr::rename(b=description) %>%
  dplyr::select(a,b,a_phecode,b_phecode, z_vandy,z_mgh,z_ukbb,z_vandy_ukbb,z_vandy_mgh,z_mgh_ukbb,z_vandy_mgh_ukbb,
                p_val_vandy,p_val_mgh,p_val_ukbb,p_vandy_mgh_max,p_vandy_ukbb_max,p_mgh_ukbb_max,p_vandy_mgh_ukbb_max,
                overlap_vandy,overlap_mgh,overlap_ukbb) 
  #               %>%
  # left_join(.,com_sim %>% dplyr::select(a,b,sim_vandy,sim_mgh,sim_ukbb,sim_vandy_mgh,sim_vandy_ukbb,sim_mgh_ukbb),by=c("a","b"))

### calculate similarity scores
## common codes existing in (not necessary to only focus on common codes)
## focu on the unique codes in all systems
#common_codes <- all_dat %$%
  # filter(!is.na(z_vandy), !is.na(z_mgh), !is.na(z_ukbb)) %$%
#  unique(c(a,b))
common_codes = unique(c(all_dat$a,all_dat$b))

## Filter the associations to only ones involving codes present in both systems
common_code_associations <- all_dat %>%
  filter(a %in% common_codes, b %in% common_codes)

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
      return(cor(pairs$a, pairs$b,use = "complete.obs"))
    }
  }
  
  vandy_pairs <- combined_associations %>%
    dplyr::select(a = z_vandy_a, b = z_vandy_b) %>%
    filter(!is.na(a), !is.na(b))
  
  mgh_pairs <- combined_associations %>%
    dplyr::select(a = z_mgh_a, b = z_mgh_b) %>%
    filter(!is.na(a), !is.na(b))
  
  ukbb_pairs <- combined_associations %>%
    dplyr::select(a = z_ukbb_a, b = z_ukbb_b) %>%
    filter(!is.na(a), !is.na(b))
  
  tibble(
    a = code_a,
    b = code_b,
    sim_vandy = get_sim_cor(vandy_pairs),
    sim_mgh = get_sim_cor(mgh_pairs),
    sim_ukbb = get_sim_cor(ukbb_pairs),
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

library(furrr)
options(future.globals.maxSize = 4000 * 1024 ^ 2)
plan(multicore,workers = 30)
start_time = Sys.time()
print("Running associations pairs")
similarity_pairs <- mutate(
  furrr::future_map2_dfr(a_i, b_i, get_similarity_score),
  a = names(phecode_to_associations)[a],
  b = names(phecode_to_associations)[b]
)
print("Finished running pairs")
end_time = Sys.time()
end_time-start_time
saveRDS(similarity_pairs,file = "data/combined_similarity.rds")
similarity_pairs = readRDS("data/combined_similarity.rds")
all_dat = all_dat %>%
  left_join(.,similarity_pairs,by=c("a","b")) %>%
  mutate(sim_vandy_mgh=(sim_vandy*overlap_vandy+z_mgh*overlap_mgh)/(overlap_vandy+overlap_mgh),
         sim_vandy_ukbb=(sim_vandy*overlap_vandy+z_ukbb*overlap_ukbb)/(overlap_vandy+overlap_ukbb),
         sim_mgh_ukbb = (sim_ukbb*overlap_ukbb+z_mgh*overlap_mgh)/(overlap_ukbb+overlap_mgh))
saveRDS(all_dat,file = "data/combined_vandy250K_mgh250K_ukbb.rds")






