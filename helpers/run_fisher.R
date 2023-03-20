phe_dat = aws.s3::s3readRDS(object = "phenome/UKBB_ICD10CMtoPhecode_wide_08122022.rds", 
                            bucket = "ukb.tbilab")

## remove phecode columns with all 0 or all 1
phecode_remove = purrr::map_dbl(1:ncol(phe_dat), function(x) {
  
  remove=((sum(phe_dat[,x])==0) | (sum(phe_dat[,x])==nrow(phe_dat)))
  return(remove)
  
})
if(sum(phecode_remove)!=0) {
 phe_dat = phe_dat[,-c(which(phecode_remove==1))]
}

## begin run fisher's exact test 
fisher_dat = expand.grid(a=1:ncol(phe_dat),b=1:ncol(phe_dat))
uid = unique(unlist(fisher_dat[c("a", "b")], use.names=FALSE)) #remove the same disease pairs (AB==BA)
swap = match(fisher_dat[["a"]], uid) > match(fisher_dat[["b"]], uid)
tmp = fisher_dat[swap, "a"]
fisher_dat[swap, "a"] = fisher_dat[swap, "b"]
fisher_dat[swap, "b"] = tmp
fisher_dat <- fisher_dat[!duplicated(fisher_dat[1:2]),]
fisher_dat <- fisher_dat[fisher_dat$a!=fisher_dat$b,]
nrow(fisher_dat)
fisher_dat$a = colnames(phe_dat)[fisher_dat$a]
fisher_dat$b = colnames(phe_dat)[fisher_dat$b]

plan(multisession, workers = 50)
options(future.globals.maxSize = 2000 * 1024 ^ 2)
fisher_res = furrr::future_map2_dfr(fisher_dat$a,fisher_dat$b, function(a,b) {
  library(Matrix)
    fisher_res = fisher.test(table(phe_dat[,a],phe_dat[,b]))
    fisher_res = data.frame(a=a,b=b,pvalue=fisher_res$p.value,or=fisher_res$estimate)
    rownames(fisher_res) = NULL
  return(fisher_res)
})
saveRDS(fisher_res, file="all_comorbidity_phecodepairs_fisher_UKB.rds")
# fisher_res$a = colnames(phe_dat)[fisher_res$a]
# fisher_res$b = colnames(phe_dat)[fisher_res$b]

rr_res = furrr::future_map2_dfr(fisher_dat$a,fisher_dat$b, function(a,b) {
  library(mbest)
  formula = as.formula(paste0(a,"~",b))
  model <- glm(formula, data = phe_dat[,c(a,b)],
              family = poisson(link = "log"),method="firthglm.fit")
  rr_res <- data.frame(a=a,b=b,
  RR=summary(model)$coefficients[2,"Estimate"],
  pvalue=summary(model)$coefficients[2,"Pr(>|z|)"])
  rr_res
})


#test running time
start.time=Sys.time()
fisher_res = purrr::map2_dfr(fisher_dat$a[1:200],fisher_dat$b[1:200], function(a,b) {
  library(Matrix)
  fisher_res = fisher.test(table(phe_dat[,a],phe_dat[,b]))
  fisher_res = data.frame(a=a,b=b,pvalue=fisher_res$p.value,or=fisher_res$estimate)
  rownames(fisher_res) = NULL
  return(fisher_res)
})
end.time=Sys.time()
start.time-end.time
