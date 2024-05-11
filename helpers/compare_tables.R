## read files from database
here()
schizophrenia = read_delim("data/schizophrenia.tsv")
hepC = read_delim("data/hepatitisC.tsv")
hepB = read_delim("data/heptatitisB.tsv")
benign_skin = read_delim("data/benign_skin.tsv")
melanoma = read_delim("data/melanoma.tsv")
colorectal = read_delim("data/colorectal_cancer.tsv")
breast = read_delim("data/breast_cancer.tsv")

schizophrenia_hepB = schizophrenia %>%
  dplyr::rename(globalScore_schizophrenia = globalScore) %>%
  inner_join(.,hepB %>% dplyr::select(symbol,globalScore_hepB=globalScore),by="symbol") %>%
  filter(globalScore_schizophrenia>=0.1,globalScore_hepB>=0.1) %>%
  dplyr::select(1,globalScore_schizophrenia,globalScore_hepB,3:41)

schizophrenia_symbol = schizophrenia %>%
  filter(globalScore>=0.1) %>%
  dplyr::select(symbol)
hepC_symbol = hepC %>%
  filter(globalScore>=0.1) %>%
  dplyr::select(symbol)

schizophrenia_hepC = schizophrenia %>%
  dplyr::rename(globalScore_schizophrenia = globalScore) %>%
  inner_join(.,hepC %>% dplyr::select(symbol,globalScore_hepC=globalScore),by="symbol") %>%
  filter(globalScore_schizophrenia>=0.1,globalScore_hepC>=0.1) %>%
  dplyr::select(1,globalScore_schizophrenia,globalScore_hepC,3:41)

schizophrenia_benign_skin = schizophrenia %>%
  dplyr::rename(globalScore_schizophrenia = globalScore) %>%
  inner_join(.,benign_skin %>% dplyr::select(symbol,globalScore_benign_skin=globalScore),by="symbol") %>%
  filter(globalScore_schizophrenia>=0.1,globalScore_benign_skin>=0.1) %>%
  dplyr::select(1,globalScore_schizophrenia,globalScore_benign_skin,3:41)

schizophrenia_melanoma = schizophrenia %>%
  dplyr::rename(globalScore_schizophrenia = globalScore) %>%
  inner_join(.,melanoma %>% dplyr::select(symbol,globalScore_melanoma=globalScore),by="symbol") %>%
  filter(globalScore_schizophrenia>=0.1,globalScore_melanoma>=0.1) %>%
  dplyr::select(1,globalScore_schizophrenia,globalScore_melanoma,3:41)

schizophrenia_colorectal = schizophrenia %>%
  # dplyr::select(symbol,globalScore_schizophrenia = globalScore) %>%
  dplyr::rename(globalScore_schizophrenia = globalScore) %>%
  inner_join(.,colorectal %>% dplyr::select(symbol,globalScore_colorectal=globalScore),by="symbol") %>%
  filter(globalScore_schizophrenia>=0.1,globalScore_colorectal>=0.1) %>%
  dplyr::select(1,globalScore_schizophrenia,globalScore_colorectal,3:41)

schizophrenia_breast = schizophrenia %>%
  # dplyr::select(symbol,globalScore_schizophrenia = globalScore) %>%
  dplyr::rename(globalScore_schizophrenia = globalScore) %>%
  inner_join(.,breast %>% dplyr::select(symbol,globalScore_breast=globalScore),by="symbol") %>%
  filter(globalScore_schizophrenia>=0.1,globalScore_breast>=0.1) %>%
  dplyr::select(1,globalScore_schizophrenia,globalScore_breast,3:41)

write.table(schizophrenia_breast, file="data/schizophrenia_breast.csv",sep = ",",quote = F,col.names = T,row.names = F)
write.table(schizophrenia_hepB, file="data/schizophrenia_hepB.csv",sep = ",",quote = F,col.names = T,row.names = F)
