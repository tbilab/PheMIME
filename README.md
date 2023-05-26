# Phecode-wide Multi-Institutional Multimorbidity Explorer (PheMIME)

## Introduction

We present Phecode-wide Multi-Institutional Multimorbidity Explorer (PheMIME) which is an interactive visualization tool of phenome-based disease network. By utilizing summary statistics from association analysis to represent the disease multimorbidity strength, a database was collected and computed from association analysis across three institutions: Vanderbilt University Medical Center, Massachusetts General Brigham, and UK Biobank. By framing the association analysis of disease multimorbidity as a network problem, it allows the analysis and inference of the latent disease phenotypes clustering, which extends the pairwise multimorbidity to the multivariate disease clusters. 


The tool takes an input from the selection of the interested disease phenotype and generates plots interactive with the table that either display the pairwise multimorbidity strength within single system or show the consistency of multimorbidity across systems, which gives users the freedom choosing disease phenotypes based on prior knowledge or doing analysis of adding or removing disease phenotypes according to the discovery by associationSubgraphs. The new version of associationSubgraphs embedded into the tool interactive with plots and tables allows tracking the trajectory of forming disease multimorbidity. Finally, the implementation with a use-case in Schizophrenia is demonstrated.


<img src="https://github.com/tbilab/PheMIME/raw/main/vignettes/overall.png"  width="950" height="600">


## Availability and implementation

PheMIME can be accessed at https://prod.tbilab.org/PheMIME/. 

Tutorial with a use-case example is available at https://prod.tbilab.org/PheMIME_supplementary_materials/. 

The source code is available at https://github.com/tbilab/PheMIME.



