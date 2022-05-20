---
title: "Irene Hsueh's BS 858 Homework 3"
author: "Irene Hsueh"
date: "10/4/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

# Reading in Dataset
```{r}
ldl <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 858 - Statistical Genetics I/Class 3 - Assessing Genetic Component of Phenotype/Homework 3/ldl.csv") %>% 
#Reordering and Renaming Variables
  dplyr::select(family_id = famid, 
                child1_id = kid1id,
                child2_id = kid2id,
                mother_id = momid,
                father_id = dadid, 
                child1_ldl = kid1ldl,
                child2_ldl = kid2ldl, 
                mother_ldl = momldl, 
                father_ldl = dadldl) %>% 
  rowwise() %>% 
  mutate(parent_average = mean(c(mother_ldl, father_ldl)))
head(ldl, 20)
```



# Estimating Heritability 
```{r}
n_families <- sum(table(ldl$family_id))


#Heritability Based on Parent-Offspring Relationship 
child1_parent_model <- lm(child1_ldl ~ parent_average, data=ldl)
summary(child1_parent_model)

child2_parent_model <- lm(child2_ldl ~ parent_average, data=ldl)
summary(child2_parent_model)



#Heritability Based on Sibling Pairs 
child_ldl <- c(ldl$child1_ldl, ldl$child2_ldl)
mean_child_ldl <- mean(child_ldl)
stdv_child_ldl <- sd(child_ldl)
n_row <- nrow(ldl)

adjusted_child1 <- ldl$child1_ldl - mean_child_ldl
adjusted_child2 <- ldl$child2_ldl - mean_child_ldl
icc <- sum(adjusted_child1 * adjusted_child2) / ((n_row-1)*(stdv_child_ldl)^2)
narrow_sense_heritability <- 2*icc
```







