### ---------------------------------------------------------------------------------------- ###
# ----------------------------- General Analysis of drsg results ----------------------------- #
### ---------------------------------------------------------------------------------------- ###

## Should ecologist prefer model-based over algorithm-based methods?
## 02.12.19

### --- OVERVIEW --- ###
#01.Setup
#02.p-values with standart deviations
### ---------------- ### 


# 01.Setup -------------------------------------------------------------------

pacman::p_load(dplyr, data.table, ggplot2)

setwd("~/01_Uni/My Papers/1909_Should ecologists prefer model- over algorithm-based multivariate methods/r_scripts/reviewer_comments/03_analyse_results/")

data = readRDS("drsg_all_results.RDS")


# 02. p-values with standart deviations  ---------------------------------------



data %>% 
      group_by(method, variable) %>%
      summarize(mean = mean(p.value), 
                       sd = sd(p.value))


# False positive and Negative Rate ----------------------------------------

## What is the overall false positive rate?
all %>% group_by(Method) %>% summarize(false.positives = sum(false.positive)) -> false.positives
all %>% group_by(Method) %>% summarize(total.Positives = sum(p.value <= 0.05)) -> Total.Positives
fpr <- left_join(false.positives, Total.Positives, by = "Method")
(fpr %<>% mutate(false.positive.rate = false.positives / total.Positives))

## What is the overall false negative rate?
all %>% group_by(Method) %>% summarize(false.negatives = sum(false.negative)) -> false.negatives
all %>% group_by(Method) %>% summarize(total.negatives = sum(p.value > 0.05)) -> Total.negatives
fnr <- left_join(false.negatives, Total.negatives, by = "Method")
(fnr %<>% mutate(false.negative.rate = false.negatives / total.negatives))


# Stop sourcing -----------------------------------------------------------

if (1 == 1) {
  stop()
}

# Runtime  ----------------------------------------------------------------

all %>%
  filter(Method %in% c("MvGLM", "CQO")) %>%
  group_by(Method, Samples) %>%
  summarize(mean = mean(runtime)) %>%
  arrange(mean)

all %>%
  filter(Method %in% c("MvGLM", "CQO")) %>%
  group_by(Method, Response) %>%
  summarize(mean = mean(runtime)) %>%
  arrange(mean)

## check
fpr[, 3] + fnr[, 3]

# Plots -------------------------------------------------------------------

## runtime
all %>%
  ggplot(aes(y = runtime / 60, x = Samples)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Method)


## General Overview
data %>%
  ggplot(aes(y = p.value, x = method, color = variable)) +
  geom_point() 


  facet_wrap(Variable ~ Method) +
  geom_hline(yintercept  = 0.05) +
  geom_jitter(alpha = 0.5,
              size = 0.5,
              pch = 21,
              aes(fill = Response))

## FPR/ FNR


### --- MVGLM --- ###
all %>% 
  filter(Method == "MvGLM") %>% 
  ggplot(aes(y = `Test Statistic`, x = Samples)) + 
  geom_point(shape = 21, size = 2,aes(fill = Variable)) + 
  facet_wrap(.~Response)
all %>%
  filter(Method == "MvGLM") %>% 
  ggplot(aes(y = p.value, x = Samples, color = Response)) +
  geom_smooth(se = T, lwd = 2) +
  facet_wrap(Variable ~ Response) +
  geom_hline(yintercept  = 0.05) +
  geom_jitter(alpha = 1,
              size = 1,
              pch = 21,
              aes(fill = Response))
all %>% 
  filter(Method == "MvGLM") %>% 
  ggplot(aes(y = , x = Samples)) + 
  geom_point() + 
  facet_wrap(.~Response)

# Numbers for Result section ----------------------------------------------

### --- Overview Table --- ### 

all %>% 
  group_by(Method, Variable) %>% 
  summarise(Mean =  mean(p.value),
            SD   =  sd(p.value))

### --- MvGLM --- ###

all %>% filter(Method == "MvGLM") %>% View()

# higher p-values for env2 in low sample sizes
all %>%
  filter(Method == "MvGLM", Variable == "env2", Samples == 25) %>% summarize(mean =
                                                                               mean(p.value), sd = sd(p.value))
all %>%
  filter(Method == "MvGLM", Variable == "env2", Samples != 25) %>%  summarize(mean =
                                                                                mean(p.value), sd = sd(p.value))
# SD in Noise  linear vs unimodal and bimodal explanatroy variable
# Not reöevant anymore
all %>% filter(Method == "MvGLM",
               Response1 == "L" | Response2 == "L",
               Variable == "Noise") %>%
  summarise(sd = sd(p.value))
all %>% filter(Method == "MvGLM",
               Response1 == "U" | Response2 == "U",
               Variable == "Noise") %>%
  summarise(sd = sd(p.value))
all %>% filter(Method == "MvGLM",
               Response1 == "B" | Response2 == "B",
               Variable == "Noise") %>%
  summarise(sd = sd(p.value))

# Noise variable below 0.05
all %>% filter(Method == "MvGLM", Variable == "Noise", p.value <= 0.05) %>% View()

# Effect of sample size on Noise variables
all %>% 
  filter(Method == "MvGLM", Variable == "Noise", Response == "BB") %>% 
  group_by(Samples) %>% 
  summarise(MEAN = mean(p.value),
           SD   = sd(p.value))

# Which models lead to FNR 
all %>% 
  filter(Method == "MvGLM", Variable == "env1"|Variable == "env2", p.value > 0.05) %>% 
  View()
