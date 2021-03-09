
## Part N. Packages
library(data.table)
library(tidyverse)
library(ggpubr)
set.seed(117)
options(dplyr.summarise.inform = FALSE)

### Part N-1. The Actual Dataset
group <- c(rep("A", 50), rep("D", 50))

generate <- function(g){
  x1 <- c(rbinom(50, 1, 2/3), rbinom(50, 1, 1/3))
  x2 <- c(rbinom(50, 1, 2/3), rbinom(50, 1, 1/3))
  groups <- data.frame(g, x1, x2)
  rows <- sample(nrow(groups))
  gen <- data.frame(groups[rows,])
  generated <- gen %>% mutate(f = ifelse(x1 & x2, 1, 0))
  colnames(generated) = c("g","x1","x2", "f")
  return(generated)
}

question <- generate(group)

### Part N-2. Function Definitions
equity <- function(x, r){
  equity_output <- x %>% 
    filter(admit == "TRUE" & g == "D") %>%
    count()
  return(as.integer(equity_output) / round(nrow(x) * r))
}

# efficiency based on true f-score not estimated f-score 
efficiency <- function(x){
  eff_output <- x %>% filter(admit == "TRUE")
  return(mean(eff_output$f))
}

mobility <- function(x){
  mobility_output <- x %>% 
    filter(g == "A") %>%
    count()
  return(as.integer(mobility_output) / nrow(x))
}

### Part N-3. Selection Function
# The selection function uses the estimated_f score to admit the top 5/18 of the students 

selection <- function(x, r){
  rows <- sample(nrow(x))
  x <- data.frame(x[rows,])
  
  a <- x %>% 
    arrange(desc(estimated_f)) %>%
    mutate(admit = ifelse(row_number() <= round(nrow(x) * r), "TRUE", "FALSE"))
  
  b <- a %>%
    group_by(g, x1, x2, estimated_x2, f, estimated_f, admit) %>%
    summarise(n = n()) %>%
    arrange(desc(estimated_f)) %>% 
    arrange(!g == "D") %>%
    arrange(!admit == "TRUE")
  
  return(list(a, b))
}

## Part A. Scenarios
### Part A-1. Default Scenario (Scenario A)
default <- function(x){
  x <- x %>% 
    mutate(estimated_x2 = x2) %>% 
    mutate(estimated_f = ifelse(x1 & x2, 1, 0))
  return(x)
}

df <- default(question)
default_output <- selection(df, 5/18)
(default_indiv <- data.frame(default_output[1]))
(default_group <- data.frame(default_output[2]))
(default_equity <- equity(default_indiv, 5/18))
(default_efficiency <- efficiency(default_indiv))

### Part A-2. First Variant (Scenario B)
variant1 <- function(x){
  x <- x %>% mutate(estimated_x2 = ifelse(x1 == 1, 1, 0)) %>%
    mutate(estimated_f = x1 * estimated_x2 + x1 + estimated_x2)
  return(x)
}

fv <- variant1(question)
fv_output <- selection(fv, 5/18)
(fv_indiv <- data.frame(fv_output[1]))
(fv_group <- data.frame(fv_output[2]))
(fv_equity <- equity(fv_indiv, 5/18))
(fv_efficiency <- efficiency(fv_indiv))

### Part A-3. Second Variant (Scenario C)
# We define estimated x2 as the expected value of x2. 
# p(x2=1|g="A") = 2/3
# p(x2=1|g="D") = 1/3

variant2 <- function(x){
  x <- x %>% mutate(estimated_x2 = ifelse(g == "A", 1, 0)) %>%
    mutate(estimated_f = x1 * estimated_x2 + x1 + estimated_x2)
  return(x)
}

sv <- variant2(question)
sv_output <- selection(sv, 5/18)
(sv_indiv <- data.frame(sv_output[1]))
(sv_group <- data.frame(sv_output[2]))
(sv_equity <- equity(sv_indiv, 5/18))
(sv_efficiency <- efficiency(sv_indiv))

### Part A-4. Third Variant (Scenario D)
variant3 <- function(x){
  x <- x %>% 
    mutate(estimated_x2 = ifelse(g == "D", x2, 0)) %>%
    mutate(estimated_f = x1 * estimated_x2 + x1 + estimated_x2)
}

tv <- variant3(question)
tv_output <- selection(tv, 5/18)
(tv_indiv <- data.frame(tv_output[1]))
(tv_group <- data.frame(tv_output[2]))
(tv_equity <- equity(tv_indiv, 5/18))
(tv_efficiency <- efficiency(tv_indiv))

### Part A-5. Box plot
bp.input <- NULL

for(i in 1:1000){
  set.seed(i^2+191)
  question <- generate(group)
  
  df <- default(question)
  default_output <- selection(df, 5/18)
  default_indiv <- data.frame(default_output[1])
  bp.input <- rbind(bp.input, c(1, equity(default_indiv, 5/18), efficiency(default_indiv)))
  
  fv <- variant1(question)
  fv_output <- selection(fv, 5/18)
  fv_indiv <- data.frame(fv_output[1])
  bp.input <- rbind(bp.input, c(2, equity(fv_indiv, 5/18), efficiency(fv_indiv)))
  
  sv <- variant2(question)
  sv_output <- selection(sv, 5/18)
  sv_indiv <- data.frame(sv_output[1])
  bp.input <- rbind(bp.input, c(3, equity(sv_indiv, 5/18), efficiency(sv_indiv)))
  
  tv <- variant3(question)
  tv_output <- selection(tv, 5/18)
  tv_indiv <- data.frame(tv_output[1])
  bp.input <- rbind(bp.input, c(4, equity(tv_indiv, 5/18), efficiency(tv_indiv)))
}

bp.input <- data.frame(bp.input)
colnames(bp.input) <- c("scenario", "equity", "efficiency")
bp.input <- bp.input %>% mutate(scenario = as.factor(scenario))

bp.eff <- bp.input %>% ggplot(aes(x = scenario, y = efficiency)) +
  geom_boxplot() +
  labs(x = "Admissions Method", y = "Efficiency")
bp.eff

# T-test to compare with theoretical calculations for efficiency
t.test(bp.input$efficiency[bp.input$scenario == 1], mu = 1)
t.test(bp.input$efficiency[bp.input$scenario == 2], mu = 5/9)
t.test(bp.input$efficiency[bp.input$scenario == 3], mu = 2/3)
t.test(bp.input$efficiency[bp.input$scenario == 4], mu = 3/5)

t.test(bp.input$efficiency[bp.input$scenario == 4], 
       bp.input$efficiency[bp.input$scenario == 2], 
       var.equal = FALSE, alternative = "less")

bp.eq <- bp.input %>% ggplot(aes(x = scenario, y = equity)) +
  geom_boxplot() +
  labs(x = "Admissions Method", y = "Equity")
bp.eq

# T-test to compare with theoretical calculations for equity
t.test(bp.input$equity[bp.input$scenario == 1], mu = 1/5)
t.test(bp.input$equity[bp.input$scenario == 2], mu = 1/3)
t.test(bp.input$equity[bp.input$scenario == 3], mu = 0)
t.test(bp.input$equity[bp.input$scenario == 4], mu = 2/5)

t.test(bp.input$equity[bp.input$scenario == 4], 
       bp.input$equity[bp.input$scenario == 2], 
       var.equal = FALSE, alternative = "greater")

## Part B
### Part B-1. Define Descendants Function
descendant <- function(x, generation, scenario){
  eff_table <- NULL
  eq_table <- NULL
  mob_table <- NULL
  for(j in 1:generation){
    x <- data.frame(selection(scenario(x), 5/18)[1])
    x$child_temp <- runif(nrow(x))
    for(i in 1:nrow(x)){
      if(x$g[i] == "A" & x$admit[i] != "TRUE"){
        x$child_g[i] = "A"}
      else if (x$g[i] == "A" & x$admit[i] != "FALSE"){
        if(x$child_temp[i] > 1/4){x$child_g[i] = "A"}
        else{x$child_g[i] = "D"}}
      else if (x$g[i] == "D" & x$admit[i] != "TRUE"){
        if(x$child_temp[i] > 1/2){x$child_g[i] = "A"}
        else{x$child_g[i] = "D"}}
      else if (x$g[i] == "D" & x$admit[i] != "FALSE"){
        if(x$child_temp[i] > 16/16){x$child_g[i] = "A"}
        else{x$child_g[i] = "D"}}
    }
    eff <- efficiency(x)
    eq <- equity(x, 5/18)
    mob <- mobility(x)
    
    eff_table[j] = eff
    eq_table[j] = eq
    mob_table[j] = mob
    
    x <- generate(x$child_g)
  }
  return(list(eff_table, eq_table, mob_table))
}

### Part B-2. Four Scenarios for 100 Generations
hundred.default <- descendant(question, 100, default)
hundred.var1 <- descendant(question, 100, variant1)
hundred.var2 <- descendant(question, 100, variant2)
hundred.var3 <- descendant(question, 100, variant3)

### Part B-3. Efficiency Plot
df.eff <- data.frame(hundred.default[1])
var1.eff <- data.frame(hundred.var1[1])
var2.eff <- data.frame(hundred.var2[1])
var3.eff <- data.frame(hundred.var3[1])

df.eff <- df.eff %>% mutate(generation = row_number()) %>%
  mutate(method = "method1")
colnames(df.eff) = c("efficiency", "generation", "method")
var1.eff <- var1.eff %>% mutate(generation = row_number())  %>%
  mutate(method = "method2")
colnames(var1.eff) = c("efficiency", "generation", "method")
var2.eff <- var2.eff %>% mutate(generation = row_number()) %>%
  mutate(method = "method3")
colnames(var2.eff) = c("efficiency", "generation", "method")
var3.eff <- var3.eff %>% mutate(generation = row_number()) %>%
  mutate(method = "method4")
colnames(var3.eff) = c("efficiency", "generation", "method")

eff.comparison <- rbind(df.eff, var1.eff, var2.eff, var3.eff)

eff.plot <- eff.comparison %>% ggplot(aes(x = generation, y = efficiency)) + 
  geom_line() +
  stat_smooth(method = "glm") + 
  stat_regline_equation(label.y = 0.1, label.x = 80) + 
  labs(x = "Generation (1 ~ 100)", y = "Efficiency (Average F-Score of Admitted Students)") + 
  facet_grid(method ~.) + 
  theme_bw()
eff.plot

### Part B-4. Equity Plot
df.eq <- data.frame(hundred.default[2])
var1.eq <- data.frame(hundred.var1[2])
var2.eq <- data.frame(hundred.var2[2])
var3.eq <- data.frame(hundred.var3[2])

df.eq <- df.eq %>% mutate(generation = row_number()) %>%
  mutate(method = "method1")
colnames(df.eq) = c("equity", "generation", "method")
var1.eq <- var1.eq %>% mutate(generation = row_number())  %>%
  mutate(method = "method2")
colnames(var1.eq) = c("equity", "generation", "method")
var2.eq <- var2.eq %>% mutate(generation = row_number()) %>%
  mutate(method = "method3")
colnames(var2.eq) = c("equity", "generation", "method")
var3.eq <- var3.eq %>% mutate(generation = row_number()) %>%
  mutate(method = "method4")
colnames(var3.eq) = c("equity", "generation", "method")

eq.comparison <- rbind(df.eq, var1.eq, var2.eq, var3.eq)

eq.plot <- eq.comparison %>% ggplot(aes(x = generation, y = equity)) + 
  geom_line() +
  stat_smooth(method = "glm") + 
  stat_regline_equation(label.y = 0.7, label.x = 80) + 
  labs(x = "Generation (1 ~ 100)", y = "Equity (Proportion of Disadvantaged Admits)") + 
  facet_grid(method ~.) + 
  theme_bw()
eq.plot

### Part B-5. Mobility Plot
df.mob <- data.frame(hundred.default[3])
var1.mob <- data.frame(hundred.var1[3])
var2.mob <- data.frame(hundred.var2[3])
var3.mob <- data.frame(hundred.var3[3])

df.mob <- df.mob %>% mutate(generation = row_number()) %>%
  mutate(method = "method1")
colnames(df.mob) = c("mobility", "generation", "method")
var1.mob <- var1.mob %>% mutate(generation = row_number())  %>%
  mutate(method = "method2")
colnames(var1.mob) = c("mobility", "generation", "method")
var2.mob <- var2.mob %>% mutate(generation = row_number()) %>%
  mutate(method = "method3")
colnames(var2.mob) = c("mobility", "generation", "method")
var3.mob <- var3.mob %>% mutate(generation = row_number()) %>%
  mutate(method = "method4")
colnames(var3.mob) = c("mobility", "generation", "method")

mob.comparison <- rbind(df.mob, var1.mob, var2.mob, var3.mob)

mob.plot <- mob.comparison %>% ggplot(aes(x = generation, y = mobility)) + 
  geom_line() +
  stat_smooth(method = "glm") + 
  stat_regline_equation(label.y = 0.15, label.x = 80) + 
  labs(x = "Generation (1 ~ 100)", y = "Mobility (Proportion of Advantaged Group)") + 
  facet_grid(method ~.) + 
  theme_bw()
mob.plot




