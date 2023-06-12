db_data <- read_csv("02_Clean_data/dbenv_use.csv")

#### Latency ####

test <- db_data %>% 
  select(date, block_id, burn_season, env_type, latency) %>% 
  drop_na()

table(test$burn_season) 
## Feasible to do Cohen's d with Fall, Summer, and Winter, but not Spring. 
## When reducing ds to pairs, 21 total observations, 2 for Fall, 1 for Spring.

#### Removal Number ####
x <- db_data %>% 
  select(date, block_id, burn_season, env_type, rem_no) %>% 
  filter(rem_no > 0)

table(x$burn_season)
table(x$env_type)

## Could do log ratio or Cohen's d. 

## Need an effect size for probability
