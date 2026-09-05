library(tidyverse)
library(tidycensus)

pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

pa_income

dim(pa_income)
glimpse(pa_income)
head(pa_income)

pa_income$GEOID

filter(pa_income, estimate > 60000)
select(pa_income, NAME, estimate, moe)
pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)
arrange(pa_income, desc(moe_pct))

pa_income %>%
  filter(moe_pct > 5) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, estimate, moe, moe_pct)

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate))

pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE        ~ "Low confidence"
  ))

count(pa_income, reliability)

pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)

pa_two

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)

pa_wide


pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)
