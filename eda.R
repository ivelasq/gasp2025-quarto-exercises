library(readr)
library(dplyr)

dat <- read_csv(here::here("data", "nation.csv"), skip = 1)

# DAM LOCATIONS
dat |> 
  count(`Source Agency`) |> 
  arrange(desc(n)) |> 
  slice(1:10)

# TOP 10 DAM PURPOSES
dat |> 
  count(`Primary Purpose`) |> 
  arrange(desc(n)) |> 
  slice(1:10) |>
  print()

# DAM HAZARD CLASSIFICATIONS
dat |> 
  count(`Hazard Potential Classification`) |> 
  arrange(desc(n)) |>
  print()

# OLDEST vs NEWEST DAMS
dat |> 
  filter(!is.na(`Year Completed`)) |> 
  summarise(
    oldest = min(`Year Completed`, na.rm = TRUE),
    newest = max(`Year Completed`, na.rm = TRUE),
    median_year = median(`Year Completed`, na.rm = TRUE)
  ) |>
  print()

# CONSTRUCTION BY DECADE
dat |> 
  filter(!is.na(`Year Completed`)) |> 
  mutate(decade = floor(`Year Completed` / 10) * 10) |> 
  count(decade) |> 
  arrange(desc(n)) |> 
  slice(1:10) |>
  print()

# TALLEST DAMS
dat |> 
  select(`Dam Name`, State, `Dam Height (Ft)`) |> 
  filter(!is.na(`Dam Height (Ft)`)) |> 
  arrange(desc(`Dam Height (Ft)`)) |> 
  slice(1:10) |>
  print()

# DAM CONDITIONS
dat |> 
  count(`Condition Assessment`) |> 
  arrange(desc(n)) |>
  print()

# FEDERAL vs STATE vs PRIVATE OWNERSHIP
dat |> 
  count(`Primary Owner Type`) |> 
  arrange(desc(n)) |> 
  slice(1:10) |>
  print()

# Year 0 dams - let's see what these are
dat |> 
  filter(`Year Completed` == 0) |> 
  select(`Dam Name`, State, `Primary Purpose`, `Year Completed`) |> 
  slice(1:5) |>
  print()

# STATES WITH HIGHEST HAZARD DAMS
dat |> 
  filter(`Hazard Potential Classification` == "High") |> 
  count(State) |> 
  arrange(desc(n)) |> 
  slice(1:10) |>
  print()

# AVERAGE DAM HEIGHT BY STATE (top 10)
dat |> 
  filter(!is.na(`Dam Height (Ft)`), `Dam Height (Ft)` > 0) |> 
  group_by(State) |> 
  summarise(
    avg_height = round(mean(`Dam Height (Ft)`), 1),
    count = n()
  ) |> 
  filter(count >= 50) |>
  arrange(desc(avg_height)) |> 
  slice(1:10) |>
  print()

# MINING WASTE DAMS (Tailings)
dat |> 
  filter(`Primary Purpose` == "Tailings") |> 
  count(State) |> 
  arrange(desc(n)) |> 
  slice(1:10) |>
  print()

# HYDROELECTRIC CAPACITY BY STATE
dat |> 
  filter(`Primary Purpose` == "Hydroelectric") |> 
  count(State) |> 
  arrange(desc(n)) |> 
  slice(1:10) |>
  print()