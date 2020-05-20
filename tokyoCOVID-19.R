library(gsheet)
library(tidyverse)

# import tokyo 23 ku
url <- "https://docs.google.com/spreadsheets/d/1ZcWXLD4fAkBgJcExeYYUzbgo8qBxwSYo-BKWBPkhBw0/edit#gid=226081036"
tokyo23 <- read_csv(construct_download_url(url)) %>% na.omit()

# longer table
t23 <- tokyo23 %>% pivot_longer(-1, names_to = "region", values_to = "case")

# daily difference
t23.diff <- t23 %>% group_by(region) %>% 
  mutate(diff = case - lag(case, 1)) %>% 
  mutate(diff = ifelse(is.na(diff), case, diff)) %>% ungroup()
# A single line to calculate daily difference
# t23.diff <- t23 %>% group_by(region) %>% mutate(diff = ifelse(is.na(case - lag(case, 1)), case, case - lag(case, 1))) # difficult to read

tokyo23.diff <- t23.diff %>% select(-case)%>% pivot_wider(names_from = "region", values_from = "diff")

# plot for cum
par(mfrow = c(6, 11))
plot(1, type = "n") & text(1, 1, paste0("累積\n", max(tokyo23$date)), cex=2)
for (i in 2:66) {tokyo23[[i]] %>% plot(type = "l", main = names(tokyo23)[i])}

# plot for diff
par(mfrow = c(6, 11))
plot(1, type = "n") & text(1, 1, paste0("新規\n", max(tokyo23$date)), cex=2)
for (i in 2:66) {tokyo23.diff[[i]] %>% plot(type = "l", main = names(tokyo23.diff)[i])}
