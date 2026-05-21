jung <- read.csv("jungheinrich.csv", stringsAsFactors = FALSE)
library("tidyverse")
require("tidyverse")
# Practical sonuçlarını karakter yap
jung$PRACTICAL.EXAM.RESULTS <- as.character(jung$PRACTICAL.EXAM.RESULTS)

noneprufung <- data.frame()

i <- 1
n <- nrow(jung)

while (i <= n) {
  
  pratik_raw <- jung$PRACTICAL.EXAM.RESULTS[i]
  
  # integer'a çevirmeyi dene
  pratik_int <- suppressWarnings(as.integer(pratik_raw))
  
  # Eğer integer değilse → tüm satırı noneprufung listesine ekle
  if (is.na(pratik_int)) {
    noneprufung <- rbind(noneprufung, jung[i, ])
  }
  
  i <- i + 1
}
view(jung_clean)
noneprufung


# noneprufung içindeki satırların indexlerini bul
sil_index <- as.numeric(rownames(noneprufung))

# jung listesinden bu satırları sil
jung_clean <- jung[-sil_index, ]


jung_clean$AREA <- ifelse(
  grepl("^Avrupa 2", jung_clean$AREA, ignore.case = TRUE),
  "Avrupa 2",
  jung_clean$AREA)

library(tidyverse)

jung_clean <- jung_clean %>% 
  mutate(across(
    where(is.character),
    ~ str_to_title(.)
  ))


jung_clean <- jung_clean %>% 
  mutate(
    PRACTICAL.EXAM.RESULTS = as.numeric(PRACTICAL.EXAM.RESULTS),
    RESULT = if_else(PRACTICAL.EXAM.RESULTS >= 66, "Passed", "Failed")
  )
library(tidyverse)

jung_clean <- jung_clean %>% 
  arrange(as.Date(TRAINING.DATE, format = "%d-%m-%y")) %>% 
  mutate(ID = row_number()) %>% 
  relocate(ID, .before = 1)

view(jung_clean)
