library(tidyverse)
library(textcat)
library(janitor)

# Read in raw text data ---------------------------------------------------

text_raw <- rio::import(here::here("data-raw", "final_merged_data_master.RData")) |>
  clean_names() |>
  select(id, title, body, protest)

tabyl(text_raw, protest)

# Stop phrases ------------------------------------------------------------

stop_phrases <- c(
  "All rights Reserved",
  "Visit our website",
  "Like,Share,Subscribe,Recommend our Facebook Page",
  "By using this website, you accept the terms of our Visitor Agreement and Privacy Policy, and understand your options regarding Ad Choices.",
  "Manage Cookie Preferences | Do Not Sell My Information",
  "Learn about careers at Cox Media Group.",
  "This station is part of Cox Media Group Television.",
  "Follow Al Jazeera English",
  "NBC UNIVERSAL",
  "Support Scroll.in Support Scroll.in Your support is crucial: India needs independent media and independent media needs you.",
  "Loading Comments...",
  "Subscribe To Our Daily Newsletter And Get News Delivered Straight To Your Inbox",
  "Your email address will not be published.",
  "It is a priority for CBC to create a website that is accessible to all Canadians including people with visual, hearing, motor and cognitive challenges. Closed Captioning and Described Video is available for many CBC shows offered on CBC Gem.",
  "Comments are welcome while open.",
  "We reserve the right to close comments at any time.",
  "Join the conversation",
  "Create account",
  "Already have an account?"
)

# Clean raw text data -----------------------------------------------------

text_df <- text_raw |>
  mutate(
    body = str_remove_all(body, regex(paste(stop_phrases, collapse = "|"), ignore_case = T)),
    body = str_trim(body),
    est_n_tokens = nchar(body) / 4,
    lang = textcat(body)
  ) |>
  filter(
    # Filter out articles missing bodies
    est_n_tokens > 50,
    # Only keep English language articles
    lang == "english"
  ) |>
  drop_na(body, protest)

tabyl(text_df, protest)

# Save cleaned data for model ---------------------------------------------

write_rds(text_df, here::here("data", "text_clean.rds"))
