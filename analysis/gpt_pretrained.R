library(tidyverse)
library(httr2)
library(glue)

# Read in training data ---------------------------------------------------

# NOTE: Pre-trained models take 2,049 tokens. The prompt takes up 32. All
# articles longer than 2,017 tokens have been removed.

text_df <- read_rds(here::here("data", "text_clean.rds")) |>
  filter(est_n_tokens < 2018)

# GPT 3.5 turbo -----------------------------------------------------------

gpt_turbo_identify_protest <- function(id, body, label) {

  prompt_raw <- "Identify with 'yes' or 'no' whether the following article (delimited in XML tags) mentions a protest, riot, demonstration, or march: <article>{body}</article>"

  prompt <- glue(prompt_raw)

  req <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY_NSF"))) |>
    req_body_json(
      list(
        "model" = "gpt-3.5-turbo-0613",
        "messages" = list(
          list(
            "role" = "system",
            "content" = "You are a helpful assistant."
          ),
          list(
            "role" = "user",
            "content" = prompt
          )
        ),
        "temperature" = 0,
        "max_tokens" = 1
      )
    ) |>
    req_retry(max_tries = 3)

  resp <- req_perform(req)

  pred <- resp_body_json(resp)$choices[[1]]$message$content

  df <- tibble(
    id = id,
    body = body,
    label = label,
    gpt_pred = pred,
    prompt = prompt_raw
  )

  write_csv(df, here::here("data", "gpt_turbo_predictions.csv"), append = T)

}

map(1:nrow(text_df),
    ~ gpt_turbo_identify_protest(text_df$id[.x], text_df$body[.x], text_df$protest[.x]),
    .progress = T)

# Evaluate model results

labelled_df <- read_csv(here::here("data", "gpt_turbo_predictions.csv")) |>
  mutate(gpt_pred = str_remove(gpt_pred, "\\."),
         label = if_else(label == 1, "Yes", "No"),
         across(label:gpt_pred, factor))

caret::confusionMatrix(
  labelled_df$gpt_pred,
  labelled_df$label,
  positive = "Yes"
)

# Davinci -----------------------------------------------------------------

gpt_davinci_identify_protests <- function(id, body, protest) {

  prompt <- glue("Identify with 'yes' or 'no' whether the following article (delimited in XML tags) mentions a protest event: <article>{body}</article>")

  req <- request("https://api.openai.com/v1/completions") |>
    req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY_AMAR"))) |>
    req_body_json(
      list(
        "model" = "text-davinci-003",
        "prompt" = prompt,
        "max_tokens" = 3,
        "temperature" = 0
      )
    ) |>
    req_retry(max_tries = 3)

  resp <- req_perform(req)

  pred <- str_remove_all(resp_body_json(resp)$choices[[1]]$text, "\\n")

  df <- tibble(
    id = id,
    body = body,
    label = protest,
    gpt_pred = pred
  )

  write_csv(df, here::here("data", "gpt_davinci_predictions.csv"), append = T)

}

map(1:nrow(text_df),
    ~ gpt_davinci_identify_protests(text_df$id[.x], text_df$body[.x], text_df$protest[.x]),
    .progress = T)

# Ada ---------------------------------------------------------------------

gpt_ada_identify_protests <- function(id, body, protest) {

  prompt <- glue("Identify with 'yes' or 'no' whether the following article (delimited in XML tags) mentions a protest event: <article>{body}</article>")

  req <- request("https://api.openai.com/v1/completions") |>
    req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY_AMAR"))) |>
    req_body_json(
      list(
        "model" = "text-ada-001",
        "prompt" = prompt,
        "max_tokens" = 3,
        "temperature" = 0
      )
    ) |>
    req_retry(max_tries = 3)

  resp <- req_perform(req)

  pred <- str_trim(str_remove_all(resp_body_json(resp)$choices[[1]]$text, "\\n"))

  df <- tibble(
    id = id,
    body = body,
    label = protest,
    gpt_pred = pred
  )

  write_csv(df, here::here("data", "gpt_ada_predictions.csv"), append = T)

}

map(1:nrow(text_df),
    ~ gpt_ada_identify_protests(text_df$id[.x], text_df$body[.x], text_df$protest[.x]),
    .progress = T)
