## TidyX Episode 179 - Too many SpaghettiOs!

## How many cans of spaghettiOs are needed to write the Lord of the Rings?
## Instagram Reel: https://www.instagram.com/p/C6hUeRVp24H/

library(tidyverse)

## use Gemini perform OCR for us and read in Tylers spaghettiO count
## Uploaded to gemini.google.com
## Prompt: Please convert this into an R tibble for me
# OUTPUT:
# data <- tibble(
#   Letter = c("a", "b", "C", "d", "e", "f", "g", "h", "i", "j", "k", "m", "n", "", "0", "P", "r", "S", "", "t", "u", "V", "W", "X", "y", "Z"),
#   `% total` = c(2.28, 2.37, 2.45, 2.2, 2.71, 2.62, 2.37, 2.2, 2.37, 2.54, 3.3, 2.71, 2.28, 2.12, 37.3, 2.96, 2.2, 3.13, 2.54, 2.62, 2.37, 2.12, 2.62, 2.37, 2.2, 3.05),
#   Times = c(27, 28, 29, 26, 32, 31, 28, 26, 28, 30, 39, 32, 27, 25, 441, 35, 26, 37, 30, 31, 28, 25, 31, 28, 26, 36)
# )

## Reviewed as was missing "l" and had a blank after "n" and "s"

tyler_can_count_df <- tibble(
  letter = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "l", "k", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"),
  `pct_total` = c(2.28, 2.37, 2.45, 2.2, 2.71, 2.62, 2.37, 2.2, 2.37, 2.54, 3.3, 2.71, 2.28, 2.12, 37.3, 2.96, 2.2, 3.13, 2.54, 2.62, 2.37, 2.12, 2.62, 2.37, 2.2, 3.05),
  `n` = c(27, 28, 29, 26, 32, 31, 28, 26, 28, 30, 39, 32, 27, 25, 441, 35, 26, 37, 30, 31, 28, 25, 31, 28, 26, 36)
)

print(tyler_can_count_df)

## did the same thing for the LOTR data.frame Tyler showed. It struggles a bit with some
## values, so I manually reviewed and corrected some values

tyler_lotr_count_df <- tibble(
  letter = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"),
  `pct_total` = c( 8.34, 1.62, 1.64, 5.21, 12.4, 2.48, 2.45, 6.57, 6.24, 0.0506, 0.887, 4.41, 2.44, 7.02, 7.85, 1.29, 0.0539, 6.07, 6.08, 8.86, 2.59, 0.801, 2.65, 0.0555, 1.87, 0.0442),
  n = c(188541, 36626, 37183, 117858, 281428, 56138, 55505, 148537, 140997, 1143, 20055, 99603, 55075, 158776, 177409, 29115, 1219, 137327, 137400, 200233, 58451, 18115, 59893, 1254, 42201, 999)
)

print(tyler_lotr_count_df)

## confirm it matches Tylers video
sum(tyler_lotr_count_df$n)


## Derive N cans of Spaggettio to book

calc_spagettios <- function(can_count_df, book_count_df){
  
  can_and_book_counts_df <- left_join(
    can_count_df, 
    book_count_df, 
    by = "letter",
    suffix = c("_can","_book")
  )
  
  n_cans_needed <- can_and_book_counts_df %>% 
    mutate(
      n_times = n_book/n_can
    ) %>% 
    mutate(
      n_cans = ceiling(n_times)
    ) %>% 
    pull(n_cans) %>% 
    max(na.rm = TRUE)
  
  leftover_noods <- can_and_book_counts_df %>% 
    mutate(
      n_noodles = n_can * n_cans_needed
    ) %>% 
    mutate(
      n_leftover = n_noodles - n_book
    ) %>% 
    select(letter, n_leftover)
  
  ## Tyler doesn't show cost/can in his code, but...
  ## in his printout its 12225.05 for 8795 cans, so...
  ## $1.39
  
  total_can_cost <- 1.39 * n_cans_needed
  total_can_calories <- 200 * n_cans_needed
  total_leftover_noods <- sum(leftover_noods$n_leftover, na.rm = TRUE)
  
  
  output_string <- paste(
    paste0("Total cans needed: ", scales::number(n_cans_needed, big.mark = ",")),
    paste0("Total cost: ", scales::dollar(total_can_cost)),
    paste0("Total calories: ",  scales::number(total_can_calories, big.mark = ",")),
    "",
    paste0("Total leftover noodles: ", scales::number(total_leftover_noods, big.mark = ",")),
    sep = "\n"
    )
  
  cat(output_string)
  
  ## Invisibly return values so you can store them if you want later
  invisible(
    list(
      n_cans = n_cans_needed, 
      cost = total_can_cost,
      calories = total_can_calories,
      counts_df = can_and_book_counts_df,
      leftover_noodles = leftover_noods
    )
  )

}

## Reproducing Tylers Outputs

calc_spagettios(tyler_can_count_df, tyler_lotr_count_df)


## calculate count_df for book text

book_letter_counts <- function(book_text){
  
  book_text %>% 
    tolower() %>% 
    stringr::str_replace_all("[^a-z]","") %>% 
    paste(collapse = "") %>% 
    str_split("") %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame() %>% 
    set_names(c("letter","n"))
  
}

book_as_spaghettiOs <- function(book_file){
  
  book_count_df <- readLines(book_file,warn = FALSE) %>% 
    book_letter_counts()
  
  cat("Letter Counts:\n\n")
  
  print(book_count_df)
  
  cat("\n")
  
  cat("Total Letter Count: ", sum(book_count_df$n))
  
  cat("\n\n")
  
  spaghettiO_can_count_df <- tibble(
    letter = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "l", "k", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"),
    `pct_total` = c(2.28, 2.37, 2.45, 2.2, 2.71, 2.62, 2.37, 2.2, 2.37, 2.54, 3.3, 2.71, 2.28, 2.12, 37.3, 2.96, 2.2, 3.13, 2.54, 2.62, 2.37, 2.12, 2.62, 2.37, 2.2, 3.05),
    `n` = c(27, 28, 29, 26, 32, 31, 28, 26, 28, 30, 39, 32, 27, 25, 441, 35, 26, 37, 30, 31, 28, 25, 31, 28, 26, 36)
  )
  
  results <- calc_spagettios(spaghettiO_can_count_df, book_count_df)
  
  invisible(results)
  
}

## Romeo and Juliet
book_as_spaghettiOs("https://folger-main-site-assets.s3.amazonaws.com/uploads/2022/11/romeo-and-juliet_TXT_FolgerShakespeare.txt")

## The Odyssey by Homer
book_as_spaghettiOs("https://classics.mit.edu/Homer/odyssey.mb.txt")


