
library(rmarkdown)

generate_report <- function(values,file){
  
  #Create temporary file
  receiver_report <- tempfile(".md")
  
  ## Create holder for plot and table
  plot_rds <- tempfile(fileext = ".png")
  table_rds <- tempfile(fileext = ".rds")
  
  #save RDS  
  ggplot2::ggsave(values$waffle,filename = plot_rds,height = 5, width = 7, units = "in")
  saveRDS(values$table, table_rds)
  
  ## generate contents
  header <- c(
    "---",
    paste0("title: 2019 Wide Receiver Distributions for ", toupper(values$tm)),
    "output: pdf_document",
    "---")
  
  body <- c(
    "## Background",
    paste("Data for this analysis was scraped from",
          "pro-football-reference.com.",
          "This report shows the targets for the top 4 players",
          "with the rest of the receivers lumped together.",
          "Additionally, there is a table showing each players",
          "statistics."),
    "",
    "## Waffle Plot ",
    "",
    paste(c("![](",normalizePath(plot_rds, winslash = "/"),")"), collapse=""),
    "",
    "## Stats ",
    "",
    as.character(knitr::kable(readRDS(table_rds))),
    ""
  )
  
  writeLines(c(header,body),receiver_report,sep = "\n")
  
  ## render to pdf
  rmarkdown::render(receiver_report, file, output_format = pdf_document())
  
  unlink(receiver_report)
  unlink(plot_rds)
  unlink(table_rds)
}

