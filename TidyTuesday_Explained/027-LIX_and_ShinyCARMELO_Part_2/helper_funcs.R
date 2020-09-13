## Convert wide to long, specific values

table_summarized_values <- function(.data, ...){
  .data %>%
    select(...) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    ) %>% 
    mutate(
      stat = gsub("(.*_.*)_.*$", "\\1", variable),
      type = gsub(".*_.*_(.*)$", "\\1", variable)
    ) %>%
    pivot_wider(
      id_cols = stat,
      names_from = type,
      values_from = value
    )
}


table_point_plots <- function(.data, datatype = NULL){
  
  dat_rows <- nrow(.data)
  
  plt <- .data %>%
    mutate(
      stat_pos = as.numeric(stat)
    ) %>% 
    ggplot() + 
    theme_void() +
    theme(legend.position = "none") +
    geom_text(
      aes(
        x =.1,
        y = stat_pos,
        label = stat
      ),
      hjust = 0,
      size = 8
    ) +
    geom_text(
      aes(
        x = 1.9,
        y = stat_pos,
        label = value
      ),
      hjust = 1,
      size = 8
    ) + 
    
    ## add table lines - vertical
    annotate("segment", 
             x = c( 0, 1, 2, 2.25, 6.25), 
             xend = c( 0, 1, 2, 2.25, 6.25),
             y = .5, yend = dat_rows+.5, size = 0.3) +
    
    ## add table lines - horizontal
    annotate("segment",
             y = seq(.5,dat_rows+.5, by = 1),
             yend = seq(.5,dat_rows+.5, by = 1),
             x = c(0), 
             xend = c(2),
             size = 1) +
    annotate("segment",
             y = dat_rows+.5,
             yend = dat_rows+.5,
             x = c(0), 
             xend = c(2),
             size = 2) +
    annotate("segment",
             y = seq(.5 ,dat_rows+.5, by = 1),
             yend = seq(.5, dat_rows+.5, by = 1),
             x = c(2.25), 
             xend = c(6.25),
             size = 1) +
    annotate("segment",
             y = dat_rows+.5,
             yend = dat_rows+.5,
             x = c(2.25), 
             xend = c(6.25),
             size = 2) +
    
    ## plot vertical lines
    annotate("segment", 
             x = c( 3.25, 5.25), 
             xend = c( 3.25, 5.25),
             y = .5, yend = dat_rows+.5, size = 0.2, color = "grey") +
    annotate("segment", 
             x = c( 4.25), 
             xend = c(4.25 ),
             y = .5, yend = dat_rows+.5, size = 2, color = "grey") +
    annotate("text", 
             x = c( 4.25), 
             y = dat_rows+.7, 
             label = "50%",
             size = 5, color = "grey") +
    
    ## plot percentiles
    geom_point(
      aes(
        x = (pctl / 25) + 2.25,
        y = stat_pos,
        fill = color,
        size = 10,
      ),
      color = "black",
      shape = 21
    ) +
    
    scale_fill_manual(
      breaks = c("Bad","Average","Good"),
      values = c("red","white","blue")
    )
    
  
  if (!is.null(datatype)) {
    plt <- plt +
      geom_text(
        data = data.frame(
          lab = datatype,
          x = c(0),
          y = c(dat_rows + 1)
        ),
        aes(x = x,
            y = y,
            label = lab),
        hjust = 0,
        size = 10
      )
  }
  
  return(plt)
}