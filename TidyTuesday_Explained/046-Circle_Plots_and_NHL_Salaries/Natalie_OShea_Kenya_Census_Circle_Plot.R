#### setup ####

# load libraries
library(tidyverse)
library(networkD3)
library(igraph) ## Added
library(ggraph) ## Added


# remotes::install_github("Shelmith-Kariuki/rKenyaCensus")
# load and wrangle data
demographics <- rKenyaCensus::V4_T2.31 %>%
  rename("ethnicity" = "ETHNICITY/NATIONALITY",
         "subgroup" = "SUBGROUP",
         "total" = "TOTAL") %>%
  mutate(name = NA,
         ethnicity = str_to_lower(ethnicity),
         subgroup = str_to_lower(subgroup))

# save only subgroup totals
totals <- demographics %>%
  slice(1:4)
kenyans <- demographics %>%
  slice(6:133)
non_kenyans <- demographics %>%
  slice(135:148)

#### prep plotting dataframe ####

totals_full <- totals %>%
  mutate(from = "census",
         to = ifelse(ethnicity == "total population",
                     "census",
                     paste0("census.", str_remove_all(ethnicity, " "))),
         name = ifelse(ethnicity == "non kenyans",
                       "Non-Kenyans",
                       NA))

kenyans_full <- kenyans %>%
  mutate(from = ifelse(subgroup == "",
                       "census.kenyans",
                       paste0("census.kenyans.", 
                              str_remove_all(ethnicity, "[^[:alnum:]]"))),
         to = ifelse(subgroup == "",
                     paste0("census.kenyans.", 
                            # remove non-alphanumeric characters
                            str_remove_all(ethnicity, "[^[:alnum:]]")),
                     paste0("census.kenyans.", 
                            str_remove_all(ethnicity, "[^[:alnum:]]"),
                            ".",
                            str_remove_all(subgroup, "[^[:alnum:]]"))),
         # only provide names for main ethnicities with pop >250k
         name = ifelse(subgroup == "" & total > 250000, 
                       str_to_title(ethnicity), 
                       NA))

non_kenyans_full <- non_kenyans %>%
  mutate(from = ifelse(subgroup == "",
                       "census.nonkenyans",
                       paste0("census.nonkenyans.", 
                              str_remove_all(ethnicity, "[^[:alnum:]]"))),
         to = ifelse(subgroup == "",
                     paste0("census.nonkenyans.", 
                            # remove non-alphanumeric characters
                            str_remove_all(ethnicity, "[^[:alnum:]]")),
                     paste0("census.nonkenyans.", 
                            str_remove_all(ethnicity, "[^[:alnum:]]"),
                            ".",
                            str_remove_all(subgroup, "[^[:alnum:]]"))))

full_df <- totals_full %>%
  bind_rows(kenyans_full, non_kenyans_full)

edges <- full_df %>%
  filter(ethnicity != "total population") %>%
  select(from, to)

vertices <- full_df %>%
  select(to, total, name) %>%
  rename("name" = "to",
         "size" = "total",
         "shortName" = "name")

mygraph <- graph_from_data_frame(edges, vertices = vertices )

#### generate plot ####

ggraph(mygraph, layout = 'circlepack', weight = size) + 
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth))) +
  geom_node_label(aes(label = shortName), size = 5, fill = "#DDDAD9", color = "#04100A", family = "Inter-Medium") +
  scale_fill_manual(values = c("0" = "#BABABA", 
                               "1" = "#0C331F", 
                               "2" = "#C04245", 
                               "3" = "#76A58E")) +
  scale_color_manual(values = c("0" = "#BABABA", 
                                "1" = "#04100A", 
                                "2" = "#04100A", 
                                "3" = "#04100A")) +
  labs(title = "Distribution of Kenyan Population by Ethnicity/Nationality",
       subtitle = "Kenya is home to a large and ethnically diverse population of more than 47.5 million\npeople. The largest ethnic groups in Kenya include the Kikuyu (17%), Kalenjin (14%),\nand Luhya (13%). Non-Kenyans make up less than 1% of Kenya's population.",
       caption = "Data: 2019 Kenya Population and Housing Census via rKenyaCensus | Viz: @_natalie_oshea") +
  theme_void() + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#BABABA", color = "#BABABA"),
        plot.title = element_text(hjust = 0.5, size = 19, color = "#04100A", family = "Inter-Medium"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#04100A", family = "Inter-Light",
                                     lineheight = 1.2, margin = margin(0.8, 0, -0.8, 0, unit = "line")),
        plot.caption = element_text(hjust = 0.5, size = 12, color = "#04100A"))
