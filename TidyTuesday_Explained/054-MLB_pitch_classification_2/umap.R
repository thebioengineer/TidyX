
umap_pitch <- train_cleaned %>% 
  select(-pitch_type) %>% 
  as.matrix %>% 
  umap::umap()

pitch_vect <- c(FF = "four-seam fastball",
SL = "slider",
CH = "change up",
FT = "two-seam fastball",
SI = "sinker",
CU = "curveball",
FC = "cutter",
KC = "knuckle-curve",
FS = "split-finger fastball")


umap_pitch$layout %>% 
  data.frame %>% 
  setNames(c("umap1","umap2")) %>% 
  bind_cols(umap_pitch$data) %>% 
  mutate(
    pitch_type = pitch_vect[train_cleaned$pitch_type]
  ) %>% 
  ggplot(aes(x = umap1, y = umap2)) + 
  geom_point(aes( color = pitch_type), alpha = .5) -> umap_plot

ggplotly(umap_plot)
