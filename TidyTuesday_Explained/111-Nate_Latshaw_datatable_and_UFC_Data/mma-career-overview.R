library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(RColorBrewer)
library(ggdark)

#--------------
# READ IN DATA
#--------------

# read in MMA careers of select fighters
all_careers_df <- fread('Guest_Explainers/Nate_Latshaw/fighter-mma-careers.csv')

unique(all_careers_df$FighterName)

# select a fighter & subset data
fighter_name <- 'Dustin Poirier'
career_df <- all_careers_df[FighterName == fighter_name]

#-----------------
# DATA PROCESSING
#-----------------

# create column for the outcome of each fight

career_df[
  grepl('Unanimous', Decision) & !grepl('Draw', Decision), 
  Result_clean := 'Unanimous Decision'
  ]

career_df[grepl('Split|Majority', Decision) & !grepl('Draw', Decision), Result_clean := 'Split Decision']
career_df[is.na(Result_clean) & grepl('Decision', Decision) & !grepl('Draw', Decision), Result_clean := 'Decision']
career_df[grepl('KO|Tko', Decision), Result_clean := 'Knockout']
career_df[grepl('Sub|Sumission', Decision) & !grepl('TKO', Decision), Result_clean := 'Submission']
career_df[grepl('DQ|Disqualification', Decision), Result_clean := 'DQ']
career_df[is.na(Result_clean) & grepl('Stoppage', Decision) & FighterWL != 'D', Result_clean := 'Knockout']
career_df[FighterWL == 'D', Result_clean := 'Draw']
career_df[is.na(Result_clean) & (grepl('No Contest|Could Not Continue|Overturned', Decision) | FighterWL == 'NC'), Result_clean := 'No Contest']
career_df[is.na(Result_clean), Result_clean := 'Other']

# remove no contests
career_df <- career_df[Result_clean != 'No Contest']

# aggregate career statistics
agg_career_df <- career_df[, .(Fighter = unique(FighterName), 
                               Fights = .N, 
                               Wins = sum(FighterWL == 'W'), 
                               Losses = sum(FighterWL == 'L'), 
                               Draws = sum(Result_clean == 'Draw'), 
                               Win_Percent = sum(FighterWL == 'W') / .N, 
                               KO_Percent = sum(FighterWL == 'W' & Result_clean == 'Knockout') / sum(FighterWL == 'W'), 
                               KOed_Percent = sum(FighterWL == 'L' & Result_clean == 'Knockout') / sum(FighterWL == 'L'), 
                               SUB_Percent = sum(FighterWL == 'W' & Result_clean == 'Submission') / sum(FighterWL == 'W'), 
                               SUBed_Percent = sum(FighterWL == 'L' & Result_clean == 'Submission') / sum(FighterWL == 'L'), 
                               R1_Finishes = sum(FighterWL == 'W' & Result_clean %in% c('Knockout', 'Submission') & EndRound == 1), 
                               R2_Finishes = sum(FighterWL == 'W' & Result_clean %in% c('Knockout', 'Submission') & EndRound == 2), 
                               R3plus_Finishes = sum(FighterWL == 'W' & Result_clean %in% c('Knockout', 'Submission') & EndRound > 2), 
                               R1_Finished = sum(FighterWL == 'L' & Result_clean %in% c('Knockout', 'Submission') & EndRound == 1), 
                               R2_Finished = sum(FighterWL == 'L' & Result_clean %in% c('Knockout', 'Submission') & EndRound == 2), 
                               R3plus_Finished = sum(FighterWL == 'L' & Result_clean %in% c('Knockout', 'Submission') & EndRound > 2), 
                               SDEC_Wins = sum(FighterWL == 'W' & Result_clean == 'Split Decision' | Result_clean == 'Decision'), 
                               UDEC_Wins = sum(FighterWL == 'W' & Result_clean == 'Unanimous Decision'), 
                               SDEC_Losses = sum(FighterWL == 'L' & Result_clean == 'Split Decision' | Result_clean == 'Decision'), 
                               UDEC_Losses = sum(FighterWL == 'L' & Result_clean == 'Unanimous Decision'))]

# fill NAs with 0
agg_career_df[is.na(agg_career_df)] <- 0

#-------------------------------
# SET FIGURE & TABLE PARAMETERS
#-------------------------------

# set figure parameters
win_color <- brewer.pal(6, 'Paired')[2]
loss_color <- brewer.pal(6, 'Paired')[1]
fig_font_color <- 'white'
fig_bg_color <- 'black'
custom_theme <- theme(plot.title = element_text(size = 12, hjust = 0, face = 'bold'), 
                      plot.subtitle = element_text(size = 11, hjust = 0), 
                      axis.text = element_text(size = 10, color = fig_font_color), 
                      legend.text = element_text(size = 10), 
                      legend.key = element_blank(), 
                      plot.title.position = 'plot', 
                      plot.margin = margin(.5, 0.2, 0, 0.2, 'cm'), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())

fighter_record <- agg_career_df[, paste0(Wins, '-', Losses, '-', Draws)]

# set table parameters
cell_text_size <- 9
colnames_text_size <- cell_text_size + 1
padding_mm <- 7

#-------------------------------------
# FIGURE 1: aggregated fight outcomes
#-------------------------------------

# aggregate fight outcomes
agg_outcomes_df <- agg_career_df[, .(Fighter = Fighter, 
                                     '% of Losses by Submission' = SUBed_Percent, 
                                     '% of Losses by Knockout' = KOed_Percent, 
                                     '% of Wins by Submission' = SUB_Percent, 
                                     '% of Wins by Knockout' = KO_Percent, 
                                     '% of Pro MMA Fights Won' = Win_Percent)]

# reshape and format data
agg_outcomes_df <- melt(agg_outcomes_df, id.vars = 'Fighter')
agg_outcomes_df[, WinCol := factor(grepl('Win|Won', variable), levels = c(TRUE, FALSE))]

# generate plot
agg_outcomes_fig <- ggplot(agg_outcomes_df, aes(x = variable, y = value, fill = WinCol, color = WinCol)) + 
  geom_bar(stat = 'identity') + 
  labs(title = 'Aggregated Fight Outcomes', 
       x = '', 
       y = '') + 
  scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1.15), label = percent) + 
  geom_text(aes(x = variable, y = value, label = paste0(100 * round(value, 2), '%')), 
            fontface = 'bold', hjust = -.2) + 
  scale_fill_manual(breaks = c(TRUE, FALSE), 
                    values = c(win_color, loss_color)) + 
  scale_color_manual(breaks = c(TRUE, FALSE), 
                     values = c(win_color, loss_color)) + 
  coord_flip() + 
  dark_theme_gray() + 
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        legend.position = 'none') + 
  custom_theme

#-----------------------------------
# FIGURE 2: detailed fight outcomes
#-----------------------------------

# identify fight outcomes
detailed_outcomes_df <- agg_career_df[, .(Fighter = Fighter, 
                                          Fights = Fights, 
                                          'Split Decision Wins' = SDEC_Wins, 
                                          'Split Decision Losses' = SDEC_Losses, 
                                          'Unanimous Decision Wins' = UDEC_Wins, 
                                          'Unanimous Decision Losses' = UDEC_Losses, 
                                          'Round 3+ Finish Wins' = R3plus_Finishes, 
                                          'Round 3+ Finish Losses' = R3plus_Finished, 
                                          'Round 2 Finish Wins' = R2_Finishes, 
                                          'Round 2 Finish Losses' = R2_Finished, 
                                          'Round 1 Finish Wins' = R1_Finishes, 
                                          'Round 1 Finish Losses' = R1_Finished)]

# reshape and format data
detailed_outcomes_df <- melt(detailed_outcomes_df, id.vars = c('Fighter', 'Fights'))
detailed_outcomes_df[, WL := gsub('.* ', '', variable)]
detailed_outcomes_df[, WL := factor(WL, levels = c('Losses', 'Wins'))]
detailed_outcomes_df[, variable := gsub(' Wins| Losses', '', variable)]
detailed_outcomes_df[, variable := factor(variable, levels = unique(variable))]

# create character value labels
detailed_outcomes_df[, value_str := as.character(value)]
detailed_outcomes_df[nchar(value_str) == 1, value_str := paste0(' ', value_str)]

# generate plot
detailed_outcomes_fig <- ggplot(detailed_outcomes_df, aes(x = variable, y = value, color = WL)) + 
  geom_point(position = position_dodge(width = .6), size = 3) + 
  geom_linerange(aes(xmin = variable, xmax = variable, ymin = 0, ymax = value), 
                 position = position_dodge(width = .6), size = 1.5) + 
  labs(title = 'Detailed Fight Outcomes', 
       x = '', 
       y = '') + 
  scale_y_continuous(limits = c(0, detailed_outcomes_df[, max(value) + 2])) + 
  geom_text(aes(x = variable, y = value, label = value_str), size = 3, fontface = 'bold', hjust = -.6, 
            position = position_dodge(width = .6), show.legend = F) + 
  scale_color_manual(breaks = c('Wins', 'Losses'), 
                     values = c(win_color, loss_color), 
                     guide = guide_legend(override.aes = list(shape = c(19, 19), 
                                                              linetype = c('blank', 'blank')))) + 
  coord_flip() + 
  dark_theme_gray() + 
  theme(legend.position = 'right', 
        legend.title = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank()) + 
  custom_theme

#--------------------------------------------------------------------------
# FIGURE 3: time series - cumulative win %, finish %, finished % over time
#--------------------------------------------------------------------------

# generate time series data of fight outcomes
time_series_df <- career_df[, .(FightDate = as.Date(FightDate, format = '%b %d, %Y'), 
                                Win = FighterWL == 'W', 
                                Finish = grepl('Knockout|Submission', Result_clean))]

# compute rolling win %, finish %, and finished %
setkey(time_series_df, FightDate)
time_series_df[, WinPercent := cumsum(Win) / 1:.N]
time_series_df[, FinishPercentOfN := cumsum(Win & Finish) / 1:.N]
time_series_df[, FinishedPercentOfN := cumsum(!Win & Finish) / 1:.N]
time_series_df[, `:=`(Win = NULL, Finish = NULL)]

# reshape data
time_series_df <- melt(time_series_df, id.vars = 'FightDate')

# account for instances in which a fighter fought multiple times in a single calendar day
time_series_df[, idx := 1:.N, by = variable]
time_series_df[, max_idx_per_day := max(idx), by = .(FightDate, variable)]
time_series_df <- time_series_df[idx == max_idx_per_day]

# generate plot
time_fig <- ggplot(time_series_df, aes(x = FightDate, y = value, color = variable)) + 
  geom_line(aes(linetype = variable), size = 1.2) + 
  scale_x_date(date_labels = '%Y') + 
  scale_y_continuous(seq(0, 1, .25), limits = c(0, 1), label = percent) + 
  scale_linetype_manual(values = c(1, 3, 3), labels = c('Win %', 'Finish % of all fights', 'Finished % of all fights')) + 
  scale_color_manual(values = c(win_color, win_color, loss_color), 
                     labels = c('Win %', 'Finish % of all fights', 'Finished % of all fights')) + 
  labs(title = 'Fight Outcomes Over Time') + 
  dark_theme_gray() + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(), 
        legend.key.size = unit(1, 'line'), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.spacing.x = unit(1, 'cm')) + 
  guides(linetype = guide_legend(label.position = 'bottom')) + 
  custom_theme + 
  theme(plot.margin = margin(0.4, 0.2, 0, 0.2, 'cm'))

#-----------------------------
# FIGURE 4: recent form table
#-----------------------------

# set number of fights to include in table
num_fights <- 6

# generate data to include: date, opponent, fight outcome, and fight duration
recent_tbl_df <- career_df[, .(Date = FightDate, Opponent, FighterWL, Result_clean, EndRound, EndTime, Event = EventName)]

# clean fight outcomes
recent_tbl_df[, FighterWL := fcase(FighterWL == 'W', 'Win', 
                                   FighterWL == 'L', 'Loss', 
                                   FighterWL == 'D', 'Draw')]
recent_tbl_df[, Outcome := paste(FighterWL, '-', Result_clean)]
recent_tbl_df[grepl('Draw', Outcome), Outcome := 'Draw']

# clean fight duration
recent_tbl_df[, Time := paste('Round', EndRound, '-', EndTime)]

# subset on most recent fights
recent_tbl_df <- recent_tbl_df[1:num_fights, .(Date, Opponent, Outcome, Time)]

# account for missing data
recent_tbl_df[is.na(recent_tbl_df)] <- '-'

# identify rows corresponding to wins and losses
win_rows <- which(recent_tbl_df[, grepl('Win', Outcome)])
loss_rows <- which(recent_tbl_df[, grepl('Loss', Outcome)])

# create table object
recent_form_tbl <- ggtexttable(recent_tbl_df, 
                               rows = NULL, 
                               theme = ttheme(padding = unit(c(padding_mm + .3, padding_mm), 'mm'), 
                                              colnames.style = colnames_style(size = colnames_text_size, 
                                                                              color = fig_font_color, 
                                                                              fill = fig_bg_color, 
                                                                              linewidth = 4), 
                                              tbody.style = tbody_style(size = cell_text_size))) %>%
  table_cell_bg(row = 2:recent_tbl_df[, .N + 1], 
                column = 1:ncol(recent_tbl_df), 
                fill = fig_bg_color, 
                color = fig_font_color, 
                linewidth = 2) %>% 
  table_cell_font(row = 2:recent_tbl_df[, .N + 1], 
                  column = 1:ncol(recent_tbl_df), 
                  color = fig_font_color, 
                  size = cell_text_size, 
                  face = 'bold') %>% 
  tab_add_title(text = paste0('Recent Form - Last ', num_fights, ' Fights (Excluding No Contests)'), 
                face = 'bold', size = 12, color = fig_font_color, hjust = .025)

# change text color for losses (if applicable)
if(length(loss_rows) > 0){
  recent_form_tbl <- recent_form_tbl %>% 
    table_cell_font(row = loss_rows + 2, 
                    column = 1:ncol(recent_tbl_df), 
                    color = loss_color, 
                    size = cell_text_size, 
                    face = 'bold')
}

# change text color for wins (if applicable)
if(length(win_rows) > 0){
  recent_form_tbl <- recent_form_tbl %>% 
    table_cell_font(row = win_rows + 2, 
                    column = 1:ncol(recent_tbl_df), 
                    color = win_color, 
                    size = cell_text_size, 
                    face = 'bold')
}

#------------------------
# COMBINE FIGURES & SAVE
#------------------------

# combine figures
combined_fig <- ggarrange(agg_outcomes_fig, 
                          detailed_outcomes_fig, 
                          time_fig, 
                          recent_form_tbl, 
                          nrow = 2, 
                          ncol = 2, 
                          heights = c(1, 1.2), 
                          widths = c(1, 1)) + 
  bgcolor(fig_bg_color)

# save combined figure
png(paste0("Guest_Explainers/Nate_Latshaw/",tolower(gsub(' ', '-', fighter_name)), '-mma-career-overview.png'), height = 562.5, width = 1000, res = 90, bg = 'white')

print(annotate_figure(
  combined_fig, 
  top = text_grob(paste0(fighter_name, ' (', fighter_record, ') Professional MMA Career Overview'), size = 18, color = 'black', face = 'bold'),
  bottom = text_grob(paste0('Created by: @NateLatshaw || Data source as of ', 
                                                format.Date(Sys.Date(), format = '%B %d, %Y'), 
                                                ': sherdog.com'), 
                                         size = 10, color = 'black', face = 'bold')))
dev.off()
