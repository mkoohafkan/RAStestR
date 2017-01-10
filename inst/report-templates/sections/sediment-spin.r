{{ sprintf("## %s", section.label) }}


#+ echo = FALSE
d1 = read_sediment(file1, section.label, type1, table.times.sediment,
  table.stations.sediment, table.grains)
d2 = read_sediment(file2, section.label, type2, table.times.sediment,
  table.stations.sediment, table.grains)
d.diff = diff_sediment(d1, d2, "Time", "GrainClass", "Diff", percent = FALSE)
d.percent = diff_sediment(d1, d2, "Time", "GrainClass", "Percent", percent = TRUE)
d.rmse.time = rmse_table(d.diff, c("GrainClass", "Time"), "Diff", "RMSE")
#diffidx = which.max(d.diff$Diff)
#tidx = which.max(d.rmse.time$RMSE)
#sidx = which.max(d.rmse.station$RMSE)

#+ echo = FALSE, fig.width = 12, dpi = 150
d.diff %>%
  mutate(Station = as.numeric(str_extract(Station, "[0-9]+"))) %>%
  ggplot() + plot.theme + facet_wrap( ~ Time) +
  aes(x = Station, y = Diff, color = GrainClass) + geom_line() +
  scale_y_continuous("Difference", labels = scales::scientific) +
  ggtitle(paste(section.label, "Differences at Stations"))

#+ echo = FALSE, fig.width = 12, dpi = 150
d.percent %>%
  mutate(Station = as.numeric(str_extract(Station, "[0-9]+"))) %>%
  ggplot() + plot.theme + facet_wrap( ~ Time) +
  aes(x = Station, y = Percent, color = GrainClass) + geom_line() +
  scale_y_continuous("Percent Difference", labels = scales::percent) +
  ggtitle(paste(section.label, "Differences at Stations"))


#+ echo = FALSE, fig.width = 12, dpi = 150
d.diff %>% ggplot() + plot.theme +
  aes(x = GrainClass, y = Diff, fill = GrainClass) +
  geom_boxplot() + facet_wrap( ~ Time) + guides(fill = FALSE) +
  scale_y_continuous("Difference", labels = scales::scientific) +
  ggtitle(paste(section.label, "Differences"), subtitle = "all stations")

#+ echo = FALSE, fig.width = 12, dpi = 150
d.percent %>% ggplot() + plot.theme +
  aes(x = GrainClass, y = Percent, fill = GrainClass) +
  geom_boxplot() + facet_wrap( ~ Time) + guides(fill = FALSE) +
  scale_y_continuous("Percent Difference", labels = scales::percent) +
  ggtitle(paste(section.label, "Percent Differences"), subtitle = "all stations")

#+ echo = FALSE, fig.width = 12, dpi = 150
d.rmse.time %>% ggplot() + plot.theme +
  aes(x = GrainClass, y = RMSE, fill = GrainClass) +
  geom_col(color = "black") + guides(fill = FALSE) +
  scale_y_continuous("RMSE", labels = scales::comma) +
  facet_wrap(~factor(Time, levels = rev(sort(unique(Time))))) + 
  ggtitle(paste(section.label, "RMSE by Grain Class"), subtitle = "summed over stations")
