
{{ sprintf("## %s", section.label) }}


#+ echo = FALSE
d1 = read_sediment(file1, section.label, type1, grain.classes, grain.rows)
d2 = read_sediment(file2, section.label, type2, grain.classes, grain.rows)
d.diff = diff_sediment(d1, d2, "Time", "GrainClass", "Diff")
d.rmse.time = rmse_table(d.diff, c("GrainClass", "Time"), "Diff", "RMSE")
#diffidx = which.max(d.diff$Diff)
#tidx = which.max(d.rmse.time$RMSE)
#sidx = which.max(d.rmse.station$RMSE)


#+ echo = FALSE, fig.width = 12, dpi = 150
d.diff %>%
  mutate(Station = as.numeric(str_extract(Station, "[0-9]+"))) %>%
  ggplot() + plot.theme +
  aes(x = Station, y = Diff, color = GrainClass) + geom_line() +
  ylab("Difference") + facet_wrap(~Time) +
  ggtitle(paste(section.label, "Differences at Stations"))

#+ echo = FALSE, fig.width = 12, dpi = 150
d.diff %>% ggplot() + plot.theme +
  aes(x = Time, y = Diff, fill = GrainClass) +
  geom_boxplot() +
  ylab("Difference") +
  ggtitle(paste(section.label, "Differences"))

#+ echo = FALSE, fig.width = 12, dpi = 150
d.rmse.time %>% ggplot() + plot.theme +
  aes(x = GrainClass, y = RMSE, fill = GrainClass) +
  geom_col(color = "black") + ylab("RMSE") +
  facet_wrap(~factor(Time, levels = rev(sort(unique(Time))))) + 
  ggtitle(paste(section.label, "RMSE by Grain Class"))
