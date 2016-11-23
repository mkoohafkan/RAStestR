
{{ sprintf("## %s", section.label) }}


#+ echo = FALSE
d1 = read_standard(file1, section.label, type1)
d2 = read_standard(file2, section.label, type2)
d.diff = diff_table(d1, d2, "Time", "Diff")
d.rmse.station = rmse_table(d.diff, "Station", "Diff", "RMSE")
d.rmse.time = rmse_table(d.diff, "Time", "Diff", "RMSE")
diffidx = which.max(d.diff$Diff)
tidx = which.max(d.rmse.time$RMSE)
sidx = which.max(d.rmse.station$RMSE)
txt = paste(
  "The maximum %s difference of %f occurred at Station %s on %s.",
  "The largest total RMSE at a single time was %f on %s.",
  "The largest total RMSE at a single station was %f at %s."
)
res = sprintf(txt, section.label, d.diff$Diff[diffidx], d.diff$Station[diffidx], 
  d.diff$Time[diffidx], d.rmse.time$RMSE[tidx], d.rmse.time$Time[tidx], 
  d.rmse.station$RMSE[sidx], d.rmse.station$Station[sidx])

#'
{{ res }}
#'

#+ echo = FALSE, fig.width = 12, dpi = 150
d.diff %>% ggplot() + plot.theme +
  aes(x = Station, y = Diff) + geom_boxplot() +
  ylab("Difference") +
  ggtitle(paste(section.label, "Differences at Stations"))

#+ echo = FALSE, fig.width = 12, dpi = 150
d.rmse.station %>% ggplot() + plot.theme +
  aes(x = Station, y = RMSE) +
  geom_col(color = "black", fill = "white") + ylab("RMSE") +
  ggtitle(paste(section.label, "RMSE at Stations"))

#+ echo = FALSE, fig.width = 12, dpi = 150
d.rmse.time %>% mutate(Time = as.POSIXct(Time, format = "%d%b%Y %M:%S",
    tz = "UTC")) %>% ggplot() + plot.theme +
  aes(x = Time, y = RMSE) + geom_line() +
  scale_x_datetime(date_labels = "%b %Y") +
  ylab("RMSE") +
  ggtitle(paste(section.label, "RMSE Through Time"))