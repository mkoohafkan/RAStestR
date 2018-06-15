#' Identify Ineffective Flow Areas
#'
#' Identify ineffective flow areas in cross sections.
#'
#' @inheritParams read_xs
#' @param map.from Define ineffective flow areas starting from 
#'   channel banks (\code{banks}) or from the thalweg (\code{thalweg}) 
#'
make_ineffective_flow_areas = function(f, which.times = NULL,
  which.stations = NULL, from.geometry = TRUE,
  map.from = c("banks", "thalweg")) {
  map.from = match.arg(map.from, c("banks", "thalweg"))

  xs = read_xs(f, which.times, which.stations, from.geometry)
  bank.stations = read_bank_stations(f)
  if (map.from == "thalweg") {
    thalwegs = left_join(xs, bank.stations, by = "Station")
    thalwegs = filter(thalwegs, Distance >= LOB, Distance <= ROB)
    thalwegs = group_by(thalwegs, River, Reach, Station)
    thalwegs = mutate(thalwegs, is.thalweg = near(Elevation, min(Elevation)))
    thalwegs = filter(thalwegs, is.thalweg)
    bank.stations = select(thalwegs, Station, LOB = Distance, ROB = Distance)
  }

  xs = left_join(xs, bank.stations, by = "Station")

  xs = group_by(xs, River, Reach, Station)
  do(xs, map_ineff(.$Distance, .$Elevation, head(.$LOB, 1), head(.$ROB, 1)))

}

#' @importFrom dplyr near
map_ineff = function(sta, elv, lob, rob) {

  which.lob = tail(which(near(lob, sta)), 1)
  which.rob = head(which(near(rob, sta)), 1)

  get_ineff = function(x, y) {
    ineff.areas = list()
    in.ineff = FALSE
    for (i in 2:length(x)) {
      if (in.ineff) {
        if ((y[i] > ineff.elv) | (i == length(x))) {
          end.ineff.sta = x[i]
          ineff.areas = c(ineff.areas, list(
            data.frame(
              start = start.ineff.sta,
              end = end.ineff.sta,
              elev = ineff.elv
            )
          ))
          in.ineff = FALSE
          start.ineff.sta = NULL
          end.ineff.sta = NULL
          ineff.elv = NULL
        }
      }
      if (!in.ineff & (i < length(x))) {
        # decide whether to start a new ineff area
        if ((y[i] >= y[i - 1]) & (y[i] >= y[i + 1])) {
          start.ineff.sta = x[i]
          end.ineff.sta = NULL
          ineff.elv = y[i]
          in.ineff = TRUE
        }
      } 
    }
    ineff.areas
  }

  ineff.areas = c(
    get_ineff(rev(sta[1:(which.lob + 1)]), rev(elv[1:(which.lob + 1)])),
    get_ineff(sta[(which.rob - 1):length(sta)], elv[(which.rob - 1):length(elv)])
  )
  do.call(rbind.data.frame, ineff.areas)
}


#set.seed(41)
#sta = sort(sample(1:1000, 100))
#elv = 600 - rlnorm(100, 1)
#plot(sta, elv, 'l')

#xs = data.frame(Station = sta, Elevation = elv)

#bank.stations = c(141, 179)
#plot(sta, elv, 'l')
#lines(sta[15:21], elv[15:21], col = 'red')

#ineff = map_ineff(xs, c(174, 174))


  #ggplot(NULL) +
  #geom_segment(data = filter(ineff, Station == "XS_33.36"), 
    #aes(x = start, xend = end, y = elev, yend = elev),
    #color = "red") +
  #geom_line(data = filter(xs, Station == "XS_33.36"),
    #aes(x = Distance, y = Elevation))
