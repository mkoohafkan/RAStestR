library(stringr)
library(dplyr)
library(h5)

source("C:/repository/testRAS/R/main.r")

folder = "C:/PROJECTS/FARGO/NEW - HWF-WF Sediment 2015_clip"           

files = list(
  quasi = "HWF-WF_2015.p02.hdf",
  #u30s = "HWF-WF_2015.p07.hdf",
  u1m = "HWF-WF_2015.p06.hdf",
  u5m = "HWF-WF_2015.p05.hdf"
)

isunsteady = list(
  quasi = FALSE,
  #u30s = TRUE,
  u1m = TRUE,
  u5m = TRUE
)

resenvs = replicate(length(files), new.env()) %>% setNames(names(files))

for (n in names(files)) {
  with(resenvs[[n]], {
    f = h5file(file.path(folder, files[[n]]))
    wse = readwse(f, isunsteady[[n]])
    cmi = readcmi(f, isunsteady[[n]], which.rows = 4778)
    lcmc = readlcmc(f, isunsteady[[n]], which.rows = 4778)
    h5close(f)

  })
}
