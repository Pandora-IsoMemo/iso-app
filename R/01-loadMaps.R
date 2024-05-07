#' loadMaps
#' @export
loadMaps <- function(){
  # we do need maps for 180 here...
  ocean <- readRDS(system.file("maps/ocean.RDS",
                               package = "MpiIsoApp"))
  grids <- readRDS(system.file("maps/grids.RDS",
                               package = "MpiIsoApp"))
  borders <- readRDS(system.file("maps/borders.RDS",
                                 package = "MpiIsoApp"))
  ocean160 <- readRDS(system.file("maps/ocean160.RDS",
                               package = "MpiIsoApp"))
  ocean200 <- readRDS(system.file("maps/ocean200.RDS",
                                  package = "MpiIsoApp"))
  grids160 <- readRDS(system.file("maps/grids160.RDS",
                               package = "MpiIsoApp"))
  grids200 <- readRDS(system.file("maps/grids200.RDS",
                                  package = "MpiIsoApp"))
  borders160 <- readRDS(system.file("maps/borders160.RDS",
                                 package = "MpiIsoApp"))
  borders200 <- readRDS(system.file("maps/borders200.RDS",
                                 package = "MpiIsoApp"))
  coast <- readRDS(system.file("maps/coast.RDS",
                                    package = "MpiIsoApp"))
  coast160 <- readRDS(system.file("maps/coast160.RDS",
                                  package = "MpiIsoApp"))
  coast200 <- readRDS(system.file("maps/coast200.RDS",
                                    package = "MpiIsoApp"))
  land <- readRDS(system.file("maps/land.RDS",
                               package = "MpiIsoApp"))
  land160 <- readRDS(system.file("maps/land160.RDS",
                                  package = "MpiIsoApp"))
  land200 <- readRDS(system.file("maps/land200.RDS",
                                  package = "MpiIsoApp"))

  return(list(ocean = ocean, grids = grids, borders = borders, coast = coast,
              land = land,
              ocean160 = ocean160, grids160 = grids160, borders160 = borders160,
              ocean200 = ocean200, grids200 = grids200, borders200 = borders200,
              coast160 = coast160, coast200 = coast200,
              land160 = land160, land200 = land200))
}
