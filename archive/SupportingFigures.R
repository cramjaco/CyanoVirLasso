source("Bring_In_Data.R")
library(sp)

parse_latitude <- function(lat){
  Degree <- lat %>% str_extract("^\\-?[:digit:]+\\.?[:digit:]*") %>% as.numeric()
  Minute <- lat %>% str_extract("[:blank:][:digit:]+\\.?[:digit:]*") %>% as.numeric()
  Minute[is.na(Minute)] <- 0
  abs(Degree + Minute/60) * sign(Degree)
}

parse_longitude <- function(lon){
  DegreeOriginal <- lon %>% str_extract("^\\-?[:digit:]+\\.?[:digit:]*") %>% as.numeric()
  Minute <- lon %>% str_extract("[:blank:][:digit:]+\\.?[:digit:]*") %>% as.numeric()
  Minute[is.na(Minute)] <- 0
  IsWest <- lon %>% str_detect("W$")
  DecimalDegree <- abs(DegreeOriginal + Minute/60) * sign(DegreeOriginal) * if_else(IsWest, -1, 1)
  DegreeSameCoord <- if_else(DecimalDegree > 180, DecimalDegree-360, DecimalDegree)
  DegreeSameCoord
}

EnvData00 <- read_excel("data/Metadata_cyano_correctedJAC.xlsx")



Locations <- EnvData00 %>% select(latitude, longitude, OceanRegion = `Ocean Region`) %>% unique %>%
  mutate(latitude = parse_latitude(latitude),
         longitude = parse_longitude(longitude))

# For David
Locations01 <- bind_rows(Locations0,
                       tribble(~latitude, ~longitude, ~OceanRegion,
                               37.35361, 0.286194, "Mediterranean"
                               )
                       )

#ggplot(Locations, aes(y = latitude, x = longitude, col = OceanRegion)) + geom_point()

library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_coastline(scale = 110, returnclass = "sf")
ggplot(data = world) + geom_sf(color = "grey30", fill = "grey90")

ggplot(data = world) + geom_sf(color = "grey30", fill = "grey90") + geom_point(data = Locations, aes(x = longitude, y = latitude) ) +
  coord_sf(xlim = c(-170, -20), ylim = c(-35, 40)) + theme_bw()
ggsave("SampleMap.pdf")

ggplot(data = world) + geom_sf(color = "grey30", fill = "grey90") + geom_point(data = Locations, aes(x = longitude, y = latitude, fill = OceanRegion, shape = OceanRegion) ) +
  coord_sf(xlim = c(-170, -20), ylim = c(-35, 40)) + theme_bw() + scale_shape_manual(values = rep(21:25, 3)) + scale_fill_viridis_d()

## David Map
ggplot(data = world) + geom_sf(color = "grey30", fill = "grey90") + geom_point(data = Locations01, aes(x = longitude, y = latitude) ) +
  coord_sf(xlim = c(-170, 0), ylim = c(-35, 40)) + theme_bw()
ggsave("MapForDavidGarcia.png")
ggsave("MapForDavidGarcia.svg")

## Plots of viruses

EnvData02 <- EnvData00 %>%
  mutate(latitude = parse_latitude(latitude),
         longitude = parse_longitude(longitude))

TargetThings <- left_join(
  EnvData02,
  VirCyanoEnv %>% select(name, nir_a, llv_reads),
  by = c("Name" = "name")
)
cb10 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

	
library(cowplot)
# More color-blind friendly colorbalettes
#http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=10

plotVir0 <- ggplot(TargetThings, aes(y = `Depth(m)`, x = nir_a, shape = `Ocean Region`, fill = `Ocean Region`)) +
  geom_point(size = 2) + scale_shape_manual(values = rep(21:25,3)) + scale_fill_viridis_d() + theme_bw()
plotVir <- plotVir0 + theme(legend.position = "none")
plotCyano <- ggplot(TargetThings, aes(y = `Depth(m)`, x = llv_reads, shape = `Ocean Region`, fill = `Ocean Region`)) +
  geom_point(size = 2) + scale_shape_manual(values = rep(21:25,3)) + scale_fill_viridis_d()  + theme_bw()+ theme(legend.position = "none")
plotCompare <- ggplot(TargetThings, aes(y = nir_a, x = llv_reads, shape = `Ocean Region`, fill = `Ocean Region`)) +
  geom_point(size = 2) + scale_shape_manual(values = rep(21:25,3)) + scale_fill_viridis_d() + theme_bw()+ theme(legend.position = "none")
Legend <- get_legend(plotVir0)

plot_grid(plotVir, plotCyano, plotCompare, Legend)

