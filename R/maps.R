library(tidyverse)
library(tigris)
library(sf)
library(sp)
library(urbnthemes)
library(haven)
library(gridExtra)
options(tigris_use_cache = FALSE)
#remotes::install_github("UrbanInstitute/urbntemplates")

set_urbn_defaults(style = "map", base_family = "Times New Roman")
source(here::here("R", "get_pumas.R"))

# get PUMA shapefiles -----------------------------------------------------

shapefiles <- get_pumas()

shapefiles <- shapefiles %>%
  st_transform(crs = 102003)

# get state shapefiles ----------------------------------------------------

territories <- c("Hawaii",
                 "Alaska",
                 "United States Virgin Islands",
                 "Commonwealth of the Northern Mariana Islands",
                 "Guam",
                 "American Samoa",
                 "Puerto Rico")

states <- tigris::states(class = "sf", cb = TRUE) %>%
  filter(!NAME %in% territories) %>%
  st_transform(crs = 102003)


# load DI data and join it to the PUMA shapefiles -------------------------
# Stata code to fix character variable:
#use "/Users/JonSchwabish/Documents/DIMaps/RDRC_Analytic_File_062420.dta"
#tostring pumaid,format(%07.0f) gen(pumaid2)

didata_full <- read_dta(file = "RDRC_Analytic_File_062420.dta")
didata <- read_dta(file = "pumatotals17.dta")

#shapefiles <- shapefiles %>% unite(pumaid,STATEFP:PUMACE10,sep="")
#shapefiles <- shapefiles %>% mutate(pumaid=paste0(STATEFP,PUMACE10))

digeodata <- full_join(shapefiles, didata_full, by=c("GEOID10" = "pumaid2"))
map_dbl(digeodata,~sum(is.na(.)))

digeodata2007 <- digeodata %>% filter(year==2007)
digeodata2017 <- digeodata %>% filter(year==2017)

# email from Dara lee Luca on 6/24/2020 -------------------------
# 
# We were hoping that you could construct maps 
# (showing variation in rates by PUMA) for the following 
# outcomes for the years 2007 and 2017:
# r_ssiben
# r_diben
# r_ssiaward
# r_diaward
# p_stwdi1ben
# p_bfwdi1ben
# p_dibenearn
# p_stwssi1ben
# p_bfwssi1ben
# p_ssibenearn
# p_stwcm1ben
# p_bfwcm1ben
# 
# We wanted to include two of the maps – p_dibenearn and p_ssibenearn
# (percent of DI/SSIbeneficiaries with earnings) -- in the 5-page conference abstract.
# If you could provide those by 7/13, that would be great. Please let me know if that would be possible.

<<<<<<< HEAD
=======
# colors! --------------------------------------------------------------------

#https://coolors.co/800000-040f16
grays = c("#F2F2F2", "#BFBFBF", "#7F7F7F")
reds = c("#FFADAD", "#800000")

>>>>>>> 77061fea83de8d35fabf3162cb30290b985c5384
# map! --------------------------------------------------------------------

# r_ssiben: SSI recipiency rate (per 10k pop under 64) max: 1533.193
gr_ssiben2007 <- ggplot() +
  geom_sf(data = digeodata2007,
          aes(fill = r_ssiben),
          color = "white",
          size = 0.0) +
  geom_sf(data = states,
          color = "white",
          fill = NA,
<<<<<<< HEAD
          size = 0.2) +
  scale_fill_gradientn()
g1
=======
          size = 0) +
  scale_fill_gradientn(limits=c(0,1600), expand=c(0,0),
                       colors=reds) +
  theme_urbn_map() +
  theme(legend.title=element_blank())
gr_ssiben2007
ggsave(file="maps/gr_ssiben2007.png")

gr_ssiben2017 <- ggplot() +
  geom_sf(data = digeodata2017,
          aes(fill = r_ssiben),
          color = "white",
          size = 0.0) +
  geom_sf(data = states,
          color = "white",
          fill = NA,
          size = 0) +
  scale_fill_gradientn(limits=c(0,1600), expand=c(0,0),
                       colors=reds,
                       labels = scales::comma) +
  theme_urbn_map() +
  theme(legend.title=element_blank())
gr_ssiben2017
ggsave(file="maps/gr_ssiben2017.png")

# r_diben: DI award rate (per 10k working-age pop) max: 1524.546
# r_ssiaward: SSI award rate (per 10k working-age pop) max: 119.3735
# r_diaward: Rate per 10,000 workin-age pop max: 121.2946
# p_stwdi1ben: Percent of DI beneficiaries who had benefits suspended or terminated max: 6.308765
# p_bfwdi1ben: Percent of DI beneficiaries who had benefits forgone due to work max: 6.278435
# p_dibenearn: Percent of DI beneficiaries who had positive earnings max: 40.86215
# p_stwssi1ben: Percent of SSI beneficiaries who had benefits suspended or terminated max: 17.52688
# p_bfwssi1ben: Percent of SSI beneficiaries who had benefits forgone due to work max: 43.54839
# p_ssibenearn: Percent of SSI beneficiaries who had positive earnings max: 48.82629
# p_stwcm1ben: Percent of combined beneficiaries who had benefits suspended or terminated max: 6.676783
# p_bfwcm1ben: Percent of combined beneficiaries who had benefits forgone due to work max: 16.22658







# ggplot(data = us_states_join) + 
#   geom_sf(mapping = aes(fill = pctLatinX), color="white", lwd=.1) +
#   #coord_sf(crs=5070) +
#   #scale_fill_manual(values=c("#d2d2d2","#1696d2")) +
#   labs(title = "LatinX", subtitle="(xxxx)", fill = NULL) +
#   scale_fill_gradientn(limits=c(0,NA), expand=c(0,0), 
#                        colors = c("white", "#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635")) + 
#   theme(legend.position = "none") 


>>>>>>> 77061fea83de8d35fabf3162cb30290b985c5384

g2 <- ggplot() +
  geom_sf(data = digeodata,
          aes(fill = dibenearn),
          color = "white",
          size = 0.05) +
  geom_sf(data = states,
          color = "white",
          fill = NA,
          size = 0.2) +
  scale_fill_gradientn()
g2

g3 <- ggplot() +
  geom_sf(data = digeodata,
          aes(fill = dibenearn),
          color = "white",
          size = 0.1) +
  geom_sf(data = states,
          color = "white",
          fill = NA,
          size = 0.2) +
  scale_fill_gradientn()
g3

g4 <- ggplot() +
  geom_sf(data = digeodata,
          aes(fill = dibenearn),
          color = "white",
          size = 0.05) +
  geom_sf(data = states,
          color = "white",
          fill = NA,
          size = 0.2) +
  scale_fill_gradientn()
g4

g5 <- ggplot() +
  geom_sf(data = digeodata,
          aes(fill = dibenearn),
          color = "white",
          size = 0.05) +
  geom_sf(data = states,
          color = "white",
          fill = NA,
          size = 0.2) +
  scale_fill_gradientn()
g5

g6 <- ggplot() +
  geom_sf(data = digeodata,
          aes(fill = dibenearn),
          color = "white",
          size = 0.05) +
  geom_sf(data = states,
          color = "white",
          fill = NA,
          size = 0.2) +
  scale_fill_gradientn()
g6

#Save file
grid.arrange(g1, g2, g3, g4, g5, g6, nrow=4)
ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2]
g <- arrangeGrob(g1, g2, g3, g4, g5, g6, nrow=4) #generates g
g

ggsave(file="six-pack.png", g) #saves g
ggsave(file="six-pack.tiff", g) #saves 




g