library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(rsofun)
library(readr)
library(tictoc)
library(raster)
library(sf)
library(cowplot)

vector_path <- "~/data_scratch/big_data/Caravan-csv/Caravan/shapefiles/camels/camels_basin_shapes.shp"

shapefile <- terra::vect(vector_path)

attributes_hydroatlas_camels <- read_csv("~/data_scratch/big_data/Caravan-csv/Caravan/attributes/camels/attributes_hydroatlas_camels.csv")

siteinfo <- read.csv("~/data_scratch/big_data/Caravan-csv/Caravan/attributes/camels/attributes_other_camels.csv")

siteinfo <- siteinfo[siteinfo$gauge_id %in% shapefile$gauge_id,]

filter_out <- readLines("../my_stuff/camels_to_discard.txt")

# we add the year of interest, for example 2004

date_start = lubridate::ymd(paste0(1982, "-01-01"))
date_end = lubridate::ymd(paste0(2011, "-12-31"))


siteinfo <- siteinfo |>
  dplyr::mutate(date_start = date_start) |>
  dplyr::mutate(date_end = date_end) |>
  mutate(sitename = gauge_id) |>
  rename(lon = gauge_lon,
         lat = gauge_lat)

attributes_hydroatlas_camels <- attributes_hydroatlas_camels |>
  rename(sitename = gauge_id) 
  #filter(hft_ix_s09 < 50 & hft_ix_s93 < 50 )

siteinfo2 <- siteinfo[siteinfo$sitename %in% attributes_hydroatlas_camels$sitename,]

siteinfo2 <- siteinfo2[!(siteinfo2$sitename %in% filter_out),]

driver_data <- readRDS("../my_stuff/driver_data_camels.rds")

meta_info  <- read_csv("../my_stuff/camels_site_info.csv")

land_use <- read_csv("~/data_scratch/long_land_use_camels_2005.csv")

climates <- read_csv("~/data_scratch/long_koeppen_camels.csv")


driver_data <- driver_data[(driver_data$sitename %in% siteinfo2$sitename),]

meta_info <- meta_info[(meta_info$sitename %in% siteinfo2$sitename),]


land_use <- land_use[(land_use$sitename %in% siteinfo2$sitename),]

climates <- climates[(climates$sitename %in% siteinfo2$sitename),]

best_par <- readRDS("~/data_scratch/my_stuff/global_calib_shorter.rds")

#best_par <- readRDS("~/data_scratch/rsofun/global_calib_diffusion.rds")


params_modl <- list(
  kphio              =  best_par$par[["kphio"]],   
  kphio_par_a        =  best_par$par[["kphio_par_a"]],       
  kphio_par_b        =  best_par$par[["kphio_par_b"]],       
  rd_to_vcmax        =  0.014,  
  soilm_thetastar    =  best_par$par[["soilm_thetastar"]],       
  beta_unitcostratio =  best_par$par[["beta_unitcostratio"]],        
  tau_acclim         =  30,
  kc_jmax            =  0.41,
  gw_calib           = best_par$par[["gw_calib"]]
)


driver_data <- driver_data |> unnest(params_siml) |>
  mutate(use_gs = TRUE,
         use_phydro = FALSE,
         use_pml= TRUE,
         is_global = FALSE) |>
  group_by(sitename) |>
  nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,  lgr3,  lgn3,  lgr4 ,
                       use_gs, use_phydro, use_pml,is_global))


filter <- attributes_hydroatlas_camels[attributes_hydroatlas_camels$dor_pc_pva== 0,]


driver_data <- driver_data[driver_data$sitename %in% filter$sitename,]

####----------

# map

vector_path <- "~/data_scratch/shapefiles/camels/camels_basin_shapes.shp"

#shapefile <- terra::vect(vector_path)

shapefile <- sf::st_read(vector_path)

coord <- driver_data |>
  unnest(site_info) |>
  dplyr::select(sitename,lat,lon)

us_map <- map_data("state")

map_plot <-
  left_join(
    coord, multi_year,
    by = "sitename"
  )


shapefile <- shapefile[shapefile$gauge_id %in% map_plot$sitename,]




shapefile$climate <- multi_year$delta


ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = map_plot, aes(x = lon, y = lat, color = delta), size = 3) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "sim - obs yearly aet (mm)", x = "Longitude", y = "Latitude")

attributes_other_camels <- read_csv("~/data_scratch/camels_timeseries/camels/attributes_other_camels.csv")

attributes_other_camels <- attributes_other_camels[attributes_other_camels$gauge_id %in% map_plot$sitename,]

map_plot <- left_join(map_plot,attributes_other_camels |>
                        dplyr::select(gauge_id,area) |>
                        rename(sitename = gauge_id),
                      by = "sitename")

ggplot(map_plot, aes(x = area, y = delta)) + geom_point()


# Fluxnet US

driver <- read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.2.rds")

driver <- driver[grep("US", driver$sitename),]


coord_flux <- driver |> unnest(site_info) |>
  dplyr::select(sitename,lon,lat, canopy_height) |>
  filter(lat < 50)


ggplot(data = shapefile) +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_sf(data = shapefile, aes(fill = climate), color = "black") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") 



map_plot <- left_join(
  map_plot,
  driver_data |> unnest(site_info) |> dplyr::select(sitename,canopy_height),
  by ="sitename"
)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = map_plot, aes(x = lon, y = lat, color = canopy_height), size = 3) +
  scale_color_viridis_c() +
  #scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "canopy height", x = "Longitude", y = "Latitude")

ggplot(map_plot, aes(x = canopy_height, y = delta)) +
  geom_point() 


####----------


# check for canopy height

# try with different reference (sum)

create_df_testing <- function(diffence_reference){
  # driver_data <- readRDS("driver_data_camels.rds")
  # 
  # 
  
  
  dir <- "~/data_scratch/elevation_lang_005.tif"
  
  df_out <- ingestr:::extract_pointdata_allsites_shp(
    filename = "~/data_scratch/elevation_lang_005.tif",
    df_shapefile = shapefile[shapefile$gauge_id %in% siteinfo$gauge_id,],
    get_time = F,
    year_arg = NA_integer_, month_arg = NA_integer_ # only used for WFDEI in combination with get_time
  ) 
  
  driver_data <- 
    left_join(driver_data |>
                unnest(site_info) |>
                dplyr::select(- canopy_height, -reference_height),
              df_out  |> unnest(data) |>
                rename(canopy_height = ETH_GlobalCanopyHeight_10m_2020_N00E006_Map) |>
                mutate(reference_height = canopy_height + diffence_reference) |>
                #mutate(reference_height = if_else(reference_height < 15,10,reference_height)) |>
                dplyr::select(-ID),
              by ="sitename")|>
    nest(site_info = c(lat,lon,elv,whc,canopy_height,reference_height))
  
  
  # driver_data <- driver_data |> unnest(params_siml) |>
  #   mutate(use_gs = TRUE,
  #          use_phydro = FALSE,
  #          use_pml= F) |>
  #   group_by(sitename) |>
  #   nest(params_siml = c(spinup, spinupyears, recycle, outdt, ltre,  ltne,  ltrd,  ltnd,  lgr3,  lgn3,  lgr4 ,
  #                        use_gs, use_phydro, use_pml))
  
  
  
  # run the model for these parameters
  output <- rsofun::runread_pmodel_f(
    driver_data,
    par = params_modl
  )
  
  
  mod <- left_join(output |>
                     unnest(data) |>
                     dplyr::select(sitename,date,cond,aet,pet),
                   driver_data |>
                     unnest(forcing) |>
                     dplyr::select(sitename,date,rain) |>
                     mutate(rain = rain * 86400),
                   by = c("sitename","date")
  ) |> 
    mutate(prec = rain + cond) |>
    group_by(sitename, year(date)) |>
    summarise(aet = sum(aet, na.rm = T),
              prec = sum(prec,na.rm = T),
              pet = sum(pet, na.rm = T))
  
  
  obs <- driver_data |>
    unnest(forcing) |>
    mutate(rain = rain * 86400) |>
    group_by(sitename, year(date)) |>
    summarise(
      
      runoff = sum(runoff, na.rm = T),
      
              rain = sum(rain,na.rm = T))
  
  obs2 <- driver_data |>
    unnest(site_info) |>
    group_by(sitename) |>
    dplyr::select(sitename,canopy_height,reference_height,lat,lon)
  
  # change runoff
  obs3 <- left_join(obs,
            attributes_hydroatlas_camels |>
              dplyr::select(sitename,run_mm_syr),
            by = "sitename")
  # 
  # multi_year <- left_join(
  #   mod, obs, by = c("sitename","year(date)")) 
  
  multi_year <- left_join(
    mod, obs3, by = c("sitename","year(date)")) |>
    rename(mod_aet = aet) |>
    mutate(obs_aet_hydro = rain - run_mm_syr,
           obs_aet = rain - runoff,
           delta = mod_aet - obs_aet,
           delta_hydro = mod_aet - obs_aet_hydro,
           error_hydro = abs(mod_aet - obs_aet_hydro),
           error = abs(mod_aet - obs_aet)) |> # change here
    group_by(sitename) |>
    summarise(mod_aet = mean(mod_aet),
              obs_aet = mean(obs_aet),
              obs_aet_hydro = mean(obs_aet_hydro),
              delta = mean(delta),
              delta_hydro = mean(delta_hydro),
              error = mean(error),
              error_hydro = mean(error_hydro),
              rain = mean(rain),
              runoff_hydro = mean(run_mm_syr),
              runoff = mean(runoff),  # change here
              pet = mean(pet))|>
    dplyr::select(sitename, mod_aet, obs_aet,obs_aet_hydro,delta,delta_hydro,error,error_hydro,rain,runoff,runoff_hydro,pet)

  multi_year <- left_join(multi_year,obs2,by="sitename")
  
  return(multi_year)
}

# testing the best absolute delta (is -1)

# reference_set <- seq(-1.2,-1,0.02)
# 
# delta_res <- NULL
# 
# for(i in reference_set){
#   multi_year <- create_df_testing(i)
#   
#   message(i)
#   delta_res <- append(delta_res,sum(abs(multi_year$error)))
#   
# }
# 
# plot(reference_set,delta_res)

multi_year <- create_df_testing(0)

shapefile <- sf::st_read(vector_path)

shapefile <- shapefile[shapefile$gauge_id %in% multi_year$sitename,]


shapefile$climate <- multi_year$delta



multi_year <- left_join(multi_year,
                        siteinfo |> 
                          dplyr::select(sitename,area),
                        by = "sitename")

multi_year2 <- left_join(multi_year,
                        attributes_hydroatlas_camels |> 
                          dplyr::select(sitename,run_mm_syr),
                        by = "sitename") |>
  mutate(obs_aet = rain - run_mm_syr)

ggplot(multi_year , aes(x = runoff_hydro, y = error)) + geom_point() 


us_map <- map_data("state")

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = multi_year, aes(x = lon, y = lat, color = delta), size = 3) +
  scico::scale_color_scico(palette =  "vik") +
  #scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = expression( paste("delta aet (mm yr "^-1,") " )), x = "Longitude", y = "Latitude")

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = multi_year, aes(x = lon, y = lat, color = delta_hydro), size = 3) +
  #scico::scale_color_scico(palette =  "vik") +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = expression( paste("delta aet (mm yr "^-1,") " )), x = "Longitude", y = "Latitude")

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = multi_year, aes(x = lon, y = lat, color = error), size = 3) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = expression( paste("RMSE aet (mm yr "^-1,") " )), x = "Longitude", y = "Latitude")

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = multi_year, aes(x = lon, y = lat, color = error_hydro), size = 3) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = expression( paste("RMSE aet (mm yr "^-1,") " )), x = "Longitude", y = "Latitude")

# max_land_use <- left_join(
#   land_use |> 
#     group_by(sitename) |>
#     summarise(percentage = max(percentage)),
#   land_use, by = c("sitename","percentage")
# ) |>
#   rename(max_percentage = percentage)
# 
max_cliamte <- left_join(
  climates |>
    group_by(sitename) |>
    summarise(fraction = max(fraction)),
  climates, by = c("sitename","fraction")
) |>
  rename(max_fraction = fraction)
#max_cliamte <- left_join(max_cliamte ,multi_year)

ggplot(max_cliamte , aes(x = run_mm_syr, y = error, color = koeppen_code)) + geom_point() 


# 
# 
# max_land_use$is_dominant <- ifelse(max_land_use$max_percentage > 66,T,F)
# 
# land_use_plot <- left_join(
#   max_land_use,
#   multi_year |> dplyr::select(sitename,delta,error),
#   by = "sitename")
# 
# ggplot(land_use_plot,aes(x = land_use, y = delta)) + 
#   geom_boxplot() + 
#   geom_jitter(aes(colour = is_dominant)) +
#   ylab("delta (mm)") +
#   coord_flip()
# 
# ggplot(land_use_plot,aes(x = land_use, y = error)) + 
#   geom_boxplot() + 
#   geom_jitter(aes(colour = is_dominant)) +
#   ylab("error (mm)") +
#   coord_flip()
# 
# max_cliamte$is_dominant <- ifelse(max_cliamte$max_fraction > 66,T,F)
# 
# climate_plot <- left_join(
#   max_cliamte,
#   multi_year |> dplyr::select(sitename,delta,error),
#   by = "sitename")
# 
# ggplot(climate_plot,aes(x = koeppen_code, y = delta)) + 
#   geom_boxplot(outlier.shape = NA) + 
#   geom_jitter() +
#   ylab(expression( paste("delta aet (mm yr "^-1,") " ))) 
# 
# ggplot(climate_plot,aes(x = koeppen_code, y = error)) + 
#   geom_boxplot() + 
#   geom_jitter(aes(colour = is_dominant)) +
#   ylab("error (mm)") 
# 


df_budyko <- rbind(multi_year |> dplyr::select(sitename,rain,pet,mod_aet) 
                   |>rename(aet = mod_aet) |> mutate(out = "mod"),
                   multi_year |> dplyr::select(sitename,rain,pet,obs_aet) |>
                     rename(aet = obs_aet)|> mutate(out = "obs"),
                   multi_year |> dplyr::select( sitename, obs_aet_hydro,  rain, pet) |>
                     rename(aet = obs_aet_hydro) |> mutate(out = "hydro")) |>
  drop_na()


df_budyko <- left_join( df_budyko,max_cliamte, by = "sitename")

multi_year <- left_join(multi_year,max_cliamte, by = "sitename")



budyko_climate <- left_join(max_cliamte, multi_year, by = "sitename")

budyko_climate2 <- budyko_climate |> 
  group_by(koeppen_code) |>
  summarise(obs_aet = median(obs_aet,na.rm = T),
            hydro_aet = median(obs_aet_hydro,na.rm=T),
            mod_aet = median(mod_aet,na.rm = T),
            rain = median(rain,na.rm = T),
            pet = median(pet,na.rm = T),
            n = n()) |>
  filter(n > 4)

budyko_climate3 <- rbind(budyko_climate2 |> dplyr::select( koeppen_code, obs_aet,  rain, pet) |>
                           rename(aet = obs_aet) |> mutate(out = "obs"),
                         budyko_climate2 |> dplyr::select( koeppen_code, mod_aet,  rain, pet) |>
                           rename(aet = mod_aet) |> mutate(out = "mod"),
                         budyko_climate2 |> dplyr::select( koeppen_code, hydro_aet,  rain, pet) |>
                           rename(aet = hydro_aet) |> mutate(out = "hydro"))


ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_hline(yintercept = 1, , linetype = "dotted") + 
  geom_point(data = df_budyko, aes(x = pet / rain, y = aet / rain, shape = out, colour = koeppen_code), alpha = 0.25) +
  geom_point(data = budyko_climate3 , aes(x = pet / rain, y = aet / rain, shape = out, colour = koeppen_code), size = 3)




# three separate plts (better)


plot_2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_hline(yintercept = 1, , linetype = "dotted") + 
  geom_point(data = df_budyko |> filter(out == "obs"), aes(x = pet / rain, y = aet / rain, colour = koeppen_code), alpha = 0.25) +
  geom_point(data = budyko_climate3 |> filter(out == "obs"), aes(x = pet / rain, y = aet / rain, colour = koeppen_code), size = 2) +
  theme(legend.position = "none") + ylim(0,1.07) + xlim(0,10)

plot_1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_hline(yintercept = 1, , linetype = "dotted") + 
  geom_point(data = df_budyko |> filter(out == "mod"), aes(x = pet / rain, y = aet / rain, colour = koeppen_code), alpha = 0.25) +
  geom_point(data = budyko_climate3 |> filter(out == "mod"), aes(x = pet / rain, y = aet / rain, colour = koeppen_code), size = 2) +
  theme(legend.position = "none") + ylab(NULL) + ylim(0,1.07)  + xlim(0,10)

plot_3 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_hline(yintercept = 1, , linetype = "dotted") + 
  geom_point(data = df_budyko |> filter(out == "hydro"), aes(x = pet / rain, y = aet / rain, colour = koeppen_code), alpha = 0.25) +
  geom_point(data = budyko_climate3 |> filter(out == "hydro"), aes(x = pet / rain, y = aet / rain, colour = koeppen_code), size = 2) +
  theme(legend.position = "none") + ylab(NULL) + ylim(0,1.07)  + xlim(0,10)


ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_hline(yintercept = 1, , linetype = "dotted") + 
  geom_point(data = budyko_climate3 , aes(x = pet / rain, y = aet / rain, shape = out, colour = koeppen_code))+
  ylab(NULL) + ylim(0,1.07)  + xlim(0,10)

plot_budyko <- plot_grid(plot_1,plot_2,plot_3, align = "h", ncol = 3, labels = letters[1:3])

ggsave(plot = plot_budyko, filename = "../budyko_merged.pdf", device = "pdf", dpi = 300, width = 21, height = 7)

#budyko_climate2 <- left_join(max_cliamte, multi_year, by = "sitename")


plot_1 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_point(data = budyko_climate2 , aes(x = obs_aet / rain, y = mod_aet / rain,  colour = koeppen_code), size = 2) +
  geom_point(data = multi_year[multi_year$koeppen_code %in% budyko_climate2$koeppen_code,], aes(x = obs_aet  / rain, y = mod_aet / rain, colour = koeppen_code), alpha = 0.25) +
  ylim(0,1.07)  + xlim(0,1.07)
  
plot_2 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_point(data = budyko_climate2 , aes(x = hydro_aet  / rain, y = mod_aet / rain,  colour = koeppen_code), size = 2) +
  geom_point(data = multi_year[multi_year$koeppen_code %in% budyko_climate2$koeppen_code,], aes(x = obs_aet_hydro  / rain, y = mod_aet / rain, colour = koeppen_code), alpha = 0.25) +
  ylim(0,1.07)  + xlim(0,1.07)

plot_3 <- ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_point(data = budyko_climate2 , aes(y = hydro_aet  / rain, x= obs_aet / rain,  colour = koeppen_code), size = 2) +
  geom_point(data = multi_year[multi_year$koeppen_code %in% budyko_climate2$koeppen_code,], aes(y = obs_aet_hydro  / rain, x = obs_aet / rain, colour = koeppen_code), alpha = 0.25) +
  ylim(0,1.07)  + xlim(0,1.07)

plot_budyko <- plot_grid(plot_1,plot_2,plot_3, align = "h", ncol = 3, labels = letters[1:3])

ggsave(plot = plot_budyko, filename = "../scatter_merged.pdf", device = "pdf", dpi = 300, width = 21, height = 7)

# runoff <- budyko_climate |> 
#   group_by(koeppen_code) |>
#   summarise(obs_runoff = median(runoff,na.rm = T),
#             hydroAtlas_runoff = median(runoff_hydro,na.rm = T),
#             n = n()) |>
#   filter(n > 4)

ggplot() +
  geom_abline(slope = 1, intercept = 0, , linetype = "dotted") +
  geom_point(data = runoff , aes(x = hydroAtlas_runoff, y = obs_runoff,  colour = koeppen_code))


df_budyko2 <- rbind(df_budyko|> dplyr::select(-sitename),budyko_climate )


# group.colors <- c(mod =  rgb(0.103516, 0.047787, 0.393530),
#                   obs = rgb(0.997061, 0.949794, 0.951206))


budyko_plot <- ggplot(df_budyko, aes(x= pet/rain, y= aet/rain, shape= out)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") + 
  scale_fill_manual(values=group.colors) +
  labs(y = "aet / precipitation", x = "pet / precipitation") 

ggsave(plot = budyko_plot, paste0("../my_stuff/","busyko_camels.png"),dpi = 300, width = 10, height = 7)


meanc_limate <- rbind(budyko_climate |> mutate(out = "obs"),
                      budyko_climate2 |> mutate(out = "mod"))

ggplot() + 
  geom_point(data =df_budyko |> filter(out == "obs"), aes(x= pet/rain, y= aet/rain, alpha = 0.5)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") + 
  labs(y = "aet / precipitation", x = "pet / precipitation") +
  geom_point(data =  budyko_climate |> 
               mutate(koeppen_code = paste0(koeppen_code, ", n = ",n)), 
             aes(x= pet/rain, y= aet/rain, color= koeppen_code))


ggplot(meanc_limate, aes(x= pet/rain, y= aet/rain, shape = out, color = koeppen_code)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") + 
  labs(y = "aet / precipitation", x = "pet / precipitation")


# land_use_plot <- left_join(
#   land_use,
#   multi_year |> dplyr::select(sitename,delta,error),
#   by = "sitename")
# 
# 
# climate_plot <- left_join(
#   climates,
#   multi_year |> dplyr::select(sitename,delta,error),
#   by = "sitename")
# 
# 
# land <- unique(max_land_use$land_use)
# 
# clim <- unique(max_cliamte$koeppen_code)


ggplot(multi_year, aes(x = canopy_height, y = delta)) + geom_point() +
  labs(y = expression( paste("delta ET (mm yr "^-1,") " )), x = "Canopy height (m)")

canopy_error <- ggplot(multi_year, aes(x = canopy_height, y = error)) + geom_point(size = 0.75) +
  labs(y = expression( paste("RMSE ET (mm yr "^-1,") " )), x = "Canopy height (m)")
print(canopy_error)

ggsave(plot = canopy_error,"../camels_error_by_canopy.png", width = 1545, height = 1093, scale = 1, units = "px")


aera_error <- ggplot(multi_year, aes(x = area, y = error)) + geom_point(size = 0.75) +
  scale_x_log10() +
  labs(y = expression( paste("RMSE ET (mm yr "^-1,") " )), 
       x = expression( paste("Area (km "^2,") " )))

print(aera_error)


#ggsave(plot = aera_error,"../camels_error_by_area.png", width = 1545, height = 1093, scale = 1, units = "px")


# recalibrate 

ref_seq <- seq(-1,1,0.1)

err_seq <- NULL

for(i in ref_seq){
  
  tmp <- create_df_testing(i)
  
  err_seq <- append(err_seq,sum(tmp$error, na.rm =T))
  
}


plot(ref_seq,err_seq)

plot(reference_set,delta_res)


multi_year <- create_df_testing(0)


ggplot(multi_year, aes(x = canopy_height, y = delta)) +
  geom_point() 


ggplot(multi_year, aes(x = pet / rain, y = mod_aet/rain)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1,linetype = "dotted")


sum(multi_year$error)

multi_year <- left_join(multi_year,
                        max_cliamte,
                        by = "sitename")


ggplot(multi_year, aes(x = koeppen_code, y = delta)) + geom_boxplot() + geom_jitter() +
  ylab("delta (mm)")

ggplot(multi_year, aes(x = koeppen_code, y = topographic_index)) + geom_boxplot() + geom_jitter() 


ggplot(multi_year, aes(x = rain / runoff, y = delta, color = koeppen_code)) +
  geom_point() + scale_x_log10()


multi_year$Dfc <- ifelse(multi_year$koeppen_code == "Dfc",T,F)


ggplot(multi_year , aes(x = topographic_index, y = delta, color= Dfc)) +
  geom_point() 
 

print(paste0("multi year mean error (mm): ",sum(abs(multi_year$error)) / 594))

print(paste0("mean daily error (mm): ",sum(abs(multi_year$error)) / (594*366)))

ggplot(df_budyko, aes(x= pet/rain, y= aet/rain, color= out)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted")

ggplot(multi_year,aes(x = error)) + geom_histogram(bins = 50, fill = "white", colour = "black") +
  xlab("year mean error (mm)")
 

ggplot(multi_year,aes(x = delta)) + geom_histogram(bins = 50, fill = "white", colour = "black") +
  xlab("year mean delta (mm)")


ggplot(multi_year,aes(x = (mod_aet / rain) - (obs_aet / rain))) + geom_histogram(bins = 50, fill = "white", colour = "black") 


climate <- left_join(max_cliamte, multi_year, by = "sitename")

ggplot(climate[!(climate$koeppen_code %in% c("Dfb","Dfa","Cfa","Dfc","Dsc")),],aes(x = (mod_aet / rain) - (obs_aet / rain))) + geom_histogram(bins = 30, fill = "white", colour = "black") 


climate <- climate |>
  mutate(ratio = (mod_aet / rain) - (obs_aet / rain))

median(climate[!(climate$koeppen_code %in% c("Dfb","Dfa","Cfa")),]$ratio)

clim <- max_cliamte

ggplot(multi_year,aes(x = (mod_aet / rain) - (obs_aet / rain))) + geom_histogram(bins = 50, fill = "white", colour = "black") 


ggplot(multi_year, aes(x = pet / rain, y = mod_aet/rain, color= Dfc)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1,linetype = "dotted")

ggplot(multi_year, aes(x = pet / rain, y = obs_aet/rain, color= Dfc)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1,linetype = "dotted")


####---------

# try with different reference (product)

create_df_testing <- function(diffence_reference){
  driver_data <- readRDS("driver_data_camels.rds")
  
  
  driver_data <- driver_data |> unnest(site_info) |>
    mutate(reference_height = canopy_height * diffence_reference) |>
    group_by(sitename) |>
    nest(site_info = c(lat,lon,elv,whc,canopy_height,reference_height))
  
  best_par <- readRDS("~/data_scratch/rsofun/global_calib_shorter.rds")
  
  params_modl <- list(
    kphio              =  best_par$par[["kphio"]],   
    kphio_par_a        =  best_par$par[["kphio_par_a"]],       
    kphio_par_b        =  best_par$par[["kphio_par_b"]],       
    rd_to_vcmax        =  0.014,  
    soilm_thetastar    =  best_par$par[["soilm_thetastar"]],       
    beta_unitcostratio =  best_par$par[["beta_unitcostratio"]],        
    tau_acclim         =  30,
    kc_jmax            =  0.41,
    gw_calib           = best_par$par[["gw_calib"]]
  )
  
  
  filter_out <- readLines("camels_to_discard.txt")
  
  driver_data <- driver_data[!(driver_data$sitename %in% filter_out),]
  
  # run the model for these parameters
  output <- rsofun::runread_pmodel_f(
    driver_data,
    par = params_modl
  )
  
  
  mod <- left_join(output |>
                     unnest(data) |>
                     dplyr::select(sitename,date,cond,aet,pet),
                   driver_data |>
                     unnest(forcing) |>
                     dplyr::select(sitename,date,rain) |>
                     mutate(rain = rain * 86400),
                   by = c("sitename","date")
  ) |> 
    mutate(prec = rain + cond) |>
    group_by(sitename, year(date)) |>
    summarise(aet = sum(aet, na.rm = T),
              prec = sum(prec,na.rm = T),
              pet = sum(pet, na.rm = T))
  
  
  obs <- driver_data |>
    unnest(forcing) |>
    mutate(rain = rain * 86400) |>
    group_by(sitename, year(date)) |>
    summarise(runoff = sum(runoff, na.rm = T),
              rain = sum(rain,na.rm = T))
  
  obs2 <- driver_data |>
    unnest(site_info) |>
    group_by(sitename) |>
    dplyr::select(sitename,canopy_height,reference_height,koeppen_code,lat,lon)
  
  
  multi_year <- left_join(
    mod, obs, by = c("sitename","year(date)")) |>
    rename(mod_aet = aet) |>
    mutate(obs_aet = rain - runoff) |>
    mutate(delta = mod_aet - obs_aet) |>
    mutate(error = abs(mod_aet - obs_aet)) |>
    group_by(sitename) |>
    summarise(mod_aet = mean(mod_aet),
              obs_aet = mean(obs_aet),
              delta = mean(delta),
              error = mean(error),
              rain = mean(rain),
              runoff = mean(runoff),
              pet = mean(pet))|>
    dplyr::select(sitename, mod_aet, obs_aet,delta,error,rain,runoff,pet)
  
  multi_year <- left_join(multi_year,obs2,by="sitename")
  
  return(multi_year)
}

us_map <- map_data("state")


# testing the best absolute delta (is -1)

reference_set <- seq(-1,1,0.25)

delta_res <- NULL

for(i in reference_set){
  multi_year <- create_df_testing(i)
  
  message(i)
  delta_res <- append(delta_res,sum(abs(multi_year$error)))
  
}

plot(reference_set,delta_res)

multi_year <- create_df_testing(0.95)


ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = multi_year, aes(x = lon, y = lat, color = delta), size = 3) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "sim - obs yearly aet (mm)", x = "Longitude", y = "Latitude")


ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = multi_year, aes(x = lon, y = lat, color = error), size = 3) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "sim - obs yearly aet (mm)", x = "Longitude", y = "Latitude")




ggplot(multi_year, aes(x = canopy_height, y = delta)) +
  geom_point() 

ggplot(multi_year, aes(x = koeppen_code, y = delta)) + geom_boxplot() + geom_jitter() +
  ylab("delta (mm)")

ggplot(multi_year, aes(x = canopy_height, y = delta)) +
  geom_point() 

ggplot(multi_year, aes(x = rain / runoff, y = delta, color = koeppen_code)) +
  geom_point() + scale_x_log10()

# box <- multi_year
# 
# box$breaks <- cut(multi_year$canopy_height, breaks = 15, labels = FALSE)
# 
# ggplot(box, aes(x = breaks, y = delta, group = breaks)) +
#   geom_boxplot()+ geom_jitter()



total_error <- sum(abs(multi_year$error))

print(paste0("multi year mean error (mm): ",total_error / 594))

print(paste0("mean daily error (mm): ",total_error / (594*366)))


ggplot(multi_year,aes(x = error)) + geom_histogram(bins = 50, fill = "white", colour = "black") +
  #geom_vline(xintercept = mean(abs(multi_year$error)), color = "red") +
  labs(y = "number of catchments", x =  expression( paste("RMSE aet (mm yr "^-1,") " )))

ggplot(multi_year,aes(x = delta)) + geom_histogram(bins = 50, fill = "white", colour = "black") +
  #geom_vline(xintercept = mean(multi_year$delta), color = "red") +
  labs(y = "number of catchments", x =  expression( paste("delta aet (mm yr "^-1,") " )))




####----------

# by calibrating (not really) the reference height


# for now by a ugly for cycle

find_best_ref <- function(i){
  
  test <- driver_data[i,]
  
  ref_array <- seq( test$site_info[[1]]$canopy_height * 0.7, test$site_info[[1]]$canopy_height + 10,length.out = 50)
  
  test <- rbind(test, test[rep(1, 49), ])
  
  test <- test |>
    unnest(site_info) |>
    mutate(reference_height = ref_array) |>
    mutate(sitename = paste0(sitename,"_",ref_array)) |>
    group_by(sitename) |>
    nest(site_info = c(lat,lon,elv,whc,canopy_height,reference_height)) |>
    dplyr::select(sitename, params_siml,site_info,forcing)
  
  output <- rsofun::runread_pmodel_f(
    test,
    par = params_modl
  )
  
  message(paste0("done ", i))
  
  mod <- left_join(output |>
                     unnest(data) |>
                     dplyr::select(sitename,date,cond,aet,pet),
                   test |>
                     unnest(forcing) |>
                     dplyr::select(sitename,date,rain) |>
                     mutate(rain = rain * 86400),
                   by = c("sitename","date")
  ) |> 
    mutate(prec = rain + cond) |>
    group_by(sitename, year(date)) |>
    summarise(aet = sum(aet, na.rm = T),
              prec = sum(prec,na.rm = T),
              pet = sum(pet, na.rm = T))
  
  
  obs <- test |>
    unnest(forcing) |>
    mutate(rain = rain * 86400) |>
    group_by(sitename, year(date)) |>
    summarise(runoff = sum(runoff, na.rm = T),
              rain = sum(rain,na.rm = T))
  
  obs2 <- test |>
    unnest(site_info) |>
    group_by(sitename) |>
    dplyr::select(sitename,canopy_height,reference_height,lat,lon)
  
  
  mod_obs <- left_join(
    mod, obs, by = c("sitename","year(date)")) |>
    rename(mod_aet = aet) |>
    mutate(obs_aet = rain - runoff) |>
    mutate(delta = mod_aet - obs_aet) |>
    mutate(error = abs(mod_aet - obs_aet)) |>
    group_by(sitename) |>
    summarise(mod_aet = mean(mod_aet),
              obs_aet = mean(obs_aet),
              delta = mean(delta),
              error = mean(error),
              rain = mean(rain),
              runoff = mean(runoff),
              pet = mean(pet))|>
    dplyr::select(sitename, mod_aet, obs_aet,delta,error,rain,runoff,pet)
  
  mod_obs <- left_join(mod_obs,obs2,by="sitename")
  
  mod_obs <- mod_obs |> filter(error == min(error)) |>
    mutate(sitename = substr(sitename,1,15))
  
  return(mod_obs)
  
  }


arr_of_best <- NULL

for(i in 1:594){
  
  
  single_site <- find_best_ref(i)
  
  arr_of_best <- rbind(arr_of_best,single_site)
  
}

arr_of_best2 <- arr_of_best

arr_of_best <- left_join(arr_of_best,
                         driver_data |> unnest(site_info) |>
                           dplyr::select(sitename, koeppen_code),
                         by ="sitename")

arr_of_best$delta_ref <- arr_of_best$reference_height - arr_of_best$canopy_height


converged <- arr_of_best |>
  filter(delta_ref < 9.5) |>
  filter(koeppen_code != "Dfc") |> 
  filter(koeppen_code != "Dsc") |> 
  filter(koeppen_code != "Dsb") 


ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = converged, aes(x = lon, y = lat, color = delta), size = 3) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "sim - obs yearly aet (mm)", x = "Longitude", y = "Latitude")

total_error <- sum(abs(converged$error))

print(paste0("multi year mean error (mm): ",total_error / 594))

print(paste0("mean daily error (mm): ",total_error / (594*366)))


ggplot(converged,aes(x = error)) + geom_histogram(bins = 50, fill = "white", colour = "black") +
  geom_vline(xintercept = mean(abs(converged$error)), color = "red") +
  xlab("21 year period error (mm)")

ggplot(converged,aes(x = delta)) + geom_histogram(bins = 50, fill = "white", colour = "black") +
  geom_vline(xintercept = mean(converged$delta), color = "red") +
  xlab("21 year period delta (mm)")

ggplot(converged, aes(x = koeppen_code, y = delta)) + geom_boxplot() + geom_jitter() +
  ylab("delta (mm)")

####----------

# look in detail at the climates

bad_climate <- driver_data |>
  filter(  koeppen_code == "Dfc" |
           koeppen_code == "Dsb" |
           koeppen_code == "Dsc" )


seasonal_aggregate <- bad_climate |>
  unnest(forcing) |>
  filter(!(month(date)== 2 & day(date) == 29)) |>
  mutate(rain = rain * (24*60*60)) |>
  group_by(sitename,lubridate::yday(date)) |>
  summarise(median_rain = median(rain),
            q33_rain = quantile(rain,0.33),
            q66_rain = quantile(rain,0.66))|>
  rename(doy = `lubridate::yday(date)`)



ggplot(seasonal_aggregate[1:366*5,], aes(x =doy, y = median_rain, color = sitename)) + geom_line()



multi_year$climate <- ifelse(multi_year$koeppen_code == "Dfc"
                         ,"Dfc","other")

multi_year$climate <- ifelse(multi_year$koeppen_code == "Dsc"
                         ,"Dsc",multi_year$climate)

multi_year$climate <- ifelse(multi_year$koeppen_code == "Dsb"
                         ,"Dsb",multi_year$climate)


ggplot(multi_year, aes(x = rain / runoff, y = delta, color=climate)) + 
  geom_point() + scale_x_log10(guide = "axis_logticks") 


ggplot(multi_year, aes(x = pet / rain, y = mod_aet/rain)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1,linetype = "dotted")


low_ratio <- multi_year |>
  mutate(ratio = rain / runoff) |>
  filter(ratio < 3)

ggplot(low_ratio , aes(x = koeppen_code, y =delta)) + geom_boxplot()

ggplot(multi_year, aes(x = rain / runoff, y = delta)) + 
  geom_point() + scale_x_log10(guide = "axis_logticks") 

multi_year <- left_join(multi_year,
                        driver_data |> unnest(site_info) |> dplyr::select(sitename,whc),
                        by = "sitename")

ggplot(multi_year, aes(x = ratio, y = whc,color=climate)) + geom_point() + scale_x_log10(guide = "axis_logticks") 


multi_year$mod_ratio <- multi_year$rain /(multi_year$rain - multi_year$mod_aet)

ggplot(multi_year, aes(x = ratio, y = mod_ratio, colour = climate)) + geom_point() + 
  scale_x_log10(guide = "axis_logticks") + 
  scale_y_log10(guide = "axis_logticks") +
  geom_abline(slope = 1, intercept = 0, color = "red")


ggplot(multi_year, aes(x = koeppen_code, y = log(mod_ratio /ratio))) + geom_boxplot() 

