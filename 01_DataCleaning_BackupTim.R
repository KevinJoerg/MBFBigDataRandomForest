# # convert to df
# carListings.df <- data.frame(carListingsClean)
# 
# # check RAM allocation
# pryr::object_size(carListings.df)
# pryr::object_size(carListingsClean)
# 
# remove(carListingsClean)
# 
# cat(tracemem(carListings.df), "\n")
# 
# ### DATA CLEANING ### -------------------------------------------
# 
# # convert all "" to NAs
# carListings.df <- carListings.df %>% mutate_all(na_if,"")
# 
# # quick plot of NAs
# count_nas <- colSums(is.na(carListings.df))/nrow(carListings.df)
# sorted <- rev(sort(count_nas))
# barplot(sorted, cex.names = 0.5, las = 2)
# title(main = '% NAs in used cars data')
# 
# # quick plot of NAs
# count_nas <- colSums(is.na(carListings.df))/nrow(carListings.df)
# sorted <- rev(sort(count_nas))
# barplot(sorted, cex.names = 0.5, las = 2)
# title(main = '% NAs in used cars data')
# 
# # drop all variables for which we have less than 50% observations
# omit <- names(which(sorted>=0.2))
# carListings.df <- carListings.df %>% select(-omit)
# remove(omit)
# 
# # let us clean and convert the variables correctly
# colnames <- c(vin = 'character', 
#               back_legroom = 'numeric', 
#               body_type = 'factor', 
#               city = 'character', 
#               city_fuel_economy = 'numeric', 
#               daysonmarket = 'numeric', 
#               dealer_zip = 'numeric',
#               description = 'character',
#               engine_cylinders = 'factor', # should be separated
#               engine_displacement = 'numeric', # ?
#               engine_type = 'numeric', # ?
#               exterior_color = 'factor', # should be broken down
#               franchise_dealer = 'boolean', 
#               franchise_make = 'factor', # either this or make_name
#               front_legroom  = 'numeric', 
#               fuel_tank_volume  = 'numeric', 
#               fuel_type = 'factor', 
#               height = 'numeric', 
#               highway_fuel_economy = 'numeric', 
#               horsepower = 'numeric', 
#               interior_color = 'character', # should be broken down
#               is_new  = 'boolean', 
#               latitude  = 'numeric', 
#               length = 'numeric', 
#               listed_date = 'date', 
#               listing_color  = 'factor', # could be taken as alternatives for colors
#               listing_id  = 'numeric', 
#               longitude = 'numeric', 
#               main_picture_url = 'character', 
#               major_options = 'character', # should be separated
#               make_name = 'factor', # either this or franchise_make
#               maximum_seating = 'numeric', 
#               mileage = 'numeric', 
#               model_name = 'character', # too many
#               power = 'character', # should be split in multiple
#               price = 'numeric', 
#               savings_amount = 'numeric', # ?
#               seller_rating = 'numeric', 
#               sp_id = 'numeric', 
#               sp_name = 'character', 
#               torque = 'numeric', 
#               transmission = 'factor', 
#               transmission_display = 'character', # can be omitted as we already have transmission 
#               trimId = 'numeric', 
#               trim_name = 'character', 
#               wheel_system = 'factor', 
#               wheel_system_display = 'factor', # can be omitted 
#               wheelbase = 'character', # needs to be separated
#               width = 'numeric', 
#               year = 'numeric', 
#               state = 'factor', 
#               county = 'character', # too many
#               DemRepRatio = 'numeric'
# )
# 
# # make conversions
# for (i in colnames(carListings.df)) {
#   if (colnames[i][[1]] == 'numeric') {
#     carListings.df[[i]] <- as.numeric(carListings.df[[i]])
#   } else if (colnames[[i]] == 'factor'){
#     carListings.df[[i]] <- as.factor(carListings.df[[i]])
#   } else if (colnames[[i]] == 'boolean'){
#     carListings.df[[i]] <- as.boolean(carListings.df[[i]])
#   } else if (colnames[[i]] == 'character'){
#     carListings.df[[i]] <- as.character(carListings.df[[i]])
#   } else if (colnames[[i]] == 'date'){
#     carListings.df[[i]] <- as.Date(carListings.df[[i]])
#   }
# }
# 
# 
# # check if it worked
# str(carListings.df)
# 
# # let us omit more variables we don't want
# omit <- c('vin', 'description', 'dealer_zip', 'listing_id', 'main_picture_url', 'sp_id', 'sp_name', 
#           'transmission_display',  'trimId', 'trim_name', 'wheel_system_display', 
#           'exterior_color', 'interior_color', # as we already account for other color
#          'franchise_make', # as we already account for the brand
#          'major_options', # too detailed
#          'model_name' # too many options
# )
# carListings.df <- carListings.df %>% select(-omit)
# 
# # rename columns to make it clear where the data comes from
# carListings.df <- carListings.df %>% rename(
#   DemRep_state = state, 
#   DemRep_county = county
# )
# 
# 
# # separate variable "power" into "hp" and into "RPM"
# list <- (str_split(carListings.df$power, " "))
# carListings.df$hp <- as.numeric(lapply(list, '[[', 1))
# 
# # but these are almost the same, so we can delete $hp
# identical(carListings.df[['horsepower']],carListings.df[['hp']])
# tail(carListings.df$horsepower)
# tail(carListings.df$hp)
# head(carListings.df$horsepower)
# head(carListings.df$hp)
# carListings.df <- carListings.df %>% dplyr::select(-hp)
# 
# # as lapply doesn't work with errors, we write our own function to ignore those errors
# testFunction <- function (x) {
#   return(tryCatch("[["(x, 4), error=function(e) NULL))}
# 
# rpm <- lapply(list, testFunction)
# carListings.df$rpm <- as.numeric(str_replace(rpm, ",", ""))
# carListings.df <- carListings.df %>% dplyr::select(-power)
# 
# # separate number from ".. in" in the variable "wheelbase"
# head(carListings.df$wheelbase)
# list <- str_split(carListings.df$wheelbase, " in")
# carListings.df$wheelbase <- as.numeric(lapply(list, '[[', 1))
# 
# # simplify engine_cylinders by allowing for less variations
# head(carListings.df$engine_cylinders)
# list <- str_split(carListings.df$engine_cylinders, " ")
# carListings.df$engine_cylinders <- as.factor(as.character(lapply(list, '[[', 1)))
# 
# # from Date to Month and Year
# carListings.df$month <- month(carListings.df$listed_date)
# carListings.df$year <- year(carListings.df$listed_date)
# 
# 
# # remove unnecessary variables
# remove(list, rpm, colnames, count_nas, i, omit, sorted, testFunction)
# 
# 
# # # create directory for ff chunks, and assign directory to ff 
# # system("mkdir ffdf_tim")
# # options(fftempdir = "ffdf_tim")
# # 
# # # Saving it this way names the files by colnames
# # carListings.ffdf <- as.ffdf(carListings.df %>% mutate_if(is.character, as.factor))
# # save.ffdf(carListings.ffdf, dir = './ffdf_tim', overwrite = TRUE)
# # 
# # load.ffdf(dir='./ffdf_tim')
# # load.ffdf(dir = './ffdf')
# 
# 
# ### Regression ### -----------------------------------------
# library(speedglm)
# library(biglm)
# 
# carListings.short <- na.omit(carListings.df)
# 
# str(carListings.df)
# 
# # simple lm will work
# summary(lm(DemRepRatio ~ price + mileage + horsepower + transmission, data = carListings.df))
# # however not for the whole dataset as the vector memory gets exhausted 
# lm(DemRepRatio ~ ., data = carListings.df)
# 
# 
# # speed glm aslo exhausts the limit
# speedlm(DemRepRatio ~., data= carListings.df)
# #cp(as.matrix(carListings.df), row.chunk = 1000) # in order for this to work, the dat needs to be a matrix with only numerics
# 
# 
# summary(bigglm(DemRepRatio ~ price + mileage + horsepower + transmission, data = carListings.df))
# 
# 
# # neither does bigglm work for a single predicter
# bigglm.ffdf(as.numeric(carListings.df$DemRepRatio) ~ as.numeric(carListings.df$price), data = carListings.df)
# 
# str(carListings.df)


### BACKUP ### ---------------------------------

# # let us clean and convert the variables correctly
# colnames <- c(vin = 'character', 
#               back_legroom = 'numeric', 
#               bed = 'factor', 
#               bed_height = 'numeric', 
#               bed_length = 'numeric', 
#               body_type = 'factor', 
#               cabin = 'factor', 
#               city = 'character', 
#               city_fuel_economy = 'numeric', 
#               combine_fuel_economy = 'boolean', 
#               daysonmarket = 'numeric', 
#               dealer_zip = 'numeric',
#               description = 'character',
#               engine_cylinders = 'numeric',
#               engine_displacement = 'numeric', 
#               engine_type = 'numeric', 
#               exterior_color = 'numeric', 
#               fleet = 'boolean', 
#               frame_damaged = 'boolean', 
#               franchise_dealer = 'boolean', 
#               franchise_make = 'character', 
#               front_legroom  = 'numeric', 
#               fuel_tank_volume  = 'numeric', 
#               fuel_type = 'character', 
#               has_accidents = 'boolean', 
#               height = 'numeric', 
#               highway_fuel_economy = 'numeric', 
#               horsepower = 'numeric', 
#               interior_color = 'character', 
#               isCab = 'boolean', # only 1 level
#               is_certified  = 'boolean', 
#               is_cpo  = 'boolean', 
#               is_new  = 'boolean', 
#               is_oemcpo  = 'boolean', 
#               latitude  = 'numeric', 
#               length = 'numeric', 
#               listed_date = 'date', 
#               listing_color  = 'character', 
#               listing_id  = 'numeric', 
#               longitude = 'numeric', 
#               main_picture_url = 'character', 
#               major_options = 'character', 
#               make_name = 'character', 
#               maximum_seating = 'numeric', 
#               mileage = 'numeric', 
#               model_name = 'character', 
#               owner_count = 'numeric', 
#               power = 'character', 
#               price = 'numeric', 
#               salvage = 'boolean', 
#               savings_amount = 'numeric', 
#               seller_rating = 'numeric', 
#               sp_id = 'numeric', 
#               sp_name = 'character', 
#               theft_title = 'boolean', 
#               torque = 'numeric', 
#               transmission = 'factor', 
#               transmission_display = 'character', 
#               trimId = 'numeric', 
#               trim_name = 'character', 
#               vehicle_damage_category = 'factor', # only 1 level
#               wheel_system = 'factor', 
#               wheel_system_display = 'factor', 
#               wheelbase = 'character', 
#               width = 'numeric', 
#               year = 'numeric', 
#               state = 'factor', 
#               county = 'factor', 
#               DemRepRatio = 'numeric'
# )
