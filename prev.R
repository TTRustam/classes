library(tidyverse)
library(DemoTools)
library(readxl)
library(mgcv)
library(xlsx) 

setwd("C:/R_initial_directory/map")
# municipality names
mun_names <- read_excel("MunicipiosCAE_2024_17_01.xlsx", 
                        sheet = "EAE", 
                        skip = 9,
                        col_names = FALSE) %>% 
  set_names(c("SEC_PROV", "SEC_MUNI", "SEC_MUNI_D"))

all_data <- list()

for (yr in 2016:2023) {
  
  yri <- read_delim(paste0("datos/agregados ", yr, "12 capv.csv"), 
                    delim = ";",
                    col_types ="cccddddddddddd") |> 
    mutate(year = yr, .before = 1)
  all_data[[as.character(yr)]] <- yri
  
}
# hta == hypertencion
prev <- bind_rows(all_data) |> 
  dplyr::select(year, codseccion, sex, pob, edadcat, hta)|>
  mutate(age = parse_number(edadcat)) |> 
  arrange(year, codseccion, sex, age) |>
  # remove missing
  filter(!is.na(codseccion)) |> 
  # separate codeseccion in geographical units
  mutate(
    codseccion = str_pad(codseccion, 10, side = "left", pad = "0"),
    SEC_PROV   = substr(codseccion, 1, 2),
    SEC_MUNI   = substr(codseccion, 3, 5),
    SEC_DIST   = substr(codseccion, 6, 7),
    SEC_SECC   = substr(codseccion, 8, 10)) |>
  # add municip names
  left_join(mun_names, by = join_by(SEC_PROV, SEC_MUNI))



smooth_ungroup <- function(.data) {
  
  data <- .data
  
  # Fit the model, adjusting for count data
  model <- gam(
    hta ~ s(age, bs = "cs"),
    data   = data,
    offset = log(pop),
    family = quasipoisson() 
  )
 
  # Create new data for prediction and predict counts
  new_data <- expand_grid(
    age      = 0:100
  )
  
  # Add predictions (counts)
  new_data$prev <- predict(model,
                             newdata = new_data,
                             type    = "response")
  
  
  return(new_data)
}


# calculate standardized prevalence 
prev_st_orig <- prev %>%
  group_by(SEC_PROV, year, sex, age) %>% 
  summarize(hta  = sum(hta), 
            pop  = sum(pob), .groups = "drop") |>
  filter(year == 2019)

save(prev_st_orig, file = "prev.RData")

# apply to province and sex
z <- prev_st_orig %>% 
  group_nest(sex, SEC_PROV) %>% 
  mutate(data1 = map(data, ~ .x %>% 
                       smooth_ungroup())) %>%
  dplyr::select(-data) %>% 
  unnest(data1) %>% 
  mutate(sex = ifelse(sex == "hombre",
                      str_c(sex, "s"),
                      str_c(sex, "es")),
         sex = str_to_title(sex))

# population from INE
pop <- readxl::read_excel("C:/Users/rusta/Downloads/pop.xlsx",
                         sheet= "one") %>%
  pivot_longer(-c(sex:SEC_PROV),
               names_to = "age",
               values_to = "pop") %>% 
  mutate(age = parse_number(age),
         year = 2019) %>% 
  mutate(SEC_PROV = parse_number(SEC_PROV),
         SEC_PROV = ifelse(SEC_PROV == 1, 
                           str_c("0", SEC_PROV),
                           SEC_PROV)) %>% 
  dplyr::select(-year)

# cases
z <- z %>% 
  full_join(pop) %>%
  mutate(casos = round(prev * pop, 0)) %>% 
  dplyr::select(-c(prev, pop))

# save in excel for exporting
a <- z %>% 
  filter(sex == "Hombres", SEC_PROV == "01")
a1 <- z %>% 
  filter(sex == "Hombres", SEC_PROV == "48")
a2 <- z %>% 
  filter(sex == "Hombres", SEC_PROV == "20")
b <- z %>% 
  filter(sex == "Mujeres", SEC_PROV == "01")
b1 <- z %>% 
  filter(sex == "Mujeres", SEC_PROV == "48")
b2 <- z %>% 
  filter(sex == "Mujeres", SEC_PROV == "20")

write.xlsx(x = a, file = "prev.xlsx", 
           sheetName = "hom_01",
           append = TRUE)
write.xlsx(x = a1, file = "prev.xlsx", 
           sheetName = "hom_48",
           append = TRUE)
write.xlsx(x = a2, file = "prev.xlsx", 
           sheetName = "hom_20",
           append = TRUE)
write.xlsx(x = b, file = "prev.xlsx", 
           sheetName = "muj_01",
           append = TRUE)
write.xlsx(x = b1, file = "prev.xlsx", 
           sheetName = "muj_48",
           append = TRUE)
write.xlsx(x = b2, file = "prev.xlsx", 
           sheetName = "muj_20",
           append = TRUE)