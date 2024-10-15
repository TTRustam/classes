
library(DemoTools)

# deaths and pop from INE
dx <- readxl::read_excel("C:/Users/rusta/Downloads/deaths.xlsx",
                         sheet= "one")
pop<- readxl::read_excel("C:/Users/rusta/Downloads/pop.xlsx",
                         sheet= "one")

dx <- dx %>%
  mutate(age = parse_number(age),
         year = as.numeric(year)) %>% 
  replace(is.na(.), 0) %>%
  pivot_longer(-c(sex:year),
               names_to = "SEC_PROV",
               values_to = "dx")
pop <- 
  pop %>%
  pivot_longer(-c(sex:SEC_PROV),
               names_to = "age",
               values_to = "pop") %>% 
  mutate(age = parse_number(age),
         year = 2019)


dat <- dx %>% 
  full_join(pop)

a <- dat %>% 
  filter(sex == "Hombres", SEC_PROV == "01 Araba/Álava")
a1 <- dat %>% 
  filter(sex == "Hombres", SEC_PROV == "48 Bizkaia")
a2 <- dat %>% 
  filter(sex == "Hombres", SEC_PROV == "20 Gipuzkoa")
b <- dat %>% 
  filter(sex == "Mujeres", SEC_PROV == "01 Araba/Álava")
b1 <- dat %>% 
  filter(sex == "Mujeres", SEC_PROV == "48 Bizkaia")
b2 <- dat %>% 
  filter(sex == "Mujeres", SEC_PROV == "20 Gipuzkoa")

write.xlsx(x = a, file = "dx.xlsx", 
           sheetName = "hom_01",
           append = TRUE)
write.xlsx(x = a1, file = "dx.xlsx", 
           sheetName = "hom_48",
           append = TRUE)
write.xlsx(x = a2, file = "dx.xlsx", 
           sheetName = "hom_20",
           append = TRUE)
write.xlsx(x = b, file = "dx.xlsx", 
           sheetName = "muj_01",
           append = TRUE)
write.xlsx(x = b1, file = "dx.xlsx", 
           sheetName = "muj_48",
           append = TRUE)
write.xlsx(x = b2, file = "dx.xlsx", 
           sheetName = "muj_20",
           append = TRUE)
