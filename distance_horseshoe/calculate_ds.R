
lnincom_california <- treated %>% filter(between(year, 1972, 1997)) %>% pull(lnincome)
beer_california <- treated %>% filter(between(year, 1984, 1997)) %>% pull(beer)
age15to24_california <- treated %>% filter(between(year, 1970, 1990)) %>% pull(age15to24)
retprice_california <- treated %>% filter(between(year, 1970, 1987)) %>% pull(retprice)


lincome <- control %>%
  select(year, state, lnincome) %>%
  pivot_wider(names_from = state, values_from = lnincome) %>%
  arrange(year)
lincome <- lincome %>% filter(between(year, 1972, 1997)) %>% select(-year) %>% as.matrix()


beer <- control %>%
  select(year, state, beer) %>%
  pivot_wider(names_from = state, values_from = beer) %>%
  arrange(year)
beer <- beer %>% filter(between(year, 1984, 1997)) %>% select(-year) %>% as.matrix()


age15to24 <- control %>%
  select(year, state, age15to24) %>%
  pivot_wider(names_from = state, values_from = age15to24) %>%
  arrange(year)
age15to24 <- age15to24 %>% filter(between(year, 1970, 1990)) %>% select(-year) %>% as.matrix()


retprice <- control %>%
  select(year, state, retprice) %>%
  pivot_wider(names_from = state, values_from = retprice) %>%
  arrange(year)
retprice <- retprice %>% filter(between(year, 1970, 1987)) %>% select(-year) %>% as.matrix()

p <- ncol(lincome)
k_d <- 0.5


library(readxl)
spatial_data =read_excel(".\\distance_horseshoe\\spatial_ds.xlsx")
spatial_ds <- spatial_data$spatial_ds
S=mean(spatial_ds)

# Declare a vector `ds` of length p to store distances from California to other states
ds <- numeric(p)
# Calculate the mean values of lnincome, beer, age15to24, retprice for California and store as vector `california_x`
california_x <- c(
  mean(lnincom_california, na.rm = TRUE),
  mean(beer_california, na.rm = TRUE),
  mean(age15to24_california, na.rm = TRUE),
  mean(retprice_california, na.rm = TRUE)
)

for (j in 1:p) {
  # For each state, compute the mean of lnincome, beer, age15to24, and retprice and store as vector `xj`
  xj <- c(
    mean(lincome[[j]], na.rm = TRUE),
    mean(beer[[j]], na.rm = TRUE),
    mean(age15to24[[j]], na.rm = TRUE),
    mean(retprice[[j]], na.rm = TRUE)
  )
  L2_norms <- sqrt(sum((california_x - xj)^2))
  ds[j] <- k_d / (1 + L2_norms)/0.07121154  + (1-k_d)*spatial_ds[j]/S
  # Where 0.07121154 is the mean value of the term k_d / (1 + L2_norms)
  
  }
  
ds <- ds * 10
