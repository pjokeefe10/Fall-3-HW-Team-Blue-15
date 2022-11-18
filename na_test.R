hurricane %>% 
  filter(if_any(everything(), ~is.na(.x))) -> na_pump



na_ids <- na_pump$ID
na_hour <- na_pump$hour


na_pump_test <- na_pump
na_pump_test[is.na(na_pump_test)] <- 0

for ( i in seq_along( na_ids ) ){
  x <- na_hour[[i]] + 9
  na_pump_test[ i , x:56 ] <- NA
}

hurr_test <- hurricane

for ( i in seq_along( na_ids ) ){
  hurr_test[ which( hurr_test$ID == na_ids[[i]] ), ] <- 
    na_pump_test[which(na_pump_test$ID == na_ids[[i]]), ]
}

na_pump_test[which(na_pump_test$ID == na_ids[[2]]), ] 

na_pump_test[1, 56:56]


na_ids <- hurricane$ID
na_hour <- hurricane$hour

hurricane_t <- hurricane
hurricane_t[ is.na( hurricane_t ) ] <- 0

for ( i in seq_along( na_ids ) ){
  if ( na_hour[[i]] == 48 ){
    
  }else{
    x <- na_hour[[i]] + 9
    hurricane_t[ i , x:56 ] <- NA
  }
}
