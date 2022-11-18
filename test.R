#Function to find 12 1s consecutively
test_vec.12 <- rep(0, 48)

test_vec.12[10:21] <- 1
test_vec.12

test_vec.11 <- rep(0, 48)
test_vec.11[10:20] <- 1
test_vec.11


get_12 <- fuction(vector){ 
  x <- 0
  for ( i in seq_along( vector ) ) {
    if ( vector[i] == 1 && x < 12 ){
      x <- x + 1
      print( x )
    } else if ( x == 12 ){
      message("TRUE")
    } else{
      
    }
  }
}

library(zoo)

rollapply(test_vec.12,)

library(data.table)

twelve <- rep(1,12)

motor_12 <- frollapply(hur_sel$motor_on, length(twelve), identical, twelve)


test_first.12 <- rep(0,48)
test_first.12[1:12] <- 1
test_first.12[24:35] <- 1
test_first.12

hur_sel %>% 
  group_by(ID) %>% 
  map()


interval <- seq(from =0, to = 36960, by = 48)

interval <- interval[-1]

motor_12_split <- split( motor_12, ceiling( seq_along( motor_12 ) / 48 ) )

hur_sel %>% 
  filter(ID == 27) %>% 
  View()

motor_12_split <- split( motor_12, ceiling( seq_along( motor_12 ) / 48 ) )



motor_12_split_test <- split( hur_sel$motor_on, ceiling( seq_along( motor_12 ) / 48 ) )
motor_12_test <- frollapply(motor_12_split_test, length(twelve), identical, twelve)