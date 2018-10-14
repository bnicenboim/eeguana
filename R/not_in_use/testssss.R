# I want to use env_get to evaluate a variable in the parent environment of the mutate call, but I didn't manage

# Given a data frame like the following
	library(dplyr)
	
	l <- list(X = 10,
			df = tibble(n = seq(-10,10), y = rnorm(21), z = runif(21)))

	attributes(l$df$n)$sampling_rate <- 999
# And custom mutate for these lists.

	mutate_.list <- function(.data, ...){
		# attributes(.data$df$n)$sampling_rate <- 10
		.data$df <- mutate_(.data$df,  ...)
		.data
	}


# I want a function that can be run inside the mutate and can use the value of
# X. Something like the following which doesn't work

	addX <- function(x) {

		n <-  rlang::env_get(env = parent.frame(), 'n', inherit = TRUE)
		Xx <- attributes(n)$sampling_rate
		x + Xx
	}

ll <-	mutate(l, x = addX(n)) %>% mutate( zz =addX(x))
attributes(ll$df$n)


# And this doesn't work. I guess I should go up parents somehow and be able to
# refer to the list, but I couldn't manage.

	addX_test <- function(x) {
		theenv <- rlang::env_get(env = parent.frame(), '.top_env', inherit = TRUE)
		tibble(rlang::env_get_list(theenv, rlang::env_names(theenv))  )
		x 
	}

	mutate(l, addX_test(x))

# I tried to see if I could find something using "env_names" but I get stuff like
# [1] "~"                        ".top_env"                
# [3] ".__tidyeval_data_mask__." ".env"

rme <- function(cols){
	theenv <- rlang::env_get(env = parent.frame(), '.top_env', inherit = TRUE)
	signal <- dplyr::as_tibble(rlang::env_get_list(theenv, rlang::env_names(theenv))  )
	# print(signal)
	r <-rowMeans(signal[, cols])
    # print(r)
    r
}


data <- list(X =100, df =data.frame(id=c(101,102,103), a=c(1,2,3), b=c(2,2,2), c=c(3,3,3)))
data %>% mutate(d = rowMeans(.$df[, c("a","b","c")]))
data %>% mutate(d = rowMeans(select(.$df, a,b,c)))
data %>% mutate(d = rme(c("a","b","c")))
