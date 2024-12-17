library(simmer)
library(simmer.plot)

# library(magrittr)
set.seed(1234)

lambda <- 2
mu <- 4
rho <- lambda/mu

mm1.trajectory <- trajectory() %>%
  seize("sistema", amount=1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("sistema", amount=1)

mm1.env <- simmer() %>%
  add_resource("sistema", capacity=1, queue_size=Inf) %>%
  add_generator("arrival", mm1.trajectory, function() rexp(1, lambda)) %>%
  run(until=2000)


resources <- get_mon_resources(mm1.env)
mm1.arrivals <- get_mon_arrivals(mm1.env)

plot(resources, metric="usage", "sistema", items = c("queue","server"), steps = TRUE)
# plot(resources, metric="utilization", c("resource"))
# plot(mm1.arrivals, metric="waiting_time")

mm1.arrivals <- get_mon_arrivals(mm1.env)
mm1.t_system <- mm1.arrivals$end_time - mm1.arrivals$start_time

mean(mm1.t_system) # Ws (confirmar)
mean(mm1.arrivals$activity_time) # Wq (confirmar)


# https://www.r-bloggers.com/simulating-queueing-systems-with-simmer/

# mm1.N <- rho/(1-rho)
# mm1.T <- mm1.N / lambda
# mm1.T ; mean(mm1.t_system)


