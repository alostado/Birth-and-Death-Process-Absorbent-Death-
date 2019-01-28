# Importing Relevant Libraries --------------------------------------------

library(ggplot2)

# Initializing Relevant Variables -----------------------------------------

lambda = runif(100);
num_transitions = 100;

Tvect = rexp(num_transitions, rate = lambda);

tot_time = sum(Tvect);

birth_prob = runif(100);
start_pop = 10;

bod = 2*rbinom(num_transitions, 1, birth_prob) - 1;
bod[1] = start_pop;

absorb_idx = 0;

# Modifying Vectors for Easier Plotting -----------------------------------

for (i in 2:num_transitions) {
  Tvect[i] = Tvect[i-1] + Tvect[i];
  bod[i] = bod[i-1] + bod[i];
}

for (i in 2:num_transitions) {
  if (bod[i-1] == 0) {
    bod[i] = 0;
    absorb_idx = absorb_idx + 1;
  }
}

absorb_idx = num_transitions - absorb_idx;


# Plotting Generated Data -------------------------------------------------

ggplot() +
  geom_step(aes(x=Tvect, y=bod)) +
  theme_bw() +
  labs(title = "Birth and Death Process \nwhere Pop = 0 is Absorbent", 
       x = "Time", y = "Population") +
  xlim(0,tot_time) +
  ylim(0,max(bod)) + 
  theme(plot.title = element_text(hjust = 0.5))
