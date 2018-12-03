# Randomly select posterior distributions ------------------------------------------
# load data
a_posterior <- read.csv("trait_fits/biting_rate.csv", head=T)
pEA_posterior <- read.csv("trait_fits/egg_to_adult_survival_probability.csv", head=T)
EFD_posterior <- read.csv("trait_fits/eggs_per_female_per_day.csv", head=T)
mu_posterior <- read.csv("trait_fits/lifespan.csv", head=T)
MDR_posterior <- read.csv("trait_fits/mosquito_developement_rate.csv", head=T)
PDR_posterior <- read.csv("trait_fits/parasite_development_rate.csv", head=T)
b_posterior <- read.csv("trait_fits/vector_competence_b.csv", head=T)
pMI_posterior <- read.csv("trait_fits/vector_competence_c.csv", head=T)

# randomly select rows
posteriors <- list(a_posterior, pEA_posterior, EFD_posterior, mu_posterior, MDR_posterior, PDR_posterior, b_posterior, pMI_posterior)
traitNames <- c("a", "pEA", "EFD", "mu_th", "MDR", "PDR", "b", "pMI")
n = 50 # number of samples
trait_posterior <- data.frame("ID"=seq(1:n))

for (i in 1:length(posteriors)){
  trait.sub <- posteriors[[i]]
  trait.sub <- trait.sub[sample(nrow(trait.sub), n), ]
  if (traitNames[i] == "pEA"|traitNames[i] == "mu_th"){
    colnames(trait.sub)[3] <- "c"
  }
  colnames(trait.sub) <- paste0(traitNames[i], "_", colnames(trait.sub))
  trait_posterior <- cbind(trait_posterior, trait.sub[,1:3])
}

# save data
write.csv(trait_posterior, "Concatenated_Data/Random_sample_of_posterior_traits.csv", row.names = F)