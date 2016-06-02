data {
	int<lower=1>  N; // num observations
	int<lower=1>  K; // num of centers
	int<lower=1>  J; // number of conditions	
	
	vector[N] time; // time points
	int<lower=1> condition[N]; // condition
	vector[K] centers; // time intervals
	vector[N] y; // response vector
}


parameters {
	real<lower=0> width;
	row_vector[K] w[J];
	real<lower=0> sigma;	
	real<lower=0> w_sigma;
	vector[K] w_0;
}

transformed parameters {
	vector[N] mu;
	matrix[N,K] phi;

	for (n in 1:N) {
		for (k in 1:K) {
			phi[n,k] <- exp(- square(time[n] - centers[k]) / square(width) );
		}
		mu[n] <- dot_product(w[condition[n]], phi[n,]);
	}
}

model {
	# Priors
	sigma ~ cauchy(0.001,5);
	w_sigma ~ cauchy(0.001,5);
	width ~ uniform(0.001, 10000);

	w_0 ~ normal(0, w_sigma);
	
	# Define model
	for (j in 1:J) {
		w[j] ~ normal(w_0, sigma);
	}

	y ~ normal(mu, sigma);
}
