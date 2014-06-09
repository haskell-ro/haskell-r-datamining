generateSequence <- function(n) {
	return (sample(c("A", "C", "G", "T"), size=n, replace=TRUE))
}

generateDataPoints <- function(n, sz) {
	return (replicate(n, generateSequence(sz)))
}

distance <- function(u, v) {
	for (i in 1:length(u))
		if (u[i] != v[i])
			break
	if (u[i] == v[i])
		return (0)
	return (2^{-(i-1)})
}

cluster <- function(seqs, a) {
	n <- dim(seqs)[2]
	w <- 1:n
	cluster <- replicate(n, NA)
	c = 1
	while (length(w) > 1) {
		s <- sample(w, 1)
		# looks somehow ugly but it's higher order function too
		ds <- sapply(w, function(x) distance(seqs[,x], seqs[,s]))
		cluster[w[ds < a]] = c
		w <- w[ds >= a]
		c = c + 1
	}
	cluster[w] = c
	structure(list(cluster=cluster), class="umcls")
}

# call from R with
# > cluster(generateDataPoints(100, 10), 0.5)
