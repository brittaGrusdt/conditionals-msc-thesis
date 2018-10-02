what should the model be able to do?

- speaker uncertainty about A and about C
- unconditionals readings
	- prior influence, seems okay
- conditional perfection
	- try different priors for Bayes Nets and probTable construction
- causal inference "if A, C" should imply BN "C depends on A"

things to try:
- check for which prob table the speaker will use "if A, C" for the "wrong/unintuitive" Bayes Net
- alternative utterances to consider: 
	- conjunctions "A & C"
	- models such as "probably A"
	
things to check:
- MCMC diagnostic

future music:
- QUD
	- look at questions "C?" and "what if A?" to get or not get perfection readings
	- if the speaker addresses QUD "C?", she selections messages using KL-divergence between
      the marginalized belief about "C" only; similarly, for "what if A?" it is the
      distribution conditionalized on A only
- Wason selection task
	- where Oaksford and Chater look at their parameterized matrixes and ask how much each
      turning of a card would contribute to telling the Dependence and Independence models
      apart, we could look at the set of all probTables inferred by the pragmatic listener
      after hearing "if A, C" and after hearing anything else / the null utterance / or just
      the prior; we could then quantify the observational evidence of each card turn towards
      distinguishing these hypotheses: we are in a world in which "if A, C" is pragmatically
      appropriate vs. we are in a world in which it is not (necessarily)
