### Models for causal inference
* Aim at solving causal inference problem: Hearing 'If A, C' should make listener infer the bayes net 'A implies C'

* speaker utility is the KL-divergence between the literal listener's average prob table and the prob table that the speaker wants to transmit
*
#### listener-causal-inference.wppl
*  positive results when using only bayes nets 'A implies C', 'C implies A' and 'A||C'. *This was done with MCMC, for rejection sampling, the result is negative..., but when more utterances are used, A implies C is slightly more likely, also with rejection sampling.*

* Prior for P(C|A) for BN 'A implies C' is sampled from beta(0.5,0.5), i.e. A given C is assumed to be rather high or low, but there is still a relatively high probability on values in middle range. The prior for P(C|not A) is sampled from a uniform distribution.

* Prior on Bayes Nets is uniform

* when bayes nets 'not A implies C' and 'not C implies A' are also included, the result is not as desired: P(A implies C) + P(not A implies C) < P(C implies A) + P(not C implies A)

* but positive results with the prior beta(10,1) for P(C|A) for BN 'A implies C' and also for P(A|C) for BN 'C implies' and the conditional probability with the antecedent negated is in both bayes nets sampled from a uniform(0,1) distribution. This is essentially the same as Michaels first model, but without conditioning on the base net in the literal listener.


#### listener-networkPriors-biscuits-cp.wppl
* same code as for causal inference but with different priors on the Bayes Networks:

* Interpretation of the utterance 'If A, C' as a *Biscuit conditional*, i.e. the listener infers that the consequent is very likely true independent of the antecedent.
  - Prior: very low on conditional Bayes Nets and very high on independent Bayes Net

* Interpretation of the utterance 'If A, C' as *Conditional Perfection*, i.e. the listener interprets it as biconditional. So P(C|A) ~ P(A|C)
