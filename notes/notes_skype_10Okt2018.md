### Skype Notes Britta 10.10.

#### Speaker utilities
Michael uses the literal listener's expected value for the respective utterance and takes the Kulback-Leibler divergence of this average with the speaker's state as utility. - makes it much faster.

$\alpha \rightarrow \infty$ usually makes all possible outcomes unless the
one with the highest probability score vanish. In our model, this was not the case since we take the Kulback-Leibler divergence of the speaker's state with each sample drawn from the literal listener. Therefore we have several executions for one utterance. We add $\alpha * KLDiv(s, s_{LL})$ to the log probability of each execution ($s:$ speaker's state and $s_{LL}$ a state sampled from literal listener, i.e. a probability table).
$\alpha$ only influences our model up to a certain value. At that point, the speaker's distribution does not change anymore for increasing $\alpha$ values.

  * Probability of one execution with $s_{LL}$ being a specific table from literal listener distribution;

      1. $P_s(u,s_{LL}|s) \propto P(u) * P_{LL}(s_{LL} | u)$

  * We add $\alpha * KL(s,s_{LL})$ to the log probability:

      2. $log P_s(u,s_{LL}| s) \propto log P(u)+ log(P_{LL}(s_{LL}|u)) + \alpha * KL(s,s_{LL})$

  * Then we have:

      3. $P_s(u,s_{LL}|s) \propto P(u) * P_{LL}(s_{LL}|u)* exp(\alpha * KL(s, s_{LL}))$

  * and therefore

      4. $P_s(u|s) \propto \sum_i P(u) * P_{LL}(s_{LL_i}|u) * exp(\alpha * KL(s,s_{LL_i}))$

  * Final log Probability

    $log P_s(u|s) \propto log P(u) + log \sum_i (P_{LL}(s_{LL_i}|u) * exp(KL(s,s_{LL_i}) * \alpha))$

  *  $P_{LL}(s_{LL_i}|u)$ is 1 divided by the number of tables making utterance $u$ true for all $i$ (assuming all tables are unique), we consider $\boldsymbol{log P_{LL}(s_{LL_i}|u) + log(\sum_i exp(KL(s,s_{LL}) * \alpha))}$. Normally, when $\alpha$ is increasing, the most probable utterance should gain probability mass while the other utterance looses some. In our model so far, it can happen that an utterance with low probability for small $\alpha$ values, gets higher probability for increasing $\alpha$ values (e.g. my speaker model with $p=[0.048, 0.617, 0.003, 0.332]$). When $\alpha$ is rather small, the number of tables turning the respective utterances true still has a big impact. With increasing $\alpha$, however, the sum of the log becomes smaller and the log values therefore become more negative. Then, the Kullback-Leibler divergence is what drives the result and not the number of tabels.

  * Taking the Literal listener's average avoids this problem.

  * Now the information of how the tables were generated is lost (different for each Bayes Net) *think todo*

#### Generation of Probability Tables
* *skewed prior probability tables*

  Instead of sampling the conditional probabilities for the conditional Bayes Nets each from a uniform distribution or one from beta(1,8) and one from beta(8,1) such that P(C|A) is either high or low and P(C|not A) is on the other extreme, Michael samples the positive conditional probability from beta(10, 1) but the negative conditional probability is still sampled from a uniform distribution.  

#### Literal listener
The causalNet is transmitted down to the Literal listener. The speaker then reasons about a 'half-informed literal listener' since the literal listener knows what the causalNet under discussion is. Literal listener is conditioned on causalNet.


#### QUD-approach
the speaker's qud is globally defined, when the speaker wants to talk about the event A, he always only considers for instance only $P(A)$ instead of the entire probability table.

### ...
Try rejection sampling instead of MCMC.
