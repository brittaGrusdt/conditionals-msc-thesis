definitions <- '
globalStore.alpha = 5
globalStore.thetaCP = 0.05
var thresholds = {t: 0.9, f: 0.05, likely: 0.7, theta: 0.9, theta_likely: 0.5}

var literals = ["A", "C", "-A", "-C"]
var likely = map(function(u){return "likely ".concat(u)}, literals)
var conditionals = ["If A, C", "If A, -C", "If -A, C", "If -A, -C",
"If C, A", "If C, -A", "If -C, A", "If -C, -A"]
var conjunctions = ["A and C", "C but -A", "A but -C", "neither A nor C"]

var utteranceDict = {"literals": literals,
"if" : conditionals,
"likely": likely,
"conj" : conjunctions
}
var utterances = reduce(function(utts,acc){acc.concat(utts)},[],
Object.values(utteranceDict))

var independentNets = ["A ind. C"]
var dependentAC = ["A implies C", "A implies -C", "-A implies C", "-A implies -C"]
var dependentCA = ["C implies A", "C implies -A", "-C implies A", "-C implies -A"]
var dependentNets = dependentAC.concat(dependentCA)
var causalNets = independentNets.concat(dependentNets)

var prob_ac_ind = {"none": 1/2,
"lawn": 1/2,
"lawn-cond": 1/2,
"pizza" : 1,
"douven1": 0.98,
"BC-impossible-cons": 1
}
'


helpers <- '
var roundToN = function(x, n){
var m = Math.pow(10,n)
return Math.round(x*m)/m
}

// marginal probabilities
var pa = function(x){return x[0]+x[2]}
var pc = function(x){return x[0]+x[1]}
var pna = function(x){return x[1]+x[3]}
var pnc = function(x){return x[2]+x[3]}

// conditional probabilities
var pAgivenC = function(x){return x[0]/pc(x)}
var pAgivenNC = function(x){return x[2]/pnc(x)}
var pNAgivenC = function(x){return x[1]/pc(x)}
var pNAgivenNC = function(x){return x[3]/pnc(x)}
var pCgivenA = function(x){return x[0]/pa(x)}
var pCgivenNA = function(x){return x[1]/pna(x)}

var utteranceProbs = cache(function(utterance, table){
var p =
utterance == "A" || utterance == "likely A" ? pa(table) :
utterance == "C" || utterance == "likely C"? pc(table) :
utterance == "-A" || utterance == "likely -A" ? pna(table) :
utterance == "-C" || utterance == "likely -C" ? pnc(table) :
utterance == "If A, C" ? table[0] / pa(table) :
utterance == "If A, -C" ? table[2] / pa(table) :
utterance == "If -A, C" ? table[1] / pna(table) :
utterance == "If -A, -C" ? table[3] / pna(table) :
utterance == "If C, A" ? table[0] / pc(table) :
utterance == "If C, -A" ? table[1] / pc(table) :
utterance == "If -C, A" ? table[2] / pnc(table):
utterance == "If -C, -A" ? table[3] / pnc(table) :
utterance == "A and C" ? table[0] :
utterance == "neither A nor C" ? table[3] :
utterance == "A but -C" ? table[2] :
utterance == "C but -A" ? table[1] : error("unknown utterance " + utterance)
return p
})
'

priors <- '
var tablesPrior = mem(function(){
return Infer({method:"forward", samples:10000, model:function(){
var alpha = Vector(repeat(4, constF(1)));
var vec = dirichlet({alpha})
var table = Object.values(vec.data)
// var table = map2(roundToN, table, [4,4,4,4])
return table
}})})

var tables = tablesPrior().support()
display("nb tables: " + tables.length)

var logLikelihood = cache(function(table, cn){
var p = cn=="A implies C" ? pCgivenA(table) :
cn=="A implies -C" ? 1-pCgivenA(table)  :
cn=="-A implies C" ? pCgivenNA(table) :
cn=="-A implies -C" ? 1-pCgivenNA(table) :
cn=="C implies A"  ? pAgivenC(table):
cn=="C implies -A" ? 1-pAgivenC(table) :
cn=="-C implies A" ? pAgivenNC(table) :
cn=="-C implies -A" ? 1-pAgivenNC(table) :
cn=="A ind. C" ?
(Math.abs(pCgivenA(table) - pCgivenNA(table)) <= 0.03 &&
Math.abs(pAgivenC(table) - pAgivenNC(table)) <= 0.03 ?
0.99 : 0.01) :

error("unknown cn in likelihood: " + cn)

var logL = Beta({a:10, b:1}).score(p)

return logL
})

var getCNpriors = function(p_ind){
return [p_ind].concat(repeat(dependentNets.length,
function(){(1-p_ind)/dependentNets.length}))
}

var networkPrior = function(bias) {
return categorical({vs: causalNets, ps: getCNpriors(prob_ac_ind[bias])})
}

var bayesNetPrior = cache(function(bias){
return Infer({method:"enumerate", model:function(){
var table = uniformDraw(tables)
var cn = networkPrior(bias)
factor(logLikelihood(table, cn))

if(bias=="lawn"){
if(table[1]<=globalStore.thetaCP){
factor(-Math.log(thresholds.f))
}
}else if(bias=="lawn-cond"){
if(pCgivenNA(table)<=globalStore.thetaCP){
factor(-Math.log(thresholds.f))
}
}else if(bias=="douven1"){
if(pc(table)>=thresholds.likely){factor(-Math.log(thresholds.f))}
}else if(bias=="BC-impossible-cons"){
condition(pc(table)<=thresholds.f)
}
return {cn, table}
}})
})
'

rsa_model <- '
var meaning = cache(function(utterance, table, cn){
var p = utteranceProbs(utterance, table)
var u_applicable = utterance.includes("likely") ?
(p >= thresholds.theta_likely) : p >= thresholds.theta
return u_applicable
})

var literalListener = cache(function(utterance, bias){
Infer({method:"enumerate",model: function(){
var bn = sample(bayesNetPrior(bias))
condition(meaning(utterance, bn.table))
return bn
}})
}, 10000)

var costs = function(utt){
if(!utterances.includes(utt)){error("unknown utterance " + utt)}
var c1 = utt.includes("If") ? 0.55 : 0
var c2 = utt.includes("and") || utt.includes("but") ? 0.25 : 0
var c3 = utt.includes("-A") || utt.includes("neither") ? 0.125 : 0
var c4 = utt.includes("-C") || utt.includes("nor") ? 0.125 : 0
var c5 = utt.includes("likely") ? 0.1 : 0
var cost = c1 + c2 + c3 + c4 + c5

return cost
}

var speaker = cache(function(bn, bias, printU){
return Infer({method:"enumerate", model: function(){
var utterance = uniformDraw(utterances)
var LL = literalListener(utterance, bias)
var utility = LL.score(bn)
if(printU && utility!=-Infinity){
print(utterance + utility)
}
factor(globalStore.alpha * (utility - costs(utterance)))
// factor(globalStore.alpha * utility)
return utterance
}
})
}, 10000)

var listener = function(utterance, bias){
return Infer({method:"enumerate", model:function(){
var bn = sample(bayesNetPrior(bias))
observe(speaker(bn, bias, false),utterance)
return {bn}
}})
}
'

analysis <- '
var conditions = {
"pnc": function(bn){return pc(bn.table)<thresholds.theta},
"pc": function(bn){return pc(bn.table)>=thresholds.theta &&
bn.table[0]<thresholds.theta &&
bn.table[1]<thresholds.theta},
"pca": function(bn){return bn.table[0]>=thresholds.theta},
"pcna": function(bn){return bn.table[1]>=thresholds.theta},
"no-condition": function(bn){return true}
}

var speakerExpectations = function(n, bias, cond){
var prior = bayesNetPrior(bias)
var prior = Infer({method:"enumerate", model:function(){
var bn = sample(prior)

var condFn = conditions[cond]
var condHolds = condFn(bn)
condition(condHolds)

return bn
}})
display("# states in prior support: " +  prior.support().length)
display("sample BNs...")
var bns = repeat(n, function(){return sample(prior)})
display("# states: " + bns.length)

var likelihoods = map(function(utt){
return sum(map(function(bn){
var speakerBeliefs = speaker(bn, bias, false)
var likelihood = Math.exp(speakerBeliefs.score(utt))
return likelihood}, bns))/n
}, utterances)

return zip(utterances, likelihoods)
}
'
setup <- function(bias, utterance, lt, alpha, lawn_theta, bn_str='{table:[], cn:""}'){
  var_bias <- paste('var bias = "', bias, '"', sep="")
  var_utt <- paste('var utterance = "', utterance, '"', sep="")
  var_lt <- paste('var lt ="', lt, '"', sep="")
  var_alpha <- paste('globalStore.alpha = ', alpha)
  var_lawn_theta <- paste('globalStore.thetaCP =', lawn_theta)
  var_speaker_bn <- paste('var bn = ', bn_str)
  display_params <- 'display("bias: " + bias + " utterance: " + utterance)
  display("alpha: " + globalStore.alpha)
  display("theta CP: " + globalStore.thetaCP)
  '
  params_str <- paste(var_bias, var_utt, var_lt, var_alpha, var_lawn_theta, var_speaker_bn,
                      display_params, sep="\n")
  return(params_str)
}

CALL_MODEL <- 'var posterior =  
lt=="PL"    ? listener(utterance, bias) :
lt=="LL"    ? literalListener(utterance, bias) :
lt=="SE"     ? speakerExpectations(1000, bias, condition) :
lt=="prior" ? bayesNetPrior(bias) :
lt=="tablePrior" ? tablesPrior() :
lt=="speaker" ? speaker(bn, bias, false) :
error("unknown listener type: " + lt)
posterior
'
MODEL <- paste(definitions, helpers, priors, rsa_model, analysis,  sep = '\n')
