# Rego Model Configuration File

| parameter  | Value|
| -------------- |:-----:|
| task | **regression** or (binary) **classification** | 
| ## *Model spec* | | 
| model.type | **rules**, **linear** or **both**| 
| model.max.rules | the approximate number of rules generated (prior to the regularization phase). It is approximate because the rules are not generated directly but via the tree ensemble. Since a *J*−terminal node tree generates *2 × (J − 1)* rules, *max.rules/J* defines the number of trees. | 
| model.max.terms | maximum number of terms selected for final model| 
| ## *Tree Ensemble control* | | 
| te.tree.size | the average number of terminal nodes in trees generated during phase-1 of the ensemble generation phase (i.e., before each tree is built, a size value is drawn from a distribution that has a mean of *J*) . This controls the dominant interaction order of the model -- e.g., size-2 trees implies a "main-effects" only model, size-3 trees allow for first-order interactions, etc. | 
| te.sample.fraction | represents the size of the training data sub-sample used to build each tree. It controls "diversity" in the ensemble (correlation among the trees) and speed. Smaller values result in higher diversity and faster compute times. Default exponentially decays from ~0.5 (for small data sets) towards ~0.01, but overwrite it if you would like to have each tree fit to larger data subsets than what this default will produce.| 
| te.interaction.suppress | interaction suppression factor. "Boosts" split scores to favor reusing the same variable along a given (root to node) tree path. This places a preference on fewer variables defining splits along such paths, which will be later converted into rules. This makes it harder for "spurious" interactions to come into the model. These spurious interactions can occur in the presence of high collinearity among the input variables.| 
| te.memory.param | tree ensemble learning rate | 
| ## *Regularization method* | | 
| sparsity.method | one of **Lasso**, **Lasso+FSR** (lasso to select variable entry order, followed by a forward stepwise regression), **FSR** (forward stepwise to select variables and fit model) | 
| ## *Model selection* | | 
| score.criterion | one of **1-AUC**(optimizes 1.0 minus the Area under the ROC Curve),**AAE**(optimizes average absolute error; if (task == *classification*), optimizes average squared-error loss on predicted probabilities),**LS** (optimizes average squared-error), **Misclassification** (optimizes misclassification risk). | 
| crossvalidation.num.folds | number of test replications used for model selection| 
| crossvalidation.fold.size | fraction of input observations used it each test fold| 
| misclassification.costs | misclassification costs (when task == *classification*)| 
| ## *Preprocessing* | | 
|  data.trim.quantile | winsorising quantile; applies to both ends of the variable range. If you need to winsorise different variables by different amounts, you need to use the col.winz.fname parameter and then set this parameter to 0.| 
|  data.NA.value | predictor variable values of NA are internally set to this value| 
| ## *Iteration Control* | | 
| convergence.threshold | coefficient estimation iterations stop when the maximum standardized coefficient change from the previous iteration is less than this value.| 
| ## *Memory management* | | 
| mem.tree.store | | 
| mem.cat.store | | 
| ## *Installation info* | | 
| rf.working.dir | path to working directory where model will be saved to. If not specified, an attempt to read environment variable RF_WORKING_DIR will be made.| 
