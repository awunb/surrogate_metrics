## Simulate the surrogate metrics performance in an example

set.seed(20)

## True response function
y_func = function(x)  2/3 * exp(x[1]) - x[3] * sin(x[2]) + x[2] 


## Prepare training data
training_X = expand.grid(seq(0, 1, by=0.05), seq(0, 1, by=0.05), seq(0, 1, by=0.05))
training_obs = apply(training_X, 1, y_func)
data = as.data.frame(cbind(training_obs, training_X))
names(data) = c("y", "x1", "x2", "x3")

## Suppose the treatment group has a lift of (0, 0.1439, 0.15)
## We observe that the new average keeps the same
lift = cbind(0, 0.14349, 0.15)

training_X_new = expand.grid(seq(0, 1, by=0.05) + lift[1], 
                             seq(0, 1, by=0.05) + lift[2],
                             seq(0, 1, by=0.05) + lift[3])
training_obs_new = apply(training_X_new, 1, y_func)

## Verify that the new average is the same as before
print(mean(training_obs_new), mean(training_obs))

## fit a linear model with all three variables
model = lm(y ~ polym(x1, x2, x3, degree=1, raw=TRUE), data = data)

testing_data = as.data.frame(cbind(runif(control_sample_size, 0, 1), 
                                   runif(control_sample_size, 0, 1),
                                   runif(control_sample_size, 0, 1)))
names(testing_data) = c("x1", "x2", "x3")
surrogate_values = predict(model, newdata = testing_data)
longTerm_values = apply(testing_data, 1, y_func) 

## Record the precition error
predicted_sigma2 = var(longTerm_values - surrogate_values)


## Start simulation
nIter = 10000
t_stat_surrogate = rep(0, nIter)
t_stat_surrogate_corrected = rep(0, nIter)
t_stat_longTerm = rep(0, nIter)
for (iter in seq(1, nIter, by = 1)) {
  
  treatment_sample_size = 150
  control_sample_size = 150
  lift = cbind(0, 0.14349, 0.15)
  
  X_control = as.data.frame(cbind(runif(control_sample_size, 0, 1), 
                                  runif(control_sample_size, 0, 1),
                                  runif(control_sample_size, 0, 1)))
  X_treatment = as.data.frame(cbind(runif(treatment_sample_size, 0, 1) + lift[1], 
                                    runif(treatment_sample_size, 0, 1) + lift[2],
                                    runif(treatment_sample_size, 0, 1) + lift[3]))
  
  names(X_control) = c("x1", "x2", "x3")
  names(X_treatment) = c("x1", "x2", "x3")
  
  longTerm_control = apply(X_control, 1, y_func) 
  longTerm_treatment = apply(X_treatment, 1, y_func) 
  
  surrogate_control = predict(model, newdata = X_control)
  surrogate_treatment = predict(model, newdata = X_treatment)
  
  diff_surrogate = mean(surrogate_treatment) - mean(surrogate_control)
  diff_longTerm = mean(longTerm_treatment) - mean(longTerm_control)
  
  var_surrogate = var(surrogate_treatment) / length(surrogate_treatment) + var(surrogate_control) / length(surrogate_control)
  var_longTerm = var(longTerm_treatment) / length(longTerm_treatment) + var(longTerm_control) / length(longTerm_control)
  
  t_stat_surrogate[iter] = diff_surrogate / sqrt(var_surrogate)
  t_stat_longTerm[iter] = diff_longTerm / sqrt(var_longTerm)
}

t_stats = as.data.frame(cbind(t_stat_surrogate, t_stat_longTerm, t_stat_surrogate_corrected))
t_stats$surrogate_stat_sig = abs(t_stats$t_stat_surrogate) > 1.96 

t_stats$longTerm_stat_sig = abs(t_stats$t_stat_longTerm) > 1.96

## Record the # of times the surrogate and the long term metrics are significant
nrow(subset(t_stats, t_stats$surrogate_stat_sig == TRUE))
nrow(subset(t_stats, t_stats$longTerm_stat_sig == TRUE))
