function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

%C = 1;
%sigma = 0.3;
%best_e = 1e200;
%
%trial_values = [0.01 0.03 0.1 0.3 1 3 10 30];
%for trial_C=trial_values
%  for trial_sigma=trial_values
%    model = svmTrain(X, y, trial_C, @(x1, x2) gaussianKernel(x1, x2, trial_sigma));
%    predictions = svmPredict(model, Xval);
%    e = mean(double(predictions ~= yval));
%    if e < best_e
%      best_e = e;
%      C = trial_C;
%      sigma = trial_sigma;
%    end
%  end
%end

C =  1;
sigma =  0.10000;
%best_e =  0.030000

% =========================================================================

end
