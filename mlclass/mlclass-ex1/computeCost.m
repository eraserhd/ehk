function J = computeCost(X, y, theta)
%COMPUTECOST Compute cost for linear regression
%   J = COMPUTECOST(X, y, theta) computes the cost of using theta as the
%   parameter for linear regression to fit the data points in X and y

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta
%               You should set J to the cost.

h = theta(1) + theta(2) * X(:,2);
J = sum((h .- y) .^ 2) / (2 * length(y));

% =========================================================================

end

% vim:set ft=octave sts=4 sw=4 ai et:
