function [J, grad] = costFunction(theta, X, y)
%COSTFUNCTION Compute cost and gradient for logistic regression
%   J = COSTFUNCTION(theta, X, y) computes the cost of using theta as the
%   parameter for logistic regression and the gradient of the cost
%   w.r.t. to the parameters.

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta
%
% Note: grad should have the same dimensions as theta
%

h = sigmoid(sum((ones(m,1) * theta') .* X,2));
J = sum(-y .* log(h) - (1 - y) .* log(1 - h)) / m;

grad = zeros(size(theta));
for j=1:length(theta)
  grad(j) = sum((h - y) .* X(:,j)) / m;
end

% =============================================================

end

% vim:set ft=octave sts=4 sw=4 ai et:
