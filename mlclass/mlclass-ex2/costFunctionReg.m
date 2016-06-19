function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta

h = sigmoid(sum((ones(m,1) * theta') .* X,2));
r = sum(theta(2:length(theta),:) .^ 2);
r = lambda / (2 * m) * r;
J = sum(-y .* log(h) - (1 - y) .* log(1 - h)) / m + r;

grad = zeros(size(theta));
grad(1) = sum((h - y) .* X(:,1)) / m;
for j=2:length(theta)
  grad(j) = (sum((h - y) .* X(:,j)) + lambda * theta(j)) / m;
end

% =============================================================

end
% vim:set ft=octave sts=4 sw=4 ai et:
