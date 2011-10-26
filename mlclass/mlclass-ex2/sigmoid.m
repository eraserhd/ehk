function g = sigmoid(z)
%SIGMOID Compute sigmoid functoon
%   J = SIGMOID(z) computes the sigmoid of z.

% You need to return the following variables correctly 

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the sigmoid of each value of z (z can be a matrix,
%               vector or scalar).

g = zeros(size(z));
for i=1:size(z,1)
    for j=1:size(z,2)
        g(i,j) = 1 / (1 + e ^ - z(i,j));
    end
end


% =============================================================

end
% vim:set ft=octave sts=4 sw=4 ai et:
