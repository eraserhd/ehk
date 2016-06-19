
X = [1, 1; 1, 2; 1, 3];
y = [2; 4; 4];

J = computeCost(X,y,[0; 0])
expected_J = 1/6 * ( 2 ^ 2 + 4 ^ 2 + 4 ^ 2 )
if J ~= expected_J
    disp("computeCost 1 FAILED");
endif

J = computeCost(X,y,[0; 1])
expected_J = 1/6 * ( (2 - 1) ^ 2 + (4 - 2) ^ 2 + (4 - 3) ^ 2 )
if J ~= expected_J
    disp("computeCost 2 FAILED");
endif

J = computeCost(X,y,[2; -1])
expected_J = 1/6 * ( (2 - (2 + 1*-1)) ^ 2 + (4 - (2 + 2*-1)) ^ 2 + (4 - (2 + 3*-1)) ^ 2 )
if J ~= expected_J
    disp("computeCost 3 FAILED");
endif

% vim:set ft=octave sts=4 sw=4 ai et:
