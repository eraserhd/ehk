
load('ex3data1.mat');
X = [ones(size(X,1),1), X];

t1_result = abs(lrCostFunction(ones(401,1) / 5,X,y,1) + 51.130) < 1e-3
t2_result = abs(lrCostFunction(ones(401,1) / 9,X,y,1) + 28.397) < 1e-3
t3_result = abs(lrCostFunction(ones(401,1) / 5,X,y,0) + 51.132) < 1e-3
t4_result = abs(lrCostFunction(ones(401,1) / 9,X,y,0) + 28.397) < 1e-3
