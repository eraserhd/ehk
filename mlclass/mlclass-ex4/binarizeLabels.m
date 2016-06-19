function [ binarized ] = binarizeLabels(y)

  num_labels = max(y);
  binarized = zeros(size(y));
  for i=1:size(y,1)
    binarized(i,y(i)) = 1;
  end

endfunction
