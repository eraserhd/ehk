
def from_truth_table p, q, t
  idx_q = q ? 0 : 1
  idx_p = p ? 0 : 1
  idx = (idx_p << 1) | idx_q
  t[idx]
end

def implies p, q
  from_truth_table p, q, [true, false, true, true]
end

VALUES = [
  [true,true],
  [true,false],
  [false,true],
  [false,false]
]

def check(n)
  puts "#{n}: #{VALUES.map{|p, q| yield p,q}.inspect}"
end

check(1) do |smoke, fire|
  implies(smoke,fire) == (smoke or !fire)
end
check(2) do |smoke, fire|
  implies(smoke,fire) == implies(!smoke,!fire)
end
check(3) do |smoke, fire|
  implies(smoke,fire) == implies(!fire,!smoke)
end
check(4) do |big, dumb|
  big or dumb or implies(big,dumb)
end
check(5) do |big, dumb|
  (big and dumb) == !(!big or !dumb)
end
