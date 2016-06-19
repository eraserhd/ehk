#!/usr/bin/env jconsole

values =: 4 2 $ 0 0 0 1 1 0 1 1

not =: 3 : 0
  x { 1 0
)

or =: 4 : 0
  y { x { 2 2 $ 0 1 1 1
)

echo 1 = 1 or 1
1 = 1 or 0
1 = 0 or 1
0 = 0 or 0

implies =: 4 : 0
  y { x { 2 2 $ 1 1 0 1
)

1 = 1 implies 1
0 = 1 implies 0
1 = 0 implies 1
1 = 0 implies 0

q1 =: 4 : 0
  (x implies y) eq (x or not y)
)

NB. vim:set sts=2 sw=2 ai et:
