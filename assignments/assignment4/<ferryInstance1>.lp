#const steps=12.

car(car1;car2;car3;car4).

location(loc1;loc2;loc3;loc4).

at(car1,loc1,0).
at(car2,loc2,0).
at(car3,loc3,0).
at(car4,loc4,0).

at(ferry,loc1,0).

goal(T):-
  at(car1,loc2,T),
  at(car2,loc3,T),
  at(car3,loc4,T),
  at(car4,loc1,T).

% This took 9152 bytes of memory and 0.033s to run on my laptop
% CPU i7-7700HQ 2.8Ghz
% RAM 16GB 2400Mhz DDR4
%
% There are 4 locations, each car at a different location
% and we move each car one location over
