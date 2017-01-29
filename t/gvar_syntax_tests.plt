
:- use_module(library(gvar_syntax)).

:- $gvar.foo = 1.


test(1):- writeln($foo.get()).

test(2):- trace, $bar.set(2), $foo.set($bar.get()), test1.

test(3):- writeln($gvar.foo).

test(4):- forall(gv_tests,true).

test(5):- $gvar.baz = point{x:vx,y:vy,z:vz}.

test(6):- writeln($baz.get().z).

test(7):- $baz.set(point{ x: ($foo.get()) , y:vy, z:vz}).

test(8):- writeln($baz.x).

all_tests:- forall(test(_),true).



