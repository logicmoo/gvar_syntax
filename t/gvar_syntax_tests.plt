
:- use_module(library(gvar_syntax)).

% :- set_prolog_flag(gvar_syntax_scope,module).

:- $foo.set() = 1.

test(0):- \+ $foo.current() = 2.

test(1):- writeln($foo.current()).

test(2):- writeln($foo.get()).

test(3):- writeln($foo.clear()).

test(4):- $bar.set(2), $foo.set($bar.get()), test(1).

test(5):- writeln($foo.set(33).set(34).current()).

test(6):- $baz.set(point{x:vx,y:vy,z:vz}).

test(7):- writeln($baz.get().z).

test(8):- $baz.set(point{ x: ($foo.get()) , y:vy, z:vz}).

test(9):- writeln($baz.current().x).

test(10):- $baz.set($baz.current().put(y,yYYYY)).

test(11):- $baz.current().y == yYYYY.

all_tests:- forall(test(_),true).

:- listing(test(_)).
