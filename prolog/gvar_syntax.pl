:- module(gvar_syntax,[
 ('.')/2, 
 was_gvar/1,
 is_gvar/2]).

/** <module> gvar_syntax - Global Variable Syntax

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- use_module(library(dicts)).


:- '$set_source_module'('$dicts').
:- clause('.'(Dict, Func, Value),BODY),nb_setval(gvar_syntax, (':-'('.'(Dict, Func, Value),BODY))).
:- redefine_system_predicate('system':'.'(_Dict, _Func, _Value)).
:- 'system':abolish('$dicts':'.'/3).
:- if(\+ current_prolog_flag(gvar_syntax,non_extendable)).
:- dynamic('$dicts':'.'/3).
:- multifile('$dicts':'.'/3).
:- module_transparent('$dicts':'.'/3).
:- endif.
:- nb_getval(gvar_syntax,WAS),
 compile_aux_clauses([(('$dicts':'.'(Self,Func,Value):- 
   gvar_syntax:is_gvar(Self,Name),!,gvar_syntax:gvar_interp(Name,Func,Value))),WAS]).
:- if(current_prolog_flag(gvar_syntax,non_extendable)).
:- compile_predicates(['$dicts':'.'(_Dict, _Func, _Value)]).
:- endif.
:- system:import('$dicts':'.'/3).
:- system:lock_predicate('$dicts':'.'/3).
:- '$set_source_module'(gvar_syntax).

:- if(\+ current_prolog_flag(gvar_callable_syntax,false)).
:- multifile((.)/2).
:- dynamic((.)/2).
:- module_transparent((.)/2).
:- Head=..['.',Self, Func], assert((( Head :- debugging(gvar_syntax,gvar_callable_syntax(Self, Func)), '$dicts':'.'(Self, Func,_)))).
:- endif.


%% is_gvar(+Self,-Name) is det.
%
%  Tests to see if Self
%  is $(Name) or was_gvar($Name).
%
is_gvar(Self,Name):- 
    ground(Self),
    (Self='$'(Name);
     Self=was_gvar('$'(Name))),!.
/*
is_gvar(Self,Name):- 
    compound(Self),
    ((Self='$'(Name);
     (Self=was_gvar(DSelf),compound(DSelf),DSelf='$'(Name)))),!
*/


%% was_gvar(atomic:+GVar) is det.
%
%  Wrapper that is callable
%
was_gvar(_).


%% gvar_interp(+GVar, +Func, ?Value) is det.
%
%  Get/Set GVar or call the previous 
%  Dict interpretor
%

% checked above gvar_interp(Name, Var, Value):- var(Var),!,make_dot(Name, Var,Value).
gvar_interp(Name, current(),Value):- !, nb_current(Name,Value).
gvar_interp(Name, get(),Value):- !,nb_getval(Name,Value).
gvar_interp(Name, value, Value):- !, gvar_unify(Name,Value).

% the trick here is an undone linkval
gvar_interp(Name, let, Value):- !, b_setval(Name,Value),nb_linkval(Name,Value).
gvar_interp(Name, set, Value):- !, gvar_put(Name,Value).
gvar_interp(Name, set(Value),was_gvar($Name)):-!, gvar_interp(Name, set, Value).
gvar_interp(Name, let(Value),was_gvar($Name)):-!, gvar_interp(Name, let, Value).
gvar_interp(Name, clear(),was_gvar($Name)):-!, nb_delete(Name).
gvar_interp(Name, Missed,Value) :-  make_dot(Name,Missed,Value).


make_dot(Name, Missed,Value):- Value =.. ['.',$(Name),Missed].

% gvar_unify(Name,Value):- nb_current(Name,Ref),!,freeze(Value,Value=Ref).
gvar_unify(Name,Value):- nb_current(Name,Was),!,Value=Was.
gvar_unify(Name,Value):- nb_linkval(Name,Value),freeze(Value,gvar_put(Name, Value)).


% Sets a copy and then unifies the value to the copy
gvar_put(Name,Value):- 
   nb_setval(Name,Value), % after duplicate_term
   nb_current(Name,Value). %  we still want the same variables (if possible)


