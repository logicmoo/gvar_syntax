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

:- system:multifile((.)/2).
:- Head=..['.',Self, Func], system:assert(( Head :- notrace(is_gvar(Self,Name)),!, gvar_call(Name,Func,_))).

:- clause('$dicts':'.'(Dict, Func, Value),BODY),
   asserta(('$dicts':'$dict_dot3'(Dict, Func, Value):- '$dicts':BODY)).

:- redefine_system_predicate('system':'.'(_Dict, _Func, _Value)).
:- 'system':abolish('$dicts':'.'/3).
'system':'.'(Dict, Func, Value) :- gvar_call(Dict,Func,Value).

%% is_gvar(+Self,-Name) is det.
%
%  Tests to see if Self
%  is $(Name) or was_gvar(Name).
%
is_gvar(Self,Name):- 
    compound(Self),
    (Self='$'(Name);
     Self=was_gvar(Name)).

%% gvar_call(+GVar, +Func, ?Value) is det.
%
%  Get/Set GVar or call the previous 
%  Dict interpretor
%
gvar_call(Self,Func,Value):- 
     notrace(is_gvar(Self,Name)) 
    -> 
     gvar_interp(Name,Func,Value) 
    ; % Call Previous Method
     '$dicts':'$dict_dot3'(Self, Func, Value).

%% was_gvar(atomic:+GVar) is det.
%
%  Wrapper that is callable
%
was_gvar(_).


gvar_interp(Name, current(),Value):-nb_current(Name,Value).
gvar_interp(Name, get(),Value):- nb_getval(Name,Value).
gvar_interp(Name, value, Value):- gvar_unify(Name,Value).

% the trick here is an undone linkval
gvar_interp(Name, let, Value):- b_setval(Name,Value),nb_linkval(Name,Value).
gvar_interp(Name, set, Value):- gvar_put(Name,Value).
gvar_interp(Name, set(Value),was_gvar(Name)):- gvar_interp(Name, set, Value).
gvar_interp(Name, let(Value),was_gvar(Name)):- gvar_interp(Name, let, Value).
gvar_interp(Name, clear(),was_gvar(Name)):- nb_delete(Name).

% gvar_unify(Name,Value):- nb_current(Name,Ref),!,freeze(Value,Value=Ref).
gvar_unify(Name,Value):- nb_current(Name,Was),!,Value=Was.
gvar_unify(Name,Value):- nb_linkval(Name,Value),freeze(Value,gvar_put(Name, Value)).

% 
gvar_put(Name,Value):- 
   nb_setval(Name,Value), % after duplicate_term
   (compound(Value)->
        nb_current(Name,Value);true). %  we still want the same variables (if possible)


