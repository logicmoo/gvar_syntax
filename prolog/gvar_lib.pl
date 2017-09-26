:- module(gvs,
[
 is_gvar/3,       % +Module, +Self, -Name
 gvar_must/4,     % +Module, +GVar, +Func, ?Value
 gvar_call/4,     % +Module, +GVar, +Func, ?Value
 '$was_gvar'/2       % +Module, +GVar
 ]).
/** <module> gvs - Global Variable Syntax

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- meta_predicate(gvar_call(+,?,?,?)).
:- set_module(class(library)).
:- multifile(dot_cache:using_dot_type/2).
:- dynamic(dot_cache:using_dot_type/2).

:- reexport(library(debug),[debug/3]).
:- reexport(library(dicts)).


:- multifile(dot_intercept/3).
:- dynamic(dot_intercept/3).
:- module_transparent(dot_intercept/3).
:- meta_predicate(dot_intercept(:,?,?)).
:- '$dicts':import(dot_intercept/3).
:- 'system':import(dot_intercept/3).

/*
 
 rtrace($varA.value()=X).
 rtrace($varA.set(9)).
 set_prolog_flag(access_level,system),set_prolog_flag(mpred_te,false).



  rtrace($varA.set(_{})).
*/

install_dot_intercept:- current_prolog_flag(dot_intercept,installed),!.
install_dot_intercept:-
   set_prolog_flag(dot_intercept,installed),
   'system':unlock_predicate('$dicts':'.'/3),
   % clause('.'(Dict, Func, Value),BODY),
   redefine_system_predicate('system':'.'(_Dict, _Func, _Value)),
   'system':abolish('$dicts':'.'/3),
   % dynamic('$dicts':'.'/3),
   % multifile('$dicts':'.'/3),
   module_transparent('$dicts':'.'/3),
   '$set_source_module'('$dicts'),
   '$dicts':compile_aux_clauses([
      (('.'(SSelf,Func,Value) :- strip_module(SSelf,M,Self), dot_intercept(M:Self,Func,Value))) %,
      % ('.'(Dict, Func, Value) ':-' BODY)
      ]),
   '$set_source_module'(gvs),
   'system':lock_predicate('$dicts':'.'/3),
   'system':import('$dicts':'.'/3).

:- install_dot_intercept.

% :- listing('$dicts':('.')/3).

:- if(\+ current_prolog_flag(gvar_callable_syntax,false)).
%:- multifile(system:(.)/2).
:- module_transparent(system:(.)/2).
:- dynamic(system:(.)/2).
:- module_transparent(system:(.)/2).
:- Head=..['.',SSelf, Func], 
  system:assert((( 
  Head :- strip_module(SSelf,M,Self), 
          nop(debugging(gvs,gvar_callable_syntax(Self, Func))),
          dot_intercept(M:Self, Func,_)))).
:- export(system:('.')/2).
:- 'system':import(('.')/2).
:- endif.

:- multifile(is_dot_hook/4).
:- dynamic(is_dot_hook/4).
:- module_transparent(is_dot_hook/4).

:- multifile(dot_overload_hook/4).
:- dynamic(dot_overload_hook/4).
:- module_transparent(dot_overload_hook/4).

dot_intercept(MSelf,Func,Value):-
   strip_module(MSelf,M,Self),
   (tracing->true;(debugging(dictoo(_))->trace;true)),
   dot_intercept4(M,Self,Func,Value).

:- 'system':import(gvs:dot_intercept/3).
  
:- module_transparent(dot_intercept4/4).
dot_intercept4(M,Self,Func,Value):- use_dot(_,M),notrace((nonvar(Value),current_prolog_flag(gvar_nondict_ok,true))),!,Value =.. ['.',Self,Func].
dot_intercept4(M,Self,Func,Value):- use_dot(_,M),show_failure(is_dot_hook(M,Self,Func,Value)),!,dot_overload_hook(M,Self,Func,Value).
dot_intercept4(M,Self,Func,Value):- is_gvar(M,Self,Name),!, gvar_must(M,Name,Func,Value).
dot_intercept4(M,Self,Func,Value):- current_prolog_flag(gvar_nondict_ok,true), \+ M:is_dict(Self),!,Value =.. ['.',Self,Func].
dot_intercept4(M,Self,Func,Value):- M:dot_dict(Self, Func, Value).


use_dot(Type):- 
   prolog_load_context(module, M),
   use_dot(Type,M).

use_dot(Type,M):- 
   \+ current_prolog_flag(dictoo_syntax,false),
   (current_prolog_flag(break_level, 0);source_location(_,_)),
   dot_cache:using_dot_type(Type,M),!.


:- '$dicts':export('$dicts':eval_dict_function/4).
:- '$dicts':export('$dicts':'$get_dict_ex'/3).
:- '$dicts':export('$dicts':'$type_error'/3).
:- system:import('$dicts':eval_dict_function/4).
:- system:import('$dicts':'$get_dict_ex'/3).
:- system:import('$dicts':'$type_error'/3).


%!  dot_dict(+R, +L, -Result)
%
%   Evaluate dot expressions. Note that  '$get_dict_ex' fails if the
%   first argument is not a dict or the second is not a valid key or
%   unbound.
:- module_transparent(dot_dict/3).
dot_dict(Data, Func, Value) :-
    (   '$get_dict_ex'(Func, Data, V0)
    *-> Value = V0
    ;   is_dict(Data, Tag)
    ->  eval_dict_function(Func, Tag, Data, Value)
    ;   is_list(Data)
    ->  (   (atomic(Func) ; var(Func))
        ->  dict_create(Dict, _, Data),
            '$get_dict_ex'(Func, Dict, Value)
        ;   '$type_error'(atom, Func)
        )
    ;   '$type_error'(dict, Data)
    ).

%! is_gvar(+Module, +Self, -Name) is det.
%
%  Tests to see if Self
%  is $(Name) or '$was_gvar'(Module,$Name).
%
is_gvar(_,Self,Name):- 
    nonvar(Self),
    (Self='$'(Name); (Self='$was_gvar'(_,DName),compound(DName),DName='$'(Name))),
    !,atom(Name).

%! '$was_gvar'(+Module, +GVar) is det.
%
%  Wrapper that is callable
%
'$was_gvar'(_,_).



%! gvar_must(+Module, +GVar, +Func, ?Value) is det.
%
%  Get/Set GVar or call the previous 
%  Dict interpretor
%
:- module_transparent(gvar_must/4).
gvar_must(M,Name, Memb, Value) :-  gvar_call(M,Name, Memb, Value)*-> true ; make_dot(M,Name,Memb,Value).

gvar_call(M, Name, Memb, Value) :- atom(Name),nonvar(Memb),
    (  \+ current_prolog_flag(gvar_syntax_scope,prepend_module)
      -> gvar_interp(M,Name,Name, Memb, Value) ;
      (atomic_list_concat([M,':',Name],NewName),
      gvar_interp(M,Name,NewName, Memb, Value))),!.


:- module_transparent(gvar_interp/5).

is_dict_function(_,get(_Key)).
is_dict_function(_,put(_Key)).
is_dict_function(_,put(_Key,_Value)).
is_dict_function(Tag,Func):- atom(Tag),current_module(Tag),
   compound_name_arity(Func,F,A), 
   A2 is A+2,
   current_predicate(Tag:F/A2),!,
   assertion(\+ is_gvar_function(Tag,Func)).

is_gvar_function(_,current()).
is_gvar_function(_,get()).
is_gvar_function(_,unify()).
is_gvar_function(_,Func):- is_gvar_set_function(Func).

is_gvar_set_function(let()).
is_gvar_set_function(set()).
is_gvar_set_function(let(_)).
is_gvar_set_function(set(_)).
is_gvar_set_function(clear()).
is_gvar_set_function(delete()).

gvar_interp(M,SN, Name, Func, Value):- 
  (M:nb_current(Name,Data) 
   -> (is_dict(Data, Tag) 
      -> gvar_interp5(M,dict(Data,Tag), SN, Name, Func, Value)
      ; gvar_interp5(M,value(Data), SN, Name, Func, Value))
  ; gvar_interp5(M,missing, SN, Name, Func, Value)).


gvar_interp5(M,dict(Data,_Tag), _SN, _Name, Func, Value):- var(Func),!,M:get_dict(Func, Data, Value).
gvar_interp5(M,dict(Data,Tag), _SN, _Name, Func, Value):- 
  (is_dict_function(Tag,Func)
      -> (!,eval_dict_function(Func, Tag, Data, Value))
      ; (atom(Func)
         -> M:get_dict(Func, Data, Value))).

% checked above?
% gvar_interp5(M,_, _, Name, Var, Value):- var(Var),!,make_dot(M,Name, Var,Value).
gvar_interp5(M,_, _, _,Func, _Value):- (\+ compound(Func) ; \+ is_gvar_function(M,Func),!,fail).
gvar_interp5(M,_, _, Name, unify(), Value):-!, gvar_unify(M,Name,Value).
gvar_interp5(M,_, _, Name, current(),Value):-!, M:nb_current(Name,Value).
gvar_interp5(M,_, _, Name, get(),Value):-!, M:nb_getval(Name,Value).
%gvar_interp5(M,_, _, Name, value(),Value):-!, M:nb_getval(Name,Value).
%gvar_interp5(M,_, _, Name, value,Value):-!, M:nb_getval(Name,Value).

% the trick here is an undone linkval
gvar_interp5(M,_, _, Name, let(), Value):- !, M:b_setval(Name,Value),M:nb_linkval(Name,Value).
gvar_interp5(M,_, _, Name, set(), Value):- !, M:nb_linkval(Name,Value), !, freeze(Value,gvar_put(M,Name,Value)).
gvar_interp5(M,_, SN,Name, set(Value),'$was_gvar'(M,$SN)):- gvar_put(M,Name,Value),!.
gvar_interp5(M,_, SN,Name, let(Value),'$was_gvar'(M,$SN)):- M:b_setval(Name,Value),M:nb_linkval(Name,Value),!.
gvar_interp5(M,_, SN,Name, clear(),'$was_gvar'(M,$SN)):-!, M:nb_setval(Name,_).
gvar_interp5(M,_, SN,Name, delete(),'$was_gvar'(M,$SN)):-!, M:nb_delete(Name).


make_dot(M,Name, Missed,M:Value):- Value =.. ['.',$(Name),Missed].

% gvar_unify(M,Name,Value):- nb_current(Name,Ref),!,freeze(Value,Value=Ref).
gvar_unify(M,Name,Value):- M:nb_current(Name,Was),!,Value=Was.
gvar_unify(M,Name,Value):- var(Value),!, M:nb_linkval(Name,Value),freeze(Value,gvar_put(M,Name, Value)).
gvar_unify(M,Name,Value):- gvar_put(M,Name,Value).

% Sets a copy and then unifies the value to the copy
gvar_put(M,Name,Value):- 
   M:nb_setval(Name,Value), % after dupe_term
   M:nb_current(Name,Value). %  we still want the same variables (if possible)





/*

expand_dict_function((QFHead := V0 :- Body), (QHead :- Body, Eval)) :-
    fqhead(QFHead, FHead, Head, QHead),
    compound(FHead),
    FHead =.. [.,R,M],
    callable(M),
    !,
    '$expand':replace_functions(V0, Eval, V, _Ctx),
    compound_name_arguments(M, Name, Args0),
    '$append'(Args0, [R,V], Args),
    compound_name_arguments(Head, Name, Args).
expand_dict_function((QFHead := V0), (QHead :- Eval)) :-
    fqhead(QFHead, FHead, Head, QHead),
    compound(FHead),
    FHead =.. [.,R,M],
    callable(M),
    !,
    '$expand':replace_functions(V0, Eval, V, _Ctx),
    compound_name_arguments(M, Name, Args0),
    '$append'(Args0, [R,V], Args),
    compound_name_arguments(Head, Name, Args).

fqhead(M:FHead, FHead, Head, M:Head) :- !.
fqhead(FHead,   FHead, Head,   Head).


system:term_expansion(FDecl, Clause) :-
    expand_dict_function(FDecl, Clause).
system:term_expansion(M:FDecl, QClause) :-
    expand_dict_function(FDecl, Clause),
    !,
    QClause = M:Clause.

*/

%% gvar_file_predicates_are_exported() is det.
%
% All Module Predicates Are Exported.

:- module_transparent(gvar_file_predicates_are_exported/0).
gvar_file_predicates_are_exported:- current_prolog_flag(xref,true),!.
gvar_file_predicates_are_exported:-
 source_location(S,_), prolog_load_context(module,LC),
 % writeln(gvar_file_predicates_are_exported(S,LC)),
 gvar_file_predicates_are_exported(S,LC).

:- module_transparent(gvar_file_predicates_are_exported/2).
gvar_file_predicates_are_exported(S,LC):-
 forall(source_file(M:H,S),
 ignore((functor(H,F,A), \+ atom_concat('$',_,F),
  ((ignore(((atom(LC),atom(M), LC\==M,M:export(M:F/A),
   % LC:multifile(M:F/A),
   fail,atom_concat('$',_,F),LC:import(M:F/A)))))),
  ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  (current_predicate('system':F/A)->true; 'system':import(M:F/A)))))))).

%% gvar_file_predicates_are_transparent() is det.
%
% All Module Predicates Are Transparent.
:- module_transparent(gvar_file_predicates_are_transparent/0).
gvar_file_predicates_are_transparent:-
 source_location(S,_), prolog_load_context(module,LC),
 gvar_file_predicates_are_transparent(S,LC).

:- module_transparent(gvar_file_predicates_are_transparent/2).
gvar_file_predicates_are_transparent(S,LC):- 
 forall(source_file(M:H,S),
 (functor(H,F,A),  
  ignore(((\+ predicate_property(M:H,transparent), ignore( LC = M), 
  module_transparent(M:F/A), 
  \+ atom_concat('__aux',_,F),
   nop(debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))).


:- 
   gvar_file_predicates_are_exported,
   gvar_file_predicates_are_transparent.

