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
:- meta_predicate(dot_intercept(?,?,?)).
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
      (('.'(Self,Func,Value) :- dot_intercept(Self,Func,Value))) %,
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
          % nop(debugging(gvs,gvar_callable_syntax(Self, Func))),
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

:- 'system':import(gvs:dot_intercept/3).
:- module_transparent(dot_intercept/3).
dot_intercept( Self,Func,Value):- is_dict(Self),!,dot_dict(Self, Func, Value).
dot_intercept(M:Self,Func,Value):- !, dot_intercept4(M,M:Self,Func,Value).
dot_intercept(MSelf,Func,Value):- strip_module(MSelf,M,Self),dot_intercept4(M,Self,Func,Value).

:- module_transparent(dot_intercept4/4).
% dot_intercept4(M,Self,Func,Value):- notrace((use_dot(_,M),nonvar(Value), \+ current_prolog_flag(gvar_nondict_ok,false))),!,Value =.. ['.',Self,Func].

dot_intercept4(M,Self,Func,Value):- 
   ((once((show_failure(is_dot_hook(M,Self,Func,Value)),show_failure(use_dot(_,M)))) -> dot_overload_hook(M,Self,Func,Value)) *-> true ;
   ((notrace(is_gvar(M,Self,Name)) -> gvar_call(M,Name,Func,Value) ) *-> true ; 
     dot_intercept5(M,Self,Func,Value))).

:- module_transparent(dot_intercept5/4).
dot_intercept5(_,Self,Func,Value):- \+ current_prolog_flag(gvar_nondict_ok,false),!,Value =.. ['.',Self,Func]. 
dot_intercept5(M,Self,Func,Value):- M:dot_dict(Self, Func, Value).


use_dot(Type):- 
   prolog_load_context(module, M),
   use_dot(Type,M).

use_dot(Type,M):- 
   \+ current_prolog_flag(gvs_syntax,false),
   (current_prolog_flag(break_level, 0);source_location(_,_);true),
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
is_gvar(_,Self,Name):- % fail,
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
is_gvar_function(_,value()).
is_gvar_function(_,unify()).
is_gvar_function(_,Func):- is_gvar_set_function(Func).

is_gvar_set_function(let()).
is_gvar_set_function(set()).
is_gvar_set_function(let(_)).
is_gvar_set_function(set(_)).
is_gvar_set_function(clear()).
is_gvar_set_function(delete()).

gvar_interp(M,SN, Name, Func, Value):- 
  (tracing->true;(debugging(gvs(trace))->trace;true)),
  ((M:nb_current(Name,Data) 
   -> (is_dict(Data, Tag) 
      -> gvar_interp5(M,dict(Data,Tag), SN, Name, Func, Value)
      ; gvar_interp5(M,value(Data), SN, Name, Func, Value))
  ; gvar_interp5(M,missing, SN, Name, Func, Value))).


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

gvar_interp5(M,_, _, Name, value(),Value):-!, M:((nb_current(Name,Value); on_bind(Value,nb_linkval(Name,Value)))),!.


% TODO needs to survive later nb_* calls 
gvar_interp5(M,_, _, Name, let(), Value):- !, M:b_setval(Name,Value). 
gvar_interp5(M,_, SN,Name, let(Value),'$was_gvar'(M,$SN)):- M:b_setval(Name,Value).


gvar_interp5(M,_, _, Name, set(), Value):- !, M:nb_linkval(Name,Value), !, on_bind(Value,gvar_put(M,Name,Value)).
gvar_interp5(M,_, SN,Name, set(Value),'$was_gvar'(M,$SN)):- gvar_put(M,Name,Value),!.


gvar_interp5(M,_, SN,Name, clear(),'$was_gvar'(M,$SN)):-!, M:nb_setval(Name,_).
gvar_interp5(M,_, SN,Name, delete(),'$was_gvar'(M,$SN)):-!, M:nb_delete(Name).


make_dot(M,Name, Missed,M:Value):- Value =.. ['.',$(Name),Missed].

% gvar_unify(M,Name,Value):- nb_current(Name,Ref),!,on_bind(Value,Value=Ref).
gvar_unify(M,Name,Value):- M:nb_current(Name,Was),!,Value=Was.
gvar_unify(M,Name,Value):- var(Value),!, M:nb_linkval(Name,Value),on_bind(Value,gvar_put(M,Name, Value)).
gvar_unify(M,Name,Value):- gvar_put(M,Name,Value).

% Sets a copy and then unifies the value to the copy
gvar_put(M,Name,Value):- 
   M:nb_setval(Name,Value), % after dupe_term
   M:nb_current(Name,Value). %  we still want the same variables (if possible)



on_bind(V,G):- freeze(V,G).

% :- set_prolog_flag(gvar_syntax,loaded).
hook_function_expand:- call(( abolish('$expand':function/2), asserta(('$expand':function(DOT2, _) :- compound(DOT2),
   DOT2=..['.',_,_],
  \+ current_prolog_flag(gvar_syntax,_), \+ functor([_|_], ., _))))).
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
  ((ignore(((atom(LC),atom(M), nop((LC\==M)),
   M:export(M:F/A),
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





gvs_expanded_op( = ).
gvs_expanded_op( := ).
% gvs_expanded_op( _ ).

:- module_transparent(expand_gvs_head/6).

expand_gvs_head(Head,_OP,_M,_VarO,_Memb,_Value):- \+ compound(Head),!,fail.
  
% $mod:var.memb = value.                            
expand_gvs_head(Head,OP,M,VarO,Memb,Value):-
  Head =.. [OP , :(DM,VarMemb),Value],
  compound(DM),
  DM =..[F,M],
  gvs_expanded_op(OP),
  VarMemb=..['.',Var,Memb],!,
  VarO = [F,Var].

% mod: $var.memb = value.
expand_gvs_head(Head,OP,M,Var,Memb,Value):-
  Head =.. [OP , :(M,VarMemb),Value],
  compound(VarMemb),
  gvs_expanded_op(OP),
  VarMemb=..['.',Var,Memb],!.
  
% $var.memb = value.
expand_gvs_head(Head,OP,M,Var,Memb,Value):-
  Head =.. [OP , MVarMemb,Value],
  compound(MVarMemb),
  gvs_expanded_op(OP),
  strip_module(MVarMemb,M,VarMemb),
  VarMemb=..['.',Var,Memb],!.

% $var.memb mod:= value.
expand_gvs_head(M:Head,OP,M,Var,Memb,Value):- 
  Head =.. [OP , VarMemb,Value],
  compound(VarMemb),
  gvs_expanded_op(OP),
  VarMemb=..['.',Var,Memb],!.



show_gvar(Name):-
 (nb_current(Name,Value)->true;Value='$missing'),
 format('~w =~t~12|~p~n', [Name, Value]).


:- multifile(dot_cache:dictoo_decl/8).
:- dynamic(dot_cache:dictoo_decl/8).
:- discontiguous(dot_cache:dictoo_decl/8).

show_gvs:- 
   listing(dot_cache:dictoo_decl/8),
   listing(dot_cache:using_dot_type/2),   
   maplist(show_gvar,['$goal_term','$term','$term_user','$variable_names','$query_term']),
   forall(prolog_debug:debugging(gvs(Name), Value, _),
    format('~w =~t~12|~p~n', [gvs(Name), Value])),
   ignore(print_toplevel_variables),
   !.




gvs_ge(Goal, P, _):- 
  var(P), \+ source_location(_,_),  
  % notrace(use_dot(_Type)), 
  show_call(gvs(syntax),nb_setval('$goal_term',Goal)),
  fail.

gvs_ge(Goal, _P, gvar_op_call(OP,M,Var,Memb,Value) ):- 
   prolog_load_context(module, M),
   show_success(gvs(goal_expansion), 
     M:(expand_gvs_head(Goal,OP,M,Var,Memb,Value),use_dot(_Type,M))),!.

gvs_ge(DOT2, _P,'dot_intercept'(A,B,_)):- DOT2 =.. ['.',A,B],!.

% gvs_ge('.'(A,B,C), _P,'dot_intercept'(A,B,C)):-!.


gvs_expand_query(Goal, Goal, Bindings, Bindings):- nb_setval('$query_term',Goal-Bindings).

gvar_op_call(=,M,Var,Memb,Value):- dot_intercept4(M,Var,Memb,Value).
gvar_op_call(:=,M,Var,Memb,Value):- dot_intercept4(M,Var,Memb,Value).



expand_functions(MBody, ExpandedBody):-
  strip_module(MBody,M,Body),
  expand_functions(M, Body, ExpandedBody).

:- 
   gvar_file_predicates_are_exported,
   gvar_file_predicates_are_transparent.

expand_functions(_,Var,Var):- \+ compound(Var).
expand_functions(M, :- Fun, :- ExpFun):- !, expand_functions(M, Fun,  ExpFun).
expand_functions(M, Head:- Fun, Head:- ExpFun):- !, expand_functions(M, Fun,  ExpFun).

expand_functions(M, Body, ExpandedBody) :-
    '$expand':replace_functions(Body, Eval, Goal, M),
    (Eval = true -> ExpandedBody = Body ;
    (expand_functions(M, Eval,  ExpandedBody0),
     ExpandedBody = (ExpandedBody0,Goal))).


:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

user:expand_query(Goal, _, Bindings, _ ):- notrace(use_dot(_Type)), nb_setval('$query_term',Goal-Bindings),fail.

user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):- % notrace(use_dot(_Type)),
    % Have vars to expand and varnames are empty
    notrace((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])),
    b_setval('$variable_names', Bindings),  % this prevents the loop
    % debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    (toplevel_variables_expand_query(Goal, Expanded0, Bindings, ExpandedBindings0) -> true; 
      (Goal = Expanded0, Bindings = ExpandedBindings0)),
    (user:expand_query(Expanded0, Expanded, ExpandedBindings0, ExpandedBindings) -> true ; 
     (Expanded0 = Expanded, ExpandedBindings0 = ExpandedBindings)).

make_top_var(Name,Var,Value):- prolog_load_context(module, M),
   Value = Var,
   use_dot(_Core,M),!,
   % '$was_gvs'(M,Var),
   oo(Var),
   add_var_to_env(Name,Var),
   toplevel_variables:assert_binding(Name,Value).

expand_vars(_, Var, Var) -->
    { var(Var) },
    !.
expand_vars(_, Atomic, Atomic) -->
    { atomic(Atomic) },
    !.
expand_vars(Bindings, $(Var), Value) -->
    { toplevel_variables:name_var(Var, Bindings, Name),
      (   toplevel_variables:toplevel_var(Name, Value)
      ->  !
      ;   (show_call(make_top_var(Name,Var,Value))-> true ; throw(error(existence_error(answer_variable, Name), _)))
      )
    },
    [ Name = Value ].
expand_vars(Bindings, Term, Expanded) -->
    { compound_name_arity(Term, Name, Arity),
      !,
      compound_name_arity(Expanded, Name, Arity),
      End is Arity + 1
    },
    expand_args(1, End, Bindings, Term, Expanded).


expand_args(End, End, _, _, _) --> !.
expand_args(Arg0, End, Bindings, T0, T) -->
    { arg(Arg0, T0, V0),
      arg(Arg0, T, V1),
      Arg1 is Arg0 + 1
    },
    expand_vars(Bindings, V0, V1),
    expand_args(Arg1, End, Bindings, T0, T).


toplevel_variables_expand_query(Query, Expanded, Bindings, ExpandedBindings) :- !,
    phrase(expand_vars(Bindings, Query, Expanded), NewBindings),
    term_variables(Expanded, Free),
    toplevel_variables:delete_bound_vars(Bindings, Free, ExpandedBindings0),
    '$append'(ExpandedBindings0, NewBindings, ExpandedBindings),
    (   toplevel_variables:verbose,
        Query \=@= Expanded
    ->  toplevel_variables:print_query(Expanded, ExpandedBindings)
    ;   true
    ).

toplevel_variables_expand_query(Goal, Expanded, Bindings, ExpandedBindings):- 
   \+ current_prolog_flag(toplevel_mode, recursive),
   use_dot(core),!,
  catch(toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings), Warn,
   ((dmsg(Warn), Goal = Expanded, Bindings = ExpandedBindings))).

toplevel_variables_expand_query(Goal, Expanded, Bindings, ExpandedBindings):-
  toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings).
                                 
:- system:dynamic(goal_expansion/4).
:- system:multifile(goal_expansion/4).

system:goal_expansion(Goal, P, NewGoal, PO):- gvs_ge(Goal, P, NewGoal),P=PO,!.

:- user:dynamic(term_expansion/4).
:- user:multifile(term_expansion/4).

user:term_expansion(FDecl,P, _, _) :-
   nonvar(P),compound(FDecl),nb_current('$term',Was),
   FDecl==Was,FDecl=(_:-Goal),
   show_call(gvs(syntax),nb_setval('$body_term',Goal)),
   fail.

