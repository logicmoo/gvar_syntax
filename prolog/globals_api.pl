:- module(gapi,
[
          nb_current_value/2, % +Key, ?Value
          nb_set_value/2,  % +Key, +Value
          b_set_value/2,  % +Key, +Value
          b_get_value/2,  % +Key, ?Value
          nb_link_value/2, % +Key, +Value
          tracker_reset/0,
          get_current_tracker/1,
          nb_current_value/3, % +Key, ?Value
          nb_set_value/3,  % +Key, +Value
          b_set_value/3,  % +Key, +Value
          b_get_value/3,  % +Key, ?Value
          nb_link_value/3, % +Key, +Value
          tracker_reset/1,
          get_current_tracker/2,
          show_name_values/0,
 push_tracker_frame/0
 ]).
/** <module> gapi - Global Variable Strorage

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- (( \+ nb_current('$tracker',_)) -> true; (rb_empty(Tracker),nb_setval('$tracker',[Tracker]))).

show_name_values:- 
 ignore(forall(nb_current_value(gvar(_),N,V),show_name_value(N,V))).
 % ignore(forall(nb_current_value(0,N,V),show_name_value(N,V))).

show_name_value(N,V):- (atomic(N)->true; \+ atomic(V)), format('~N~n',[]),fail.
show_name_value(N,V):-  attvar(V),!, fmt9( N :- V ).
show_name_value(N,V):-  \+ compound(V),!,format( '~q :- ~q.~n', [N , V] ).
show_name_value(N,Tree):- is_rbtree(Tree),   
   (rb_empty(Tree) -> must(fmt( N = is_rbtree_empty(Tree)));
     forall(nb_current_value(Tree,NS,V),show_name_value('::'(N,NS),V))),!.

show_name_value(N,List):- is_list(List),  (( \+ \+ ((member(M,List),  (compound(M);attvar(M))))) -> 
  forall(nth1(I,List,V),show_name_value('::'(N,I),V));
  fmt9( N :- List)) ,!.
show_name_value(N,V):-  fmt9( N :- V ).
  



% gvar_rbv
% gvar_v
% rbv

nb_current_value(N,V):- quietly(nonvar(N)->once(((context_default(Ctx), nb_current_value(Ctx,N,V))));(((context_default(Ctx), nb_current_value(Ctx,N,V))))).
nb_current_value(gvar(Type),N,V):- (nonvar(Type)->!;true), nb_current(N,V0),V0\==[],get_value_value(Type,V0,V).
nb_current_value(Tracker,N,V):- is_rbtree(Tracker),!,rb_in(N,_,Tracker),rb_lookup(N,V0,Tracker),must(get_value_value(rbv,V0,V)).
nb_current_value(Var,N,V):- nonvar(Var),Var=[E|List], !, member(Ctx,[E|List]),nb_current_value(Ctx,N,V),!.
nb_current_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),nb_current_value(Tracker,N,V).

nb_link_value(N,V):- quietly((context_default(Ctx), nb_link_value(Ctx,N,V))),!.

nb_link_value(Tracker,N,V):- is_rbtree(Tracker),!,nb_rb_link(Tracker,N,V).
nb_link_value([E|List],N,V):- nonvar(E),!, member(Ctx,[E|List]),nb_link_value(Ctx,N,V),!.
nb_link_value(gvar(v),N,V):- !, nb_linkval(N,V).
nb_link_value(gvar(_),N,V):- !, nb_current(N,Was),!,(((compound(Was),Was=rbv(_)))->nb_setarg(1,Was,V);nb_linkval(N,V)).
nb_link_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),!,nb_rb_link(Tracker,N,V).

dupe_same(X,Y):- duplicate_term(X,Y),X=Y.

nb_rb_link(Tracker,N,V):- 
  (nb_rb_get_node(Tracker,N,Node)
    -> (dupe_same(rbv(V),D),nb_rb_set_node_value(Node,D))
    ;  nb_rb_insert(Tracker,N,rbv(V))).

/*
nb_rb_link(Tracker,N,V):- 
  (nb_rb_get_node(Tracker,N,Node)
    -> (dupe_same(V,D),nb_rb_node_value(Node,RBV),nb_linkarg(1,RBV,D))
    ;  nb_rb_insert(Tracker,N,rbv(V))).
*/

nb_set_value(N,V):- quietly((context_default(Ctx), nb_set_value(Ctx,N,V))),!.
nb_set_value(gvar(v),N,V):- !, nb_setval(N,V).
nb_set_value(gvar(_),N,D):- !, nb_current(N,Was),!,(((compound(Was),Was=rbv(_)))->(duplicate_term(D,V),nb_setarg(1,Was,V));nb_setval(N,D)).
nb_set_value([E|List],N,V):- !, member(Ctx,[E|List]),nb_set_value(Ctx,N,V),!.
nb_set_value(Ctx,N,D):- get_current_tracker(Ctx,Tracker),!,mcopy_term(D,V),nb_rb_link(Tracker,N,V),!.
mcopy_term(X,X).

b_set_value(N,V):- quietly((context_default(Ctx), b_set_value(Ctx,N,V))),!.
b_set_value(gvar(v),N,V):- !, b_setval(N,V).
b_set_value(gvar(_),N,V):- !, nb_current(N,Was),!,(((compound(Was),Was=rbv(_)))->setarg(1,Was,V);b_setval(N,V)).
b_set_value([E|List],N,V):- !, member(Ctx,[E|List]),b_set_value(Ctx,N,V),!.
b_set_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),
  (rb_lookup(N,RBV,Tracker)-> setarg(1,RBV,V) ; (RBV=rbv([]),nb_rb_insert(Tracker,N,RBV),setarg(1,RBV,V))).

b_get_value(N,V):- quietly((context_default(Ctx), b_get_value(Ctx,N,V))),!.
b_get_value(gvar(Type),N,V):- !,b_getval(N,V0),get_value_value(Type,V0,V).
b_get_value([E|List],N,V):- !, member(Ctx,[E|List]),b_get_value(Ctx,N,V),!.
b_get_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),rb_lookup(N,RBV,Tracker),!,arg(1,RBV,V).


nb_get_value(N,V):- context_default(Ctx), nb_get_value(Ctx,N,V),!.
nb_get_value(Ctx,N,V):- b_get_value(Ctx,N,V).


reset_rb_tree(Tracker):- rb_empty(X),arg(1,X,L),arg(2,X,R),nb_setarg(1,Tracker,L),nb_setarg(2,Tracker,R).
tracker_reset(Ctx):-get_current_tracker(Ctx,Tracker),reset_rb_tree(Tracker).
tracker_reset:- rb_empty(Tracker),nb_setval('$tracker',[Tracker]).
push_tracker_frame :- rb_empty(Tracker),nb_current('$tracker',Trackers),b_setval('$tracker',[Tracker|Trackers]).
get_nth_tracker(Ctx,Tracker):- nb_current('$tracker',Trackers)->nth_tracker(Ctx,Trackers,Tracker);(tracker_reset,nb_current('$tracker',Trackers),nth_tracker(Ctx,Trackers,Tracker)).


context_default(0).
context_default(gvar(v)).

get_current_tracker(Tracker):- context_default(Ctx), get_current_tracker(Ctx,Tracker).

get_current_tracker(Ctx,Ctx):- nonvar(Ctx),is_rbtree(Ctx),!.
get_current_tracker(Ctx,Tracker):- compound(Ctx),!,get_named_tracker(Ctx,Tracker).
get_current_tracker(Ctx,Tracker):- get_nth_tracker(Ctx,Tracker).

nth_tracker(Ctx,Trackers,Tracker):- nth0(Ctx,Trackers,Tracker).

set_named_tracker(#(Ctx),Tracker):- (var(Tracker)->rb_empty(Tracker);true),nb_linkval(Ctx,Tracker).
set_named_tracker(?(Ctx),Tracker):- (var(Tracker)->rb_empty(Tracker);true),
  get_nth_tracker(0,TrackerTracker),
  nb_link_value(TrackerTracker,Ctx,Tracker).

get_named_tracker(#(Ctx),Tracker):- nb_current(Ctx,Tracker)->assertion(is_rbtree(Tracker));set_named_tracker(#(Ctx),Tracker).
get_named_tracker(?(Ctx),Tracker):- get_nth_tracker(0,TrackerTracker),
     (nb_get_value(TrackerTracker,Ctx,Tracker) -> assertion(is_rbtree(Tracker)) ; 
        (rb_empty(Tracker),nb_link_value(TrackerTracker,Ctx,Tracker))).


get_value_value(v,V0,V):- \+ compound(V0),!,V0=V.
get_value_value(rbv,rbv(V0),V):- !,V0=V.
get_value_value(v,V0,V):- !,V0=V.

set_value_value(v,V0,V):- V0=V.
set_value_value(rbv,V,rbv(V0)):- !,V0=V.



%% gvar_file_predicates_are_exported() is det.
%
% All Module Predicates Are Exported.

:- module_transparent(gvar_file_predicates_are_exported/0).
gvar_file_predicates_are_exported:- current_prolog_flag(xref,true),!.
gvar_file_predicates_are_exported:-
 source_location(S,_), prolog_load_context(module,LC),
 % writeln(gvar_file_predicates_are_exported(S,LC)),
 gvar_file_predicates_are_exported(S,LC).


lmconfig:never_export_named_gvar(attr_unify_hook/2).
lmconfig:never_export_named_gvar(attribute_goals/3).
lmconfig:never_export_named_gvar(project_attributes/2).
lmconfig:never_export_named_gvar(attr_portray_hook/2).


:- module_transparent(gvar_file_predicates_are_exported/2).
gvar_file_predicates_are_exported(S,LC):-
 forall(source_file(M:H,S),
 ignore((functor(H,F,A), \+ atom_concat('$',_,F), \+ lmconfig:never_export_named_gvar(F/_),
  ignore(((atom(LC),atom(M), LC\==M,M:export(M:F/A),LC:multifile(M:F/A),fail,atom_concat('$',_,F),LC:import(M:F/A)))),
  ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  ignore(((current_predicate(system:F/A)->true; system:import(M:F/A)))))))))).

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

