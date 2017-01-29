:- module(gvar_syntax,[]).

/** <module> gvar_syntax - Global Variable Syntax

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

get_gvar(Name,Value):- nb_current(Name,Ref),!,Ref=Value, nonvar(Value)->true;freeze(Value,set_gvar(Name, Value)).
get_gvar(Name,Value):- nb_linkval(Name,Value),freeze(Value,set_gvar(Name, Value)).
set_gvar(Name,Value):- nb_linkval(Name,Value).

gvar_type(gvar_default_type,nb_linkval).


% Call Previous Method
gv_call(Self,Memb,Value):- (var(Self);is_dict(Self);Self\='$'(_)),!,'$dict_dot3'(Self, Memb, Value).

gv_call($gvar,Name,Value):-!,get_gvar(Name,Value).
gv_call($Name, set(Value),udt:get_gvar(Name,Value)):-nonvar(Name),!.
gv_call($Name, get(),Value):-nonvar(Name),!,nb_getval(Name,Value).
gv_call($Name, let(Value),udt:get_gvar(Name,Value)):-nonvar(Name),!.



:- if(clause('$dicts':'.'(_,_,_),_)).

:- clause('$dicts':'.'(Dict, Func, Value),BODY),
   asserta(('$dict_dot3'(Dict, Func, Value):- '$dict':BODY)).

:- else.

'$dict_dot3'(Dict, Func, Value) :-
    (   '$get_dict_ex'(Func, Dict, V0)
    *-> Value = V0
    ;   is_dict(Dict, Tag)
    ->  '$dicts':eval_dict_function(Func, Tag, Dict, Value)
    ;   is_list(Dict)
    ->  (   (atomic(Func) ; var(Func))
        ->  dict_create(Dict, _, Dict),
            '$get_dict_ex'(Func, Dict, Value)
        ;   '$type_error'(atom, Func)
        )
    ;   '$type_error'(dict, Dict)
    ).

:- endif.


:- redefine_system_predicate('system':'.'(_Dict, _Func, _Value)).

:- 'system':abolish('$dicts':'.'/3).

'system':'.'(Dict, Func, Value) :- gv_call(Dict,Func,Value).

