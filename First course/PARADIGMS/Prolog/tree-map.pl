split(null, _, null, null) :- !.
split(node(Key, Value, Prior, L, R), Splitter, LeftResult, RightResult) :-
        Key =< Splitter,
        split(R, Splitter, NRL, RightResult),
        LeftResult = node(Key, Value, Prior, L, NRL), !.
split(node(Key, Value, Prior, L, R), Splitter, LeftResult, RightResult) :-
        Key > Splitter,
        split(L, Splitter, LeftResult, NLR),
        RightResult = node(Key, Value, Prior, NLR, R), !.


merge(Node, null, Node) :- !.
merge(null, Node, Node) :- !.
merge(node(KeyLeft, ValueLeft, PriorLeft, LeftLeftTree, LeftRightTree), node(KeyRight, ValueRight, PriorRight, RightLeftTree, RightRightTree), Result) :-
        PriorLeft < PriorRight,
        merge(LeftRightTree, node(KeyRight, ValueRight, PriorRight, RightLeftTree, RightRightTree), MergedR),
        Result = node(KeyLeft, ValueLeft, PriorLeft, LeftLeftTree, MergedR), !.
merge(node(KeyLeft, ValueLeft, PriorLeft, LeftLeftTree, LeftRightTree), node(KeyRight, ValueRight, PriorRight, RightLeftTree, RightRightTree), Result) :-
        PriorLeft >= PriorRight,
        merge(node(KeyLeft, ValueLeft, PriorLeft, LeftLeftTree, LeftRightTree), RightLeftTree, MergedL),
        Result = node(KeyRight, ValueRight, PriorRight, MergedL, RightRightTree), !.


map_put(Node, Key, Value, Ans) :-
        split(Node, Key, LeftTree, RigthTree),
        split(LeftTree, Key - 1, NewLeftTree, _),
        rand_int(100000, Prior),
        merge(NewLeftTree, node(Key, Value, Prior, null, null), Result),
        merge(Result, RigthTree, Ans), !.


map_get(node(X, Value, _, _, _), X, Value) :- !.
map_get(node(Key, _, _, Left, _), Find, Value) :-
        Find < Key,
        map_get(Left, Find, Value).
map_get(node(Key, _, _, _, Right), Find, Value) :-
        Find > Key,
        map_get(Right, Find, Value).


map_remove(Node, Key, Result) :-
        split(Node, Key, LeftTree, RightTree),
        split(LeftTree, Key - 1, NLT, _),
        merge(NLT, RightTree, Result).


map_build([], null).
map_build([(Key, Value) | T], Tree) :-
        map_build(T, NewTree),
        map_put(NewTree, Key, Value, Tree).



map_replace(Tree, Key, Value, Result):-
        map_get(Tree, Key, _),
        map_remove(Tree, Key, T),
        map_put(T, Key, Value, Result), !.
map_replace(Tree, _, _, Res):- Res = Tree.