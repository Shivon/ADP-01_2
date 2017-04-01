% Objektmengen: elem, stack
% Operationen: (semantische Signatur) / (syntaktische Struktur)
% reverseS: stack → stack                         / reverseS(<Stack>)

-module(stack).
-compile(export_all).
-import(list, [create/0, insert/3, delete/2, retrieve/2, isEmpty/1, equal/2]).

% createS: ∅ → stack
% createS()
createS() -> create().

% push:  stack × elem → stack
% in this implementation list position 1 equals stack top
push(Stack, Element) -> insert(Stack, 1, Element).

% pop: stack → stack
pop(Stack) -> delete(Stack, 1).

% top: stack → elem
top(Stack) -> retrieve(Stack, 1).

% isEmptyS: stack → bool
isEmptyS(Stack) -> isEmpty(Stack).

% equalS: stack × stack → bool
equalS(FirstStack, SecondStack) -> equal(FirstStack, SecondStack).

% reverseS: stack → stack
% reverseS(<Stack>)
