-module(stack).
-export([create/0, push/2, pop/1, top/1, is_empty/1, equal/2, reverse/1]).


% create: ∅ → stack
% create()
create() -> list:create().


% push:  stack × elem → stack
% in this implementation list position 1 equals stack top
push(Stack, Element) -> list:insert(Stack, 1, Element).


% pop: stack → stack
pop(Stack) -> list:delete(Stack, 1).


% top: stack → elem
top(Stack) -> list:retrieve(Stack, 1).


% is_empty: stack → bool
is_empty(Stack) -> list:is_empty(Stack).


% equal: stack × stack → bool
equal(FirstStack, SecondStack) -> list:equal(FirstStack, SecondStack).


% reverse: stack → stack
reverse(Stack) -> reverse(Stack, {}).

reverse({}, ResultStack) -> ResultStack;
reverse({Top, Rest}, ResultStack) ->
  reverse(Rest, {Top, ResultStack}).
