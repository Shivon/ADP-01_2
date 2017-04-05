-module(queue_ad).
-export([create/0, enqueue/2, dequeue/1, front/1, is_empty/1, equal/2]).


% Requirements: queue is implemented with 2 stacks (in and out stack)
% InStack gets restacked to OutStack, _only_ when trying to access empty OutStack
% example queues
% Q1 = {{99, {2, {101, {1, {2, {}}}}}}, {}}.
% Q2 = {{99, {2, {101, {1, {2, {}}}}}}, {1,{}}}.
% Q3 = {{99,{2,{101,{1,{2,{}}}}}},{1,{2,{}}}}.


% create: ∅ → queue
create() -> {stack:create(), stack:create()}.


% enqueue:  queue × elem → queue
enqueue({InStack, OutStack}, Element) -> {stack:push(InStack, Element), OutStack}.


% dequeue: queue → queue (Mutator)
dequeue({InStack, OutStack}) ->
  OutStackEmpty = stack:is_empty(OutStack),
  if
    not(OutStackEmpty) -> {InStack, stack:pop(OutStack)};
    OutStackEmpty ->
      % restack InStack to OutStack when OutStack already empty
      NewOutStack = stack:reverse(InStack),
      {{}, stack:pop(NewOutStack)}
  end.


% front: queue → elem
front({InStack, OutStack}) ->
  OutStackEmpty = stack:is_empty(OutStack),
  if
    OutStackEmpty -> NewOutStack = stack:reverse(InStack), stack:top(NewOutStack);
    true -> stack:top(OutStack)
  end.


% is_empty: queue → bool
is_empty({InStack, OutStack}) -> stack:is_empty(InStack) and stack:is_empty(OutStack).


% equal: queue × queue → bool
equal({FirstInStack, FirstOutStack}, {SecondInStack, SecondOutStack}) ->
  stack:equal(FirstInStack, SecondInStack) and stack:equal(FirstOutStack, SecondOutStack).
