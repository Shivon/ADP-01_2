-module(queueAD).
-export([createQ/0, enqueue/2, dequeue/1, front/1, isEmptyQ/1, equalQ/2]).
-import(stack, [createS/0, push/2, pop/1, top/1, isEmptyS/1, equalS/2, reverseS/1]).


% Requirements: queue is implemented with 2 stacks (in and out stack)
% InStack gets restacked to OutStack, _only_ when trying to access empty OutStack
% example queues
% Q1 = {{99, {2, {101, {1, {2, {}}}}}}, {}}.
% Q2 = {{99, {2, {101, {1, {2, {}}}}}}, {1,{}}}.
% Q3 = {{99,{2,{101,{1,{2,{}}}}}},{1,{2,{}}}}.


% createQ: ∅ → queue
createQ() -> {createS(), createS()}.


% enqueue:  queue × elem → queue
enqueue({InStack, OutStack}, Element) -> {push(InStack, Element), OutStack}.


% dequeue: queue → queue (Mutator)
dequeue({InStack, OutStack}) ->
  OutStackEmpty = isEmptyS(OutStack),
  if
    not(OutStackEmpty) -> {InStack, pop(OutStack)};
    OutStackEmpty ->
      % restack InStack to OutStack when OutStack already empty
      NewOutStack = reverseS(InStack),
      {{}, pop(NewOutStack)}
  end.


% front: queue → elem
front({InStack, OutStack}) ->
  OutStackEmpty = isEmptyS(OutStack),
  if
    OutStackEmpty -> NewOutStack = reverseS(InStack), top(NewOutStack);
    true -> top(OutStack)
  end.


% isEmptyQ: queue → bool
isEmptyQ({InStack, OutStack}) -> isEmptyS(InStack) and isEmptyS(OutStack).


% equalQ: queue × queue → bool
equalQ({FirstInStack, FirstOutStack}, {SecondInStack, SecondOutStack}) ->
  equalS(FirstInStack, SecondInStack) and equalS(FirstOutStack, SecondOutStack).
