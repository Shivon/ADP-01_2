% Eine ADT Queue ist zu implementieren:
% Vorgabe:
% Funktional (nach außen)
%
%     1. "First in First out" Speicher
%
% Technisch (nach innen)
%
%     1. Die ADT Queue ist mittels ADT Stack, wie in der Vorlesung vorgestellt, zu realisieren.
%         Es sind z.B. zwei explizite Stacks zu verwenden und das „umstapeln“ ist nur bei
%         Zugriff auf einen leeren „out-Stack“ durchzuführen.
%     2. equalQ testet auf strukturelle Gleichheit
%
% Objektmengen: elem, queue
% Operationen: (semantische Signatur) / (syntaktische Struktur)
% enqueue:  queue × elem → queue                / enqueue(<Queue>,<Element>)
% dequeue: queue → queue (Mutator)             / dequeue(<Queue>)
% front: queue → elem                                       / front(<Queue>)
% isEmptyQ: queue → bool                               / isEmptyQ(<Queue>)
% equalQ: queue × queue → bool                    / equalQ(<Queue>,<Queue>)

-module(queueAD).
-compile(export_all).
% -export([createQ/0, enqueue/2, dequeue/1, front/1, isEmptyQ/1, equalQ/2]).
-import(stack, [createS/0, push/2, pop/1, top/1, isEmptyS/1, equalS/2, reverseS/1]).


% createQ: ∅ → queue
createQ() -> {createS(), createS()}.


% enqueue:  queue × elem → queue
% enqueue(<Queue>,<Element>)
% enqueue({InStack, OutStack}, Element) ->
%   OutStackEmpty = isEmptyS(OutStack),
%   if
%     OutStackEmpty -> {}
%   end.


% dequeue: queue → queue (Mutator)
% dequeue(<Queue>)


% front: queue → elem
front({InStack, OutStack}) ->
  OutStackEmpty = isEmptyS(OutStack),
  if
    OutStackEmpty -> NewOutStack = reverseS(InStack), top(NewOutStack);
    true -> top(OutStack)
  end.
% {{99, {2, {101, {1, {2, {}}}}}}, {}}.
% {{99, {2, {101, {1, {2, {}}}}}}, {1,{}}}.


% isEmptyQ: queue → bool
isEmptyQ({InStack, OutStack}) -> isEmptyS(InStack) and isEmptyS(OutStack).


% equalQ: queue × queue → bool
equalQ({FirstInStack, FirstOutStack}, {SecondInStack, SecondOutStack}) ->
  equalS(FirstInStack, SecondInStack) and equalS(FirstOutStack, SecondOutStack).
