-module(list).
-compile({no_auto_import,[length/1]}).
-export([create/0, isEmpty/1, length/1, isList/1, equal/2, find/2, retrieve/2, delete/2, insert/3, concat/2, diffList/2, eoCount/1]).

% List model: {1, {2, {3, {}}}} => Pos1 = 1, Pos2 = 2 etc
% test lists:
% L1 = {1, {2, {3, {}}}}.
% L2 = {}.
% L3 = {1, {2, {3, {}}}}.
% L4 = {4, {5, {6, {}}}}.
% L5 = {9, {}}.
% L6 = {99, {2, {101, {1, {2, {}}}}}}.

% create: ∅ → list
create() -> {}.


% isEmpty: list → bool
% isEmpty(List) -> List == {}.
isEmpty(List) -> length(List) == 0.


% length: list → int
length(List) -> length_(List, 0).

length_({}, Accu) -> Accu;
length_({_Head, Tail}, Accu) -> length_(Tail, Accu + 1).


% isList: list → bool
isList({}) -> true;
isList({_Head, Tail}) -> isList(Tail);
isList(_) -> false.


% equal: list × list → bool
% Equal tests for structural equality.
equal({}, {}) -> true;
equal({FirstHead, FirstTail}, {SecondHead, SecondTail}) when FirstHead == SecondHead ->
  equal(FirstTail, SecondTail);
equal(_, _) -> false.


% find: list × elem → pos
% returns nil when element is not found
find(List, Element) -> find_(List, Element, 1).

find_({}, _, _) -> nil;
find_({Head, _Tail}, Element, AccuPosition) when Head == Element -> AccuPosition;
find_({_Head, Tail}, Element, AccuPosition) -> find_(Tail, Element, AccuPosition + 1).


% retrieve: list × pos → elem
% returns nil when position invalid
retrieve({}, _) -> nil;
retrieve({Head, _Tail}, 1) -> Head;
retrieve({_Head, Tail}, Position) -> retrieve(Tail, Position - 1).


% delete: list × pos → list
% returns unamended list when position is invalid
delete({}, _) -> {};
delete({_Head, Tail}, 1) -> Tail;
delete({Head, Tail}, Position) -> {Head, delete(Tail, Position - 1)}.


% insert:  list × pos × elem → list
% List starts at position 1.
% Insertion is non-destructive:
%   if element is inserted at a position, at which already another element is,
%   the other element is shifted by one position.
insert(List, 1, Element) -> {Element, List};
insert({Head, Tail}, Position, Element) -> {Head, insert(Tail, Position - 1, Element)};
insert({}, _, _) -> {}.


% concat: list × list → list
concat({}, List) -> List;
concat(List, {}) -> List;
concat(FirstList, {Head, Tail}) ->
  LengthFirstList = length(FirstList),
  AccuList = insert(FirstList, LengthFirstList + 1, Head),
  concat(AccuList, Tail).


% diffListe: list × list → list
diffList(FirstList, SecondList) -> diffList_(FirstList, SecondList, {}).

diffList_(List, {}, ResultList) -> concat(List, ResultList);
diffList_({}, List, ResultList) -> concat(List, ResultList);
diffList_(FirstList, SecondList, ResultList) ->
  {FirstHead, FirstTail} = FirstList,
  {SecondHead, SecondTail} = SecondList,
  FirstHeadInSecondList = elementInList(FirstHead, SecondList),
  SecondHeadInFirstList = elementInList(SecondHead, FirstList),

  if
    FirstHeadInSecondList and SecondHeadInFirstList ->
      NewFirstList = deleteAll(FirstTail, SecondHead),
      NewSecondList = deleteAll(SecondTail, FirstHead),
      diffList_(NewFirstList, NewSecondList, ResultList);

    FirstHeadInSecondList ->
      NewSecondList = deleteAll(SecondTail, FirstHead),
      diffList_(FirstTail, NewSecondList, {SecondHead, ResultList});

    SecondHeadInFirstList ->
      NewFirstList = deleteAll(FirstTail, SecondHead),
      diffList_(NewFirstList, SecondTail, {FirstHead, ResultList});

    true -> diffList_(FirstTail, SecondTail, {FirstHead, {SecondHead, ResultList}})
  end.


% eoCount: list → [int,int]
% eoCount(L) counts the lists with even and odd length within list L including list L itself. An empty list is considered to have
% even length. List L can include elements which are no lists. Return value is the tuple [<number or even lists>, <number of odd lists>]
% Note: the empty list, which indicates the end of the list, is not counted as element or as even list
eoCount(List) ->
  IsEven = hasEvenLength(List),
  if
    IsEven -> eoCount_(List, [1, 0]);
    not(IsEven) -> eoCount_(List, [0, 1])
  end.

eoCount_({}, Result) -> Result;
eoCount_(List, [Even, Odd]) ->
  {Head, Tail} = List,
  HeadIsList = isList(Head),
  if
    not(HeadIsList) -> eoCount_(Tail, [Even, Odd]);
    HeadIsList ->
      HeadIsEven = hasEvenLength(Head),
      if
        HeadIsEven -> eoCount_(Tail, [Even + 1, Odd]);
        not(HeadIsEven) -> eoCount_(Tail, [Even, Odd + 1])
      end
  end.
% {{1, {2, {}}}, {{1, {2, {}}}, {{101, {1, {2, {}}}}, {1, {2, {}}}}}}.
% {{1, {2, {}}}, {2, {{101, {1, {2, {}}}}, {1, {2, {}}}}}}.


% Helper
% elementInList: elem × list → bool
elementInList(_Element, {}) -> false;
elementInList(Element, {Head, _Tail}) when Element == Head -> true;
elementInList(Element, {_Head, Tail}) -> elementInList(Element, Tail).


% deleteAll: list × elem → list
% deletes all elements with the same value as given element from a given list and returns new list
deleteAll({}, _) -> {};
deleteAll({Head, Tail}, Element) when Head == Element -> deleteAll(Tail, Element);
deleteAll({Head, Tail}, Element) -> {Head, deleteAll(Tail, Element)}.


% hasEvenLength: list → bool
hasEvenLength(List) -> isEven(length(List)).


% isEven: integer → bool
isEven(Integer) -> Integer rem 2 == 0.
