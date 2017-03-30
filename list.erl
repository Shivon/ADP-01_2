% Vorgabe:
% Funktional (nach außen)
% eoCount(L) zählt die Anzahl der in der Länge geraden bzw. ungeraden Listen in der Liste L inklusive dieser Liste selbst, also alle möglichen Listen! Eine leere Liste wird als Liste mit gerader Länge angesehen. In der Liste können Elemente vorkommen, die keine Liste sind. Rückgabe ist das Tupel [<Anzahl gerade>,<Anzahl ungerade>]

% Objektmengen: pos, elem, list
% Operationen: (semantische Signatur) / (syntaktische Struktur)
% concat: list × list → list                                  / concat(<Liste>,<Liste>)
% diffListe: list × list → list                                / diffListe(<Liste>,<Liste>)
% eoCount: list → [int,int]                                  / eoCount(<Liste>)

-module(list).
-compile({no_auto_import,[length/1]}).
-compile(export_all).

% List model: {1, {2, {3, {}}}} => Pos1 = 1, Pos2 = 2 etc
% test lists:
% L1 = {1, {2, {3, {}}}}.
% L2 = {}.
% L3 = {1, {2, {3, {}}}}.
% L4 = {4, {5, {6, {}}}}.
% L5 = {9, {}}.

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
% TODO: Frage: so wie oben besser, oder auch bei letztem Fall nochmal sicherheitshalber einen Guard einbauen?
% find_({Head, Tail}, Element, AccuPosition) when Head /= Element -> find_(Tail, Element, AccuPosition + 1).


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


% diffList(<Liste>,<Liste>)
% eoCount(<Liste>)
