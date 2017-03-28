% Vorgabe:
% Funktional (nach außen)
% eoCount(L) zählt die Anzahl der in der Länge geraden bzw. ungeraden Listen in der Liste L inklusive dieser Liste selbst, also alle möglichen Listen! Eine leere Liste wird als Liste mit gerader Länge angesehen. In der Liste können Elemente vorkommen, die keine Liste sind. Rückgabe ist das Tupel [<Anzahl gerade>,<Anzahl ungerade>]

% Objektmengen: pos, elem, list
% Operationen: (semantische Signatur) / (syntaktische Struktur)
% insert:  list × pos × elem → list                     / insert(<Liste>,<Position>,<Element>)
% delete: list × pos → list                                 / delete(<Liste>,<Position>)
% find: list × elem → pos                                  / find(<Liste>,<Element>)
% retrieve: list × pos → elem                           / retrieve(<Liste>,<Position>)
% concat: list × list → list                                  / concat(<Liste>,<Liste>)
% diffListe: list × list → list                                / diffListe(<Liste>,<Liste>)
% eoCount: list → [int,int]                                  / eoCount(<Liste>)

-module(liste).
-compile(export_all).


% create: ∅ → list
create() -> {}.


% isEmpty: list → bool
% isEmpty(List) -> List == {}.
isEmpty(List) -> Length = laenge(List), Length == 0.


% laenge: list → int
laenge(List) -> laenge_(List, 0).

laenge_({}, Accu) -> Accu;
laenge_({Head, _Tail}, Accu) -> laenge_(Head, Accu + 1).


% isList: list → bool
isList({}) -> true;
isList({Head, _Tail}) -> isList(Head);
isList(_) -> false.


% equal: list × list → bool
% Equal tests for structural equality.
equal({}, {}) -> true;
equal({FirstHead, FirstTail}, {SecondHead, SecondTail}) when FirstTail == SecondTail ->
  equal(FirstHead, SecondHead);
equal(_, _) -> false.


% find: list × elem → pos
% returns nil, when element is not found
find({}, _) -> nil;
find({Head, Tail}, Element) when Tail == Element -> laenge({Head, Tail});
find({Head, _Tail}, Element) -> find(Head, Element).
% TODO: Frage: so wie oben besser, oder auch bei letztem Fall nochmal sicherheitshalber einen Guard einbauen?
% find({Head, Tail}, Element) when Tail /= Element -> find(Head, Element).


% retrieve: list × pos → elem
retrieve({}, _) -> nil;
retrieve(List, Position) ->
  Length = laenge(List),
  {Head, Tail} = List,
  if
    Length < Position -> nil;
    Length == Position -> Tail;
    Length > Position -> retrieve(Head, Position)
  end.


% concat: list × list → list
% concat(<Liste>,<Liste>)


% insert:  list × pos × elem → list
% List starts at position 1.
% Insertion is non-destructive:
%   if element is inserted at a position, at which already another element is,
%   the other element is shifted by one position.
% insert(<Liste>,<Position>,<Element>)
% insert({}, 1, Element) -> {Element, {}};
% insert({Head, Tail}, Position, Element) ->


% delete(<Liste>,<Position>)
% diffListe(<Liste>,<Liste>)
% eoCount(<Liste>)
