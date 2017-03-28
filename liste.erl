% Eine ADT Liste ist zu implementieren:
% Vorgabe:
% Funktional (nach außen)
% Die Liste beginnt bei Position 1.
% Die Liste arbeitet nicht destruktiv, d.h. wird ein Element an einer vorhandenen Position eingefügt, wird das dort stehende Element um eine Position verschoben.
% equal testet auf strukturelle Gleichheit
% eoCount(L) zählt die Anzahl der in der Länge geraden bzw. ungeraden Listen in der Liste L inklusive dieser Liste selbst, also alle möglichen Listen! Eine leere Liste wird als Liste mit gerader Länge angesehen. In der Liste können Elemente vorkommen, die keine Liste sind. Rückgabe ist das Tupel [<Anzahl gerade>,<Anzahl ungerade>]
% Technisch (nach innen)
% Die Liste ist intern mittels dem Erlang Tupel { } zu realisieren.
% Die zugehörige Datei heißt liste.erl
% Objektmengen: pos, elem, list
% Operationen: (semantische Signatur) / (syntaktische Struktur)
% create: ∅ → list                                             / create()
% isEmpty: list → bool                                       / isEmpty(<Liste>)
% isList: list → bool                                           / isList(<Liste>)
% equal: list × list → bool                                  / equal(<Liste>,<Liste>)
% laenge: list → int                                            / laenge(<Liste>)
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
laenge_({_Head, Tail}, Accu) -> laenge_(Tail, Accu + 1).


% isList: list → bool
isList({}) -> true;
isList({_Head, Tail}) -> isList(Tail);
isList(_) -> false.



% equal(<Liste>,<Liste>)
% insert(<Liste>,<Position>,<Element>)
% delete(<Liste>,<Position>)
% find(<Liste>,<Element>)
% retrieve(<Liste>,<Position>)
% concat(<Liste>,<Liste>)
% diffListe(<Liste>,<Liste>)
% eoCount(<Liste>)
