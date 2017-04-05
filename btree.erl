% Technisch (nach innen)
%
%     1. equalBT testet auf strukturelle Gleichheit.
%     3. isBT überprüft, ob BTree von korrekter syntaktischer Struktur ist und ob die Semantik (Sortierung und Höhe) korrekt ist.
%         Bei der Prüfung darf der Baum nur einmal von oben nach unten durchlaufen werden und wegen der Rekursion dann einmal von unten nach oben.
%         Bei der semantischen Prüfung eines Knotens haben also alle dazu benötigten Informationen direkt verwertbar als Datenstruktur vorzuliegen.
%
% Objektmengen:  elem, btree, high
% Operationen: (semantische Signatur) / (syntaktische Struktur)
% isBT: btree → bool                                    / isBT(<BTree>)
% isEmptyBT: btree → bool                         / isEmptyBT(<BTree>)
% equalBT: btree × btree → bool                / equalBT(<BTree>,<BTree>)

-module(btree).
-compile(export_all).
% -export([initBT/0, isBT/1, insertBT/2, isEmptyBT/1, equalBT/2]).


% initBT: ∅ → btree
initBT() -> {}.

% Pattern {{Parent, Height}, {LeftChild, Height}, {RightChild, Height}}
% Height of the tree: Empty tree = 0, only root = 1 and so on
% Attention: Height != Level! Height at each node is the height of the partial tree from this node on,
% so the height gets smaller towards the leafs

% isBT: btree → bool
% isBT(<BTree>)


% insertBT: btree × elem → btree
% elements in the partial left tree are smaller and in the partial right tree greater than parent
% elements == parent are ignored
insertBT({}, Element) -> initLeaf(Element);

insertBT({{Parent, _Height}, {}, RightChild}, Element) when Element < Parent ->
  NewLeftChild = initLeaf(Element),
  NewParentHeight = getMaxHeight(NewLeftChild, RightChild) + 1,
  {{Parent, NewParentHeight}, NewLeftChild, RightChild};

insertBT({{Parent, _Height}, LeftChild, {}}, Element) when Element > Parent ->
  NewRightChild = initLeaf(Element),
  NewParentHeight = getMaxHeight(LeftChild, NewRightChild) + 1,
  {{Parent, NewParentHeight}, LeftChild, NewRightChild};

insertBT({{Parent, Height}, LeftChild, RightChild}, Element) when Element == Parent ->
  {{Parent, Height}, LeftChild, RightChild};

insertBT({{Parent, _Height}, LeftChild, RightChild}, Element) ->
  if
    Element < Parent ->
      NewLeftChild = insertBT(LeftChild, Element),
      NewParentHeight = getMaxHeight(NewLeftChild, RightChild) + 1,
      {{Parent, NewParentHeight}, NewLeftChild, RightChild};
    Element > Parent ->
      NewRightChild = insertBT(RightChild, Element),
      NewParentHeight = getMaxHeight(LeftChild, NewRightChild) + 1,
      {{Parent, NewParentHeight}, LeftChild, NewRightChild}
  end.


% isEmptyBT: btree → bool
% isEmptyBT(<BTree>)


% equalBT: btree × btree → bool
% equalBT(<BTree>,<BTree>)


% Helper
% initLeaf: elem → btree
initLeaf(Element) -> {{Element, 1}, {}, {}}.


% getHeight: btree → integer
% Get height of parent node of given (partial) tree
getHeight({}) -> 0;
getHeight({{_Parent, Height}, _LeftChild, _RightChild}) -> Height.


% getMaxHeight: btree × btree → integer
% Get max. height of the two given (partial) trees
getMaxHeight({}, {}) -> 0;
getMaxHeight(FirstTree, SecondTree) -> max(getHeight(FirstTree), getHeight(SecondTree)).
