% Technisch (nach innen)
%
%     1. equal testet auf strukturelle Gleichheit.
%     3. is_btree überprüft, ob BTree von korrekter syntaktischer Struktur ist und ob die Semantik (Sortierung und Höhe) korrekt ist.
%         Bei der Prüfung darf der Baum nur einmal von oben nach unten durchlaufen werden und wegen der Rekursion dann einmal von unten nach oben.
%         Bei der semantischen Prüfung eines Knotens haben also alle dazu benötigten Informationen direkt verwertbar als Datenstruktur vorzuliegen.
%
% Objektmengen:  elem, btree, high
% Operationen: (semantische Signatur) / (syntaktische Struktur)
% is_btree: btree → bool                                    / is_btree(<BTree>)
% is_empty: btree → bool                         / is_empty(<BTree>)
% equal: btree × btree → bool                / equal(<BTree>,<BTree>)

-module(btree).
-compile(export_all).
% -export([init_btree/0, is_btree/1, insert_node/2, is_empty/1, equal/2]).


% init_btree: ∅ → btree
init_btree() -> {}.

% Pattern {{Parent, Height}, {LeftChild, Height}, {RightChild, Height}}
% Height of the tree: Empty tree = 0, only root = 1 and so on
% Attention: Height != Level! Height at each node is the height of the partial tree from this node on,
% so the height gets smaller towards the leafs

% is_btree: btree → bool
% is_btree(<BTree>)


% insert_node: btree × elem → btree
% elements in the partial left tree are smaller and in the partial right tree greater than parent
% elements == parent are ignored
insert_node({}, Element) -> init_leaf(Element);

insert_node({{Parent, _Height}, {}, RightChild}, Element) when Element < Parent ->
  NewLeftChild = init_leaf(Element),
  NewParentHeight = max_height(NewLeftChild, RightChild) + 1,
  {{Parent, NewParentHeight}, NewLeftChild, RightChild};

insert_node({{Parent, _Height}, LeftChild, {}}, Element) when Element > Parent ->
  NewRightChild = init_leaf(Element),
  NewParentHeight = max_height(LeftChild, NewRightChild) + 1,
  {{Parent, NewParentHeight}, LeftChild, NewRightChild};

insert_node({{Parent, Height}, LeftChild, RightChild}, Element) when Element == Parent ->
  {{Parent, Height}, LeftChild, RightChild};

insert_node({{Parent, _Height}, LeftChild, RightChild}, Element) ->
  if
    Element < Parent ->
      NewLeftChild = insert_node(LeftChild, Element),
      NewParentHeight = max_height(NewLeftChild, RightChild) + 1,
      {{Parent, NewParentHeight}, NewLeftChild, RightChild};
    Element > Parent ->
      NewRightChild = insert_node(RightChild, Element),
      NewParentHeight = max_height(LeftChild, NewRightChild) + 1,
      {{Parent, NewParentHeight}, LeftChild, NewRightChild}
  end.


% is_empty: btree → bool
% is_empty(<BTree>)


% equal: btree × btree → bool
% equal(<BTree>,<BTree>)


% Helper
% init_leaf: elem → btree
init_leaf(Element) -> {{Element, 1}, {}, {}}.


% height: btree → integer
% Get height of parent node of given (partial) tree
height({}) -> 0;
height({{_Parent, Height}, _LeftChild, _RightChild}) -> Height.


% max_height: btree × btree → integer
% Get max. height of the two given (partial) trees
max_height({}, {}) -> 0;
max_height(FirstTree, SecondTree) -> max(height(FirstTree), height(SecondTree)).
