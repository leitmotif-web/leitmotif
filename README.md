# Intro

stateful HTML tree generation

# Specs

## dependencies

* current path from the root, e.g. headline level based on the number of `section` etc. elements nested so far
* children of the current node, changing style based on a node's content

## structure manipulation
* inject intermediate nodes, like shell/core
* inject shell/core as children of the attached node

# Domains

## Tree building

### First class operation for insertion
* Explicit marking of insertion points
* Insertion policy with transformations for both parts

## Attribute propagation

* accumulated information
* tag name

### Style??
* Attributes should be unknown to trees; Trees are parameterized so that the style implementation is arbitrary.
* Attributes can be abstracted into (sets of) roles that are assigned to tree elements and later compiled to attributes
* Role compilers are lists of rules for how sets of roles interact with each other
* Insertion may be done at the role level
* Transformations manipulate roles

## DSL Nodes
* deviate from conventional set of html nodes like `h1`, `h2`, and use more generic ones like `h`

## Philosophy
In the wild, separation of HTML and CSS is not usual.
Still, we want clean separation of concerns.
We want to define components as pure semantic containers and reuse them in multiple places.

Even in the trivial case, the abstraction should be better than regular tree building because it separates structures by
data-driven concerns.

layer language on top of HTML and CSS: LM(M)L
