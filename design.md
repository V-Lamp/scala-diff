
## Open research questions
- GOAL: Given two trees, produce a structure that explains the edits from the first to the second.

- Needs to combine ordered and unordered tree edits. Sequences are ordered. Maps, Sets are unordered

- Object maps e.g. Map[String,Any] are weird: You don't expect/allow any edit on keys! Only modifications in the values
    Having a schema definition should massively reduce the complexity!


## STEPS:

- Think of an output data structure that allows to be rendered into a diff-like representation

- Test how it all looks on JSON & XML, while making manual diffs on simple structures

- Make it work for a basic tree data structure, then see how to generalise/type class it

- Generalise it for objects (Shapeless)



## Resources:

- Tree edit distance issues: In problem definition, if a node is deleted, its children are appended to the (grand)parent node
- Survey paper: 
    A Survey on Tree Edit Distance and Related Problems Philip Bille
    http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.100.2577&rep=rep1&type=pdf

- R&D page:
    http://tree-edit-distance.dbresearch.uni-salzburg.at/ 