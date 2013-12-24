
## The Plan As Of Now ##

* Use Ukkonen's algorithm to generate a generalized suffix tree

  + Use an underlying directed graph implementation based on maps:
  
    - Data item associated with nodes (**Note:** data which is often diagrammed as being associated with edges rather than nodes can be represented as data associated with nodes, and should be.)
    
    - Data item associated with edges -- this will be used solely to give the type of an edge, not to store suffix-tree data.
      
    - Different types of edges (regular edge and suffix link, but underlying implementation should support arbitrary edge-types)
    
    - Useful debug tool would definitely be a function to export to GraphViz DOT format
    
* Extract longest matching substrings from suffix tree, eliminating redundancies at each node as the strings "bubble up" from the leaf nodes.

   + Abstraction layer for the suffix tree graph which allows accessing like a tree via ``clojure.zip`` (suffix links are ignored for this, which means it is always a proper tree)
  
   + Tree traversal/value-manipulation abstraction which operates as follows:
  
      - Takes a zip-tree and several functions:
      
         1. "Get value" accessor function to take a zip/node and extract the value stored at that node
         2. "Set value" 'mutator' function to take a zip/node and a new value, and set the value at that ``zip/node`` to the new value. Note that this is different than a raw ``zip/edit`` replace with value, because (in most cases) the function supplied here preserves children, only modifying a "value" attribute stored at the node. For annotating a tree while preserving its data, mutator can set metadata or something.
         3. "Going down" function is applied to nodes *before* children are traversed;
         4. "Going up" function is applied to nodes *after* children are traversed
            - Should each take as arguments the following:
               1. value function applied to the parent (i.e. ``(first (zip/path loc))``) of the current loc (nil if root node)
               2. value function applied to ``zip/node`` of the current loc
               3. value function mapped over ``zip/children`` of the current loc (nil if leaf node)
            - Return a single value which will be the new value for that node in the tree
            - **A word about values passed:** The reason the supplied values to these functions are not the raw zip/nodes given by the ``zip`` library is because if you are traversing arbitrary subtrees every time you modify a single node, you are *doing it wrong* by this abstraction. For obvious asymptotic-complexity reasons, it is far better to traverse the whole tree some finite number of times rather than traverse the tree once, each time also traversing every subtree.

               + I cannot think of any tree transformation which would be made possible by manipulating the values of all subtrees of parent and children during the walk which could not be accomplished by clever use of transforming functions and multiple passes over the data. Perhaps it's even the case that no such cases exist; I'm not sure how I could prove that, though.
            
      - Uses these to walk the tree in a non-stack-consuming manner via ``loop/recur`` and the ``zip`` library.
      
      - Can be used to implement pre-order or post-order traversal, or some mix of the two:
         * Pre-order implies you transform on the way down, and don't alter on the way up; post-order implies the converse.
         * "Don't alter" for either function looks like: ``(defn node-identity (fn [_ n _] n))``.
         * Examples:
           - Sum of every node beneath it and itself: ``node-identity`` and ``(fn [_ n cs] (reduce + n (cs)))`` -- obviously, sum of only its children requires only swapping the two functions.
           - Label nodes by level: ``(fn [p n _] (if p (+ n p) 0))`` and ``node-identity``
           - Label by maximum number of children in any subtree: ``node-identity`` and ``(fn [_ _ cs] (reduce max (count cs) cs))``
           
   + The above is generalized a lot mostly because it's cool and might be useful for other things. The actual tree traversal required for the suffix-tree traversal is seemingly only of the "bubbling up" pattern, and does not require the "trickling down" pattern also supported by this abstraction.

* Repeat for the reverse of the strings, and return only the results which are in both sets of results. This eliminates redundancies not caught in the tree traversal. (Need to figure out how much redundancy-elimination to do in tree traversal.)
