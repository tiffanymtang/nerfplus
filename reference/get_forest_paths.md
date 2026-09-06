# Extract all root-to-leaf paths in a forest.

Extract all root-to-leaf paths in a forest.

## Usage

``` r
get_forest_paths(tree_infos)
```

## Arguments

- tree_infos:

  List of size ntrees with each entry being the output of
  [`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md).

## Value

A list of size ntrees with each entry being a list of decision paths in
each tree.
