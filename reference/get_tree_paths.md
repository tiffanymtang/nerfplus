# Extract all root-to-leaf paths in a tree.

Extract all root-to-leaf paths in a tree.

## Usage

``` r
get_tree_paths(tree_info)
```

## Arguments

- tree_info:

  Output of
  [`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md)
  for a single tree.

## Value

A list of decision paths in a single tree.
