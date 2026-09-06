### Bayesian Additive Semi-Structured Regression Trees ###
### Semi-Structured decision tree class                ###

library(R6)
library(igraph)

GetEdgeIdCompat <- function(graph, vertices) {
  get_edge_ids(graph, vp = as.integer(vertices))
}

### Decision tree node class ----

TreeNode <- R6Class('TreeNode', list(
  ids = integer(),  # indices of observations in this node
  knot_ids = integer(),  # indices of knots in this node
  is_terminal = TRUE,  # whether this is a terminal node / leaf
  split_rule = NULL,  # splitting rule of this node; NULL for terminal nodes
  parent = NULL,  # parent node
  children = list(), # children nodes
  depth = NULL, # depth of this node in the decision tree
  mu = NULL,  # mean estimator in each terminal node / leaf,
  x_bound = NULL,  # a p*2 matrix recording the lower and upper bound info of x
  # where X_cutpoints[i, x_bound[i, 1]] is the lower bound for x_i
  # where X_cutpoints[i, x_bound[i, 2]] is the upper bound for x_i

  # for prediction
  ids_new = integer(),  # indices of out-of-sample observations

  initialize = function(ids, knot_ids, depth, is_terminal = TRUE, parent = NULL, x_bound = NULL) {
    stopifnot(is.integer(ids))
    stopifnot(is.null(parent) | class(parent)[1] == 'TreeNode')

    self$ids = ids
    self$knot_ids = knot_ids
    self$depth = depth
    self$is_terminal = is_terminal
    self$parent = parent
    self$x_bound = x_bound
  },

  # Function to split a leaf
  Split = function(split_rule, ids_1, ids_2, knot_ids_1, knot_ids_2) {
    stopifnot(length(self$children) == 0)

    x_bound_1 = self$x_bound  # bounds for first child
    x_bound_2 = self$x_bound  # bounds for second child
    x_bound_1[split_rule$xid, 2] = split_rule$cutpoint_id
    x_bound_2[split_rule$xid, 1] = split_rule$cutpoint_id

    self$is_terminal = FALSE
    self$split_rule = split_rule
    self$children = c(
      self$children,
      TreeNode$new(ids_1, knot_ids_1, self$depth + 1, parent = self, x_bound = x_bound_1),
      TreeNode$new(ids_2, knot_ids_2, self$depth + 1, parent = self, x_bound = x_bound_2)
    )
  },

  # Function to merge two leaves
  Merge =  function() {
    stopifnot(length(self$children) == 2)
    stopifnot(self$children[[1]]$is_terminal & self$children[[2]]$is_terminal)

    self$is_terminal = TRUE
    self$split_rule = NULL
    self$children[[2]] = NULL
    self$children[[1]] = NULL
  },

  # function to check if the node is splittable
  IsSplittable = function(type) {
    if(type == 'by_x') {
      # if split by x
      return( length(self$ids) > 1 & any(self$x_bound[, 1] < self$x_bound[, 2] - 1) )
    }
    # if split by spatial
    return(length(self$knot_ids) > 1)
  },

  # function to check if the node is mergeable
  IsMergeable = function() {
    if(length(self$children) == 0)
      return(FALSE)
    return(self$children[[1]]$is_terminal & self$children[[2]]$is_terminal)
  },

  # function to get splittable covariate x
  GetSplittableX = function() {
    return(which(self$x_bound[, 1] < self$x_bound[, 2] - 1))
  },

  # function to predict using covariates
  PredictByX = function(X_new) {
    if(length(self$ids_new) > 0 & !self$is_terminal) {
      X_new = X_new[self$ids_new, , drop = F]
      xid = self$split_rule$xid
      cutoff = self$split_rule$cutoff

      # first child: x <= cutoff
      self$children[[1]]$ids_new = self$ids_new[X_new[, xid] <= cutoff]
      # second child: x > cutoff
      self$children[[2]]$ids_new = self$ids_new[X_new[, xid] > cutoff]
    }
  },

  # function to predict using spatial locations
  PredictByLocation = function(nn_rank, nn) {
    memberships = rep(0L, ncol(nn_rank))

    if(length(self$ids_new) > 0 & !self$is_terminal) {
      nn_rank = nn_rank[self$ids_new, self$ids, drop = F]
      nn = min(nn, length(self$ids))
      # get nearest neighbors, stored in a length(ids_new) * nn matrix
      # NOTE: this is the index in self$ids
      nn_idx = t( apply(nn_rank, 1, WhichNMin, n = nn) )
      n_new = length(self$ids_new)
      # sample one neighbor
      if(nn > 1) {
        nn_idx_selected = nn_idx[cbind(1:n_new, sample.int(nn, n_new, replace = T))]
      } else {
        nn_idx_selected = nn_idx
      }
      # convert index in self$ids to actual observation id
      nn_selected = self$ids[nn_idx_selected]

      # determine child membership using nn_selected
      memberships[ self$children[[1]]$ids ] = 1
      memberships[ self$children[[2]]$ids ] = 2
      memberships_new = memberships[nn_selected]  # length = length(ids_new)

      # assign ids_new to children
      idx = (memberships_new == 1)
      self$children[[1]]$ids_new = self$ids_new[idx]
      self$children[[2]]$ids_new = self$ids_new[!idx]
    }
  },

  # function to predict using spatial locations and knots
  # with random projection
  PredictByLocation2 = function(nn_idx) {
    memberships = self$split_rule$memberships_knot

    if(length(self$ids_new) > 0 & !self$is_terminal) {
      nn_idx = nn_idx[self$ids_new, , drop = F]
      nn = ncol(nn_idx)
      n_new = length(self$ids_new)
      # sample one neighbor, nn_idx_selected is the projection for new locations
      if(nn > 1) {
        nn_idx_selected = nn_idx[cbind(1:n_new, sample.int(nn, n_new, replace = T))]
      } else {
        nn_idx_selected = nn_idx[, 1]
      }

      # determine child membership using nn_selected
      memberships_new = memberships[nn_idx_selected]  # length = length(ids_new)

      # assign ids_new to children
      idx = (memberships_new == 1)
      self$children[[1]]$ids_new = self$ids_new[idx]
      self$children[[2]]$ids_new = self$ids_new[!idx]
    }
  }
))


### Partition class ----

Partition <- R6Class('Partition', list(
  decision_root = NULL,  # root of decision tree
  spanning_tree = NULL,  # spatial spanning tree
  st_e_adj_list = NULL,  # edge adjacency list of spanning tree
  n_terminals = NULL,    # number of terminal nodes
  n_mergeables = NULL,   # number of mergeable nodes
  projection = NULL,     # mapping from observed locations to knots
  cnt_st_proposals = NULL,

  initialize = function(spanning_tree, p, numcut, projection, n_knots) {
    ids = as.integer(1:length(projection))
    knot_ids = 1:n_knots
    x_bound = cbind(rep(0, p), numcut + 1)
    self$decision_root = TreeNode$new(ids, knot_ids, 0, x_bound = x_bound)
    self$spanning_tree = spanning_tree
    self$st_e_adj_list = as_adj_edge_list(self$spanning_tree)
    self$n_terminals = 1
    self$n_mergeables = 0
  },

  # function to get all terminal nodes / leaves
  GetTerminalNodes = function() {
    terminals = list()
    # DFS
    stack = collections::stack()
    stack$push(self$decision_root)
    while(stack$size() > 0) {
      node = stack$pop()
      if(node$is_terminal) {
        terminals = c(terminals, node)
      } else {
        stack$push(node$children[[2]])
        stack$push(node$children[[1]])
      }
    }
    return(terminals)
  },

  # function to get all splittable nodes / leaves
  # type == 'by_x' or 'by_location' or 'all'
  GetSplittableNodes = function(type) {
    terminal_nodes = self$GetTerminalNodes()
    if(type == 'by_x' | type == 'by_location') {
      splittable_nodes = list()
      for(i in 1:length(terminal_nodes))
        if( terminal_nodes[[i]]$IsSplittable(type) )
          splittable_nodes = c(splittable_nodes, terminal_nodes[[i]])
      return(splittable_nodes)

    } else {
      x_splittable_nodes = list()
      loc_splittable_nodes = list()
      for(i in 1:length(terminal_nodes)) {
        if( terminal_nodes[[i]]$IsSplittable('by_x') )
          x_splittable_nodes = c(x_splittable_nodes, terminal_nodes[[i]])
        if( terminal_nodes[[i]]$IsSplittable('by_location') )
          loc_splittable_nodes = c(loc_splittable_nodes, terminal_nodes[[i]])
      }
      return(list('x' = x_splittable_nodes, 'loc' = loc_splittable_nodes))
    }
  },

  # function to get candidate nodes for merging
  GetMergeableNodes = function() {
    candidates = list()
    # DFS
    stack = collections::stack()
    stack = stack$push(self$decision_root)
    while(stack$size() > 0) {
      node = stack$pop()
      if(node$is_terminal) next
      if(node$children[[1]]$is_terminal & node$children[[2]]$is_terminal) {
        candidates = c(candidates, node)
      } else {
        stack$push(node$children[[2]])
        stack$push(node$children[[1]])
      }
    }
    return(candidates)
  },

  ProposeSplitByX = function(candidate_nodes, X, X_cutpoints, projection) {
    # find candidate node to split on
    node_split = candidate_nodes[[ sample.int(length(candidate_nodes), 1) ]]

    # find covariate to split on
    candidate_xid = node_split$GetSplittableX()
    xid = candidate_xid[ sample.int(length(candidate_xid), 1) ]

    # get cutoff
    candidate_cutoff = (node_split$x_bound[xid, 1] + 1):(node_split$x_bound[xid, 2] - 1)
    cutpoint_id = candidate_cutoff[ sample.int(length(candidate_cutoff), 1) ]
    cutoff = X_cutpoints[xid, cutpoint_id]

    # propose split
    X = X[node_split$ids, , drop = F]
    split_rule = SplitRuleX$new(xid, cutoff, cutpoint_id)
    # first child: x <= cutoff
    is_in_1 = X[, xid] <= cutoff
    ids_1 = node_split$ids[is_in_1]
    # second child: x > cutoff
    ids_2 = node_split$ids[!is_in_1]

    # find knots in children
    knot_ids_1 = unique(projection[ids_1])
    knot_ids_2 = unique(projection[ids_2])

    split_proposal = SplitProposal$new(node_split, split_rule, ids_1, ids_2, knot_ids_1, knot_ids_2)

    # check if children are splittable
    x_bound_1 = node_split$x_bound  # bounds for first child
    x_bound_2 = node_split$x_bound  # bounds for second child
    x_bound_1[xid, 2] = cutpoint_id
    x_bound_2[xid, 1] = cutpoint_id
    child_1_splittable = (any(x_bound_1[, 1] < x_bound_1[, 2] - 1) | length(knot_ids_1) > 1)
    child_2_splittable = (any(x_bound_2[, 1] < x_bound_2[, 2] - 1) | length(knot_ids_2) > 1)

    return(list('proposal' = split_proposal, 'n_splittables' = length(candidate_nodes),
                'child_1_splittable' = child_1_splittable, 'child_2_splittable' = child_2_splittable))
  },

  # function to get removable edges
  GetRemovableEdges = function(knot_ids) {
    n = length(self$st_e_adj_list)
    # whether the i-th knot is in current node
    in_node = rep(F, n)
    in_node[knot_ids] = T
    # edges to be visited, format: (from_vertex, to_vertex, edge_index)
    to_visit = collections::stack()
    # path to current vertex
    path = collections::stack()
    # candidate edges
    ecand = c()

    # begin with an arbitrary vertex in knot_ids
    v = knot_ids[1]
    # add neighbors to to_visit
    for (i in 1:length(self$st_e_adj_list[[v]])) {
      edge = self$st_e_adj_list[[v]][i]
      endpoints = ends(self$spanning_tree, edge, names = F)
      if (endpoints[1] == v) {
        to_visit$push( c(endpoints[1], endpoints[2], as.numeric(edge)) )
      } else {
        to_visit$push( c(endpoints[2], endpoints[1], as.numeric(edge)) )
      }
    }

    while (to_visit$size() > 0) {
      triple = to_visit$pop()
      from = triple[1]; to = triple[2]; edge_idx = triple[3]

      # add this edge to path
      path$push(triple)

      if (in_node[to]) {
        # this edge is adjacent to a knot in knot_ids
        # the path reaching 'to' should be removable
        while (path$size() > 0)
          ecand = c(ecand, path$pop()[3])
      }

      # look at neighbors of 'to'
      if (length(self$st_e_adj_list[[to]]) == 1) {
        # reach a leaf, backtrack
        if (to_visit$size() == 0)
          break
        while (path$size() > 0) {
          if (path$peek()[2] == to_visit$peek()[1])
            break
          invisible(path$pop())
        }
        next
      }

      # add neighbors to to_visit
      for (i in 1:length(self$st_e_adj_list[[to]])) {
        edge = self$st_e_adj_list[[to]][i]
        endpoints = ends(self$spanning_tree, edge, names = F)
        if (endpoints[1] == to & endpoints[2] != from) {
          to_visit$push( c(endpoints[1], endpoints[2], as.numeric(edge)) )
        } else if (endpoints[2] == to & endpoints[1] != from) {
          to_visit$push( c(endpoints[2], endpoints[1], as.numeric(edge)) )
        }
      }
    }

    return(E(self$spanning_tree)[ecand])
  },


  # function to propose split by spatial locations
  ProposeSplitByLocation = function(candidate_nodes, projection) {
    # find candidate node to split on
    node_split = candidate_nodes[[ sample.int(length(candidate_nodes), 1) ]]

    # split on graph on knots
    # by randomly selecting an edge from a path between two random knots
    knots = node_split$knot_ids[sample.int(length(node_split$knot_ids), 2, replace = F)]
    path_knots = all_simple_paths(self$spanning_tree, knots[1], knots[2])[[1]]
    idx = sample.int(length(path_knots) - 1, 1)
    edge_idx_removed = GetEdgeIdCompat(self$spanning_tree, path_knots[c(idx, idx + 1)])
    edge_removed = E(self$spanning_tree)[edge_idx_removed]

    st_subgraph = delete_edges(self$spanning_tree, edge_removed)
    connect_comp = components(st_subgraph)
    memberships_knot = connect_comp$membership

    # get membership of observations
    memberships = memberships_knot[ projection[node_split$ids] ]

    is_in_1 = (memberships == 1)
    ids_1 = node_split$ids[is_in_1]
    ids_2 = node_split$ids[!is_in_1]

    knot_is_in_1 = (memberships_knot[node_split$knot_ids] == 1)
    knot_ids_1 = node_split$knot_ids[knot_is_in_1]
    knot_ids_2 = node_split$knot_ids[!knot_is_in_1]

    split_rule = SplitRuleSpatial$new('removing_edge', memberships_knot, edge_removed$eid)

    split_proposal = SplitProposal$new(node_split, split_rule, ids_1, ids_2, knot_ids_1, knot_ids_2)

    # check if children are splittable
    splittable_by_x = node_split$IsSplittable('by_x')
    child_1_splittable = (splittable_by_x | length(knot_ids_1) > 1)
    child_2_splittable = (splittable_by_x | length(knot_ids_2) > 1)

    return(list('proposal' = split_proposal, 'n_splittables' = length(candidate_nodes),
                'child_1_splittable' = child_1_splittable, 'child_2_splittable' = child_2_splittable))
  },

  # function to propose splitting a terminal node
  ProposeSplit = function(splittables, prob_split_by_x, X, X_cutpoints, alpha, beta,
                          rb, rs, rhy, e_m, sigmasq_y, sigmasq_mu, projection) {
    if(runif(1) <= prob_split_by_x) {
      # split by covariates
      propose_res = self$ProposeSplitByX(splittables$x, X, X_cutpoints, projection)
    } else {
      # split by spatial locations
      propose_res = self$ProposeSplitByLocation(splittables$loc, projection)
    }

    ## compute acceptance probability
    split_proposal = propose_res$proposal
    node_split = split_proposal$node_split
    depth = node_split$depth
    n_splittables = propose_res$n_splittables

    # compute log-prior ratio
    if(propose_res$child_1_splittable) {
      log_prior_child_1 = log(1 - alpha * (2+depth)^(-beta))
    } else {
      log_prior_child_1 = 0
    }
    if(propose_res$child_2_splittable) {
      log_prior_child_2 = log(1 - alpha * (2+depth)^(-beta))
    } else {
      log_prior_child_2 = 0
    }
    log_A = log(alpha) - beta*log(1 + depth) + log_prior_child_1 + log_prior_child_2 - log(1 - alpha * (1+depth)^(-beta))

    # compute log-proposal ratio
    n_mergeables_new = self$n_mergeables + 1  # node_split becomes mergeable
    if(!is.null(node_split$parent))
      if(node_split$parent$IsMergeable())
        n_mergeables_new = n_mergeables_new - 1  # parent of node_split is no longer mergeable

    if (length(splittables$x) > 1 | length(splittables$loc) > 1) {
      rd_new = (1 - rs - rhy) / 2
    } else if (propose_res$child_1_splittable | propose_res$child_2_splittable) {
      rd_new = (1 - rs - rhy) / 2
    } else if (length(splittables$x) == 1 & length(splittables$loc) == 1 & splittables$loc[1] == splittables$x[1]) {
      rd_new = (1 - rs - rhy) / 2
    } else {
      rd_new = 1 - rs - rhy
    }

    log_P = log(rd_new) - log(rb) + log(n_splittables) - log(n_mergeables_new)

    # compute log-likelihood ratio
    log_L = EvalLogLikeRatio('split', e_m, split_proposal$ids_1, split_proposal$ids_2, sigmasq_y, sigmasq_mu)

    #acceptance probability
    acc_prob = min(0, log_A + log_P + log_L)
    acc_prob = exp(acc_prob)
    split_proposal$acc_prob = acc_prob
    split_proposal$n_mergeables_new = n_mergeables_new

    return(split_proposal)
  },

  # function to split a terminal node
  Split = function(split_proposal, projection) {
    stopifnot(class(split_proposal)[1] == 'SplitProposal')
    node_split = split_proposal$node_split
    node_split$Split(split_proposal$split_rule, split_proposal$ids_1, split_proposal$ids_2,
                     split_proposal$knot_ids_1, split_proposal$knot_ids_2)

    self$n_terminals = self$n_terminals + 1
    self$n_mergeables = split_proposal$n_mergeables_new
  },

  # function to propose merge two children of a non-terminal node
  ProposeMerge = function(alpha, beta, rd, rs, rhy, e_m, sigmasq_y, sigmasq_mu,
                          n_x_splittables, n_loc_splittables) {
    # find candidate node to merge
    candidate_nodes = self$GetMergeableNodes()
    node_merge = candidate_nodes[[ sample.int(length(candidate_nodes), 1) ]]

    # observations in children
    ids_1 = node_merge$children[[1]]$ids
    ids_2 = node_merge$children[[2]]$ids

    ## compute acceptance prob
    depth = node_merge$depth
    n_mergeables = length(candidate_nodes)

    # compute log-prior ratio
    x_splittable_1 = node_merge$children[[1]]$IsSplittable('by_x')
    loc_splittable_1 = node_merge$children[[1]]$IsSplittable('by_location')
    x_splittable_2 = node_merge$children[[2]]$IsSplittable('by_x')
    loc_splittable_2 = node_merge$children[[2]]$IsSplittable('by_location')

    if(x_splittable_1 | loc_splittable_1) {
      log_prior_child_1 = log(1 - alpha * (2+depth)^(-beta))
    } else {
      log_prior_child_1 = 0
    }
    if(x_splittable_2 | loc_splittable_2) {
      log_prior_child_2 = log(1 - alpha * (2+depth)^(-beta))
    } else {
      log_prior_child_2 = 0
    }
    log_A = -( log(alpha) - beta*log(1 + depth) + log_prior_child_1 + log_prior_child_2 -
                 log(1 - alpha * (1+depth)^(-beta)) )

    # compute log-proposal ratio
    n_terminals_new = self$n_terminals - 1
    if(n_terminals_new == 1) {
      rb_new = 1 - rs - rhy
    } else {
      rb_new = (1 - rs - rhy) / 2
    }

    if(class(node_merge$split_rule)[1] == 'SplitRuleX') {
      n_splittables_new = n_x_splittables + 1
      if(x_splittable_1)
        n_splittables_new = n_splittables_new - 1
      if(x_splittable_2)
        n_splittables_new = n_splittables_new - 1
    } else {
      n_splittables_new = n_loc_splittables + 1
      if(loc_splittable_1)
        n_splittables_new = n_splittables_new - 1
      if(loc_splittable_2)
        n_splittables_new = n_splittables_new - 1
    }

    log_P = -( log(rd) - log(rb_new) + log(n_splittables_new) - log(n_mergeables) )

    # compute log-likelihood ratio
    log_L = EvalLogLikeRatio('merge', e_m, ids_1, ids_2, sigmasq_y, sigmasq_mu)

    # acceptance probability
    acc_prob = min(0, log_A + log_P + log_L)
    acc_prob = exp(acc_prob)

    return( MergeProposal$new(node_merge, acc_prob) )
  },

  # function to merge two children of a non-terminal node
  Merge = function(merge_proposal) {
    stopifnot(class(merge_proposal)[1] == 'MergeProposal')
    node_merge = merge_proposal$node_merge
    node_merge$Merge()

    self$n_terminals = self$n_terminals - 1
    self$n_mergeables = self$n_mergeables - 1  # node_merge is no longer mergeable
    if(!is.null(node_merge$parent))
      if(node_merge$parent$IsMergeable())
        self$n_mergeables = self$n_mergeables + 1  # parent of node_merge becomes mergeable
  },

  # function to get height of decision tree
  GetHeight = function() {
    height = 0
    # DFS
    stack = collections::stack()
    stack$push(self$decision_root)
    while (stack$size() > 0) {
      node = stack$pop()
      if (node$is_terminal) {
        height = max(height, node$depth + 1)
      } else {
        stack$push(node$children[[2]])
        stack$push(node$children[[1]])
      }
    }
    return(height)
  },

  # function to evaluate log prior
  # probability that a node is non-terminal is alpha * (1 + d) ^ (-beta)
  EvalLogPrior = function(alpha, beta, prob_split_by_x, X) {
    log_prior = 0
    # DFS
    stack = collections::stack()
    stack$push(self$decision_root)
    while(stack$size() > 0) {
      node = stack$pop()
      if(node$is_terminal) {
        log_prior = log_prior + log(1 - alpha * (1 + node$depth) ^ (-beta))
      } else {
        log_prior = log_prior + log(alpha) - beta * log(1 + node$depth)
        split_rule = node$split_rule
        if(class(split_rule)[1] == 'SplitRuleSpatial') {
          log_prior = log_prior + log(1 - prob_split_by_x)
          if(split_rule$type == 'removing_edge') {
            log_prior = log_prior - log(length(node$ids) - 1)
          } else {
            log_prior = log_prior - log(split_rule$n_components)
          }

        } else {
          # split by covariates
          X_node = X[node$ids, , drop = F]
          p = ncol(X)
          xrange = range(X_node[, split_rule$pid])
          log_prior = log_prior + log(prob_split_by_x)
          log_prior = log_prior - log(p) - log(xrange[2] - xrange[1])
        }

        stack$push(node$children[[2]])
        stack$push(node$children[[1]])
      }
    }
    return(log_prior)
  },

  # function to sample mu for each terminal node
  # e: responses Y (single tree model) or partial residual (additive model)
  # return: fitted values of length n
  UpdateMu = function(e, sigmasq_y, sigmasq_mu) {
    # get all terminal nodes
    terminal_nodes = self$GetTerminalNodes()
    # get cluster size
    csize = sapply(terminal_nodes, function(node) length(node$ids))
    # get sum(e) within each terminal node / cluster
    e_sum = sapply(terminal_nodes, function(node) sum(e[node$ids]))

    # sample mu
    Qinv_diag = 1 / (csize / sigmasq_y + 1 / sigmasq_mu)
    b = Qinv_diag * e_sum / sigmasq_y
    mu = rnorm(length(terminal_nodes), b, sqrt(Qinv_diag))
    g = rep(0, length(e))  # fitted values
    for(i in 1:length(terminal_nodes)) {
      terminal_nodes[[i]]$mu = mu[i]
      g[terminal_nodes[[i]]$ids] = mu[i]
    }

    return(g)
  },

  # function for prediction
  # when arguments are NULL, return in-sample prediction
  # using knots for out-of-sample prediction
  Predict2 = function(X_new = NULL, nn_idx = NULL) {
    if(is.null(X_new) | is.null(nn_idx)) {
      ## in-sample prediction
      # get all terminal nodes
      terminal_nodes = self$GetTerminalNodes()

      n = length(self$decision_root$ids)
      g = rep(0, n)

      for(i in 1:length(terminal_nodes)) {
        node = terminal_nodes[[i]]
        g[node$ids] = node$mu
      }
      return(g)
    } else {
      ## out-of-sample prediction
      n_new = nrow(X_new)
      g_new = numeric(n_new)
      self$decision_root$ids_new = 1:n_new
      if (class(nn_idx) == 'numeric')
        nn_idx = as.matrix(nn_idx, ncol = 1)

      # DFS
      stack = collections::stack()
      stack$push(self$decision_root)
      while(stack$size() > 0) {
        node = stack$pop()
        if(node$is_terminal) {
          g_new[node$ids_new] = node$mu
        } else {
          split_rule = node$split_rule
          if(class(split_rule)[1] == 'SplitRuleX') {
            node$PredictByX(X_new)
          } else {
            node$PredictByLocation2(nn_idx)
          }
          stack$push(node$children[[2]])
          stack$push(node$children[[1]])
        }
      }

      return(g_new)
    }
  }
))

### Partition proposal class ----

SplitProposal <- R6Class('SplitProposal', list(
  node_split = NULL,  # terminal node to split
  acc_prob = NULL,   # acceptance probability
  split_rule = NULL,  # splitting rule
  ids_1 = NULL,  # observation ids in the first child
  ids_2 = NULL,  # observation ids in the second child
  # is_in_1 = NULL, # bool vector: is_in_1[i] == TRUE if node$ids[i] should be include in its first child
  n_mergeables_new = NULL,  # number of mergeable nodes in the partition after split
  knot_ids_1 = NULL,
  knot_ids_2 = NULL,

  initialize = function(node_split = NULL, split_rule = NULL,
                        ids_1 = NULL, ids_2 = NULL, knot_ids_1 = NULL, knot_ids_2 = NULL) {
    self$node_split = node_split
    self$split_rule = split_rule
    self$ids_1 = ids_1
    self$ids_2 = ids_2
    self$knot_ids_1 = knot_ids_1
    self$knot_ids_2 = knot_ids_2
  }
))

MergeProposal <- R6Class('MergeProposal', list(
  node_merge = NULL,  # node whose two children are to be merged
  acc_prob = NULL,    # acceptance probability

  initialize = function(node_merge, acc_prob) {
    self$node_merge = node_merge
    self$acc_prob = acc_prob
  }
))


### Splitting rules class ----

SplitRule <- R6Class('SplitRule')

SplitRuleSpatial <- R6Class('SplitRuleSpatial', inherit = SplitRule,
                            public = list(
                              # type of spatial splitting rule
                              # can be either 'removing_edge' (split by removing an edge from spanning tree)
                              # or 'component' (split by treating a connected component as a new cluster)
                              type = NULL,

                              # edge id of the removed edge from spanning tree
                              # NULL when type == 'component'
                              eid_removed = NULL,

                              # memberhsips of knots after split
                              memberships_knot = NULL,

                              initialize = function(type, memberships_knot = NULL, eid_removed = NULL) {
                                stopifnot(type == 'removing_edge' | type == 'component')
                                stopifnot(is.null(eid_removed) | (is.integer(eid_removed) & length(eid_removed) == 1) )

                                self$type = type
                                if(self$type == 'removing_edge') {
                                  self$eid_removed = eid_removed
                                  self$memberships_knot = memberships_knot
                                }
                              }
                            )
)

SplitRuleX <- R6Class('SplitRuleX', inherit = SplitRule,
                      public = list(
                        xid = NULL,  # index of covariate used to split (from 1 to p)
                        cutoff = NULL,  # cutoff value
                        cutpoint_id = NULL, # idx of cutoff in X_cutpoints

                        initialize = function(xid, cutoff, cutpoint_id) {
                          stopifnot(is.integer(xid) & length(xid) == 1)
                          stopifnot(is.numeric(cutoff) & length(cutoff) == 1)

                          self$xid = xid
                          self$cutoff = cutoff
                          self$cutpoint_id = cutpoint_id
                        }
                      )
)

### Utils -----

# function to obtain indices of n smallest values in a vector
WhichNMin <- function(x, n = 1) {
  n = min(n, length(x))
  if (n == 1)
    return(which.min(x))

  threshold = sort(x, partial = n)[n]
  which_n_min = which(x <= threshold)
  # deal with ties
  if(length(which_n_min) > n) {
    x = x[which_n_min]
    which_n_min = which_n_min[order(x)[1:n]]
  }
  return(which_n_min)
}

### Model class ----

# function to get log likelihood ratio
EvalLogLikeRatio <- function(move, e_m, ids_1, ids_2, sigmasq_y, sigmasq_mu) {
  sigma_ratio = sigmasq_y / sigmasq_mu
  csize_1 = length(ids_1); csize_2 = length(ids_2)
  sum_e_1 = sum(e_m[ids_1]); sum_e_2 = sum(e_m[ids_2])

  if(move == 'split') {
    logdetdiff = -0.5 * (log(csize_1+sigma_ratio) + log(csize_2+sigma_ratio) -
                           log(csize_1+csize_2+sigma_ratio) - log(sigma_ratio))
    quaddiff = 0.5 * (sum_e_1^2/(csize_1+sigma_ratio) + sum_e_2^2/(csize_2+sigma_ratio) -
                        (sum_e_1+sum_e_2)^2/(csize_1+csize_2+sigma_ratio)) / sigmasq_y
  }

  if(move == 'merge') {
    logdetdiff = -0.5 * (log(csize_1+csize_2+sigma_ratio) - log(csize_1+sigma_ratio) -
                           log(csize_2+sigma_ratio) + log(sigma_ratio))
    quaddiff = 0.5 * ((sum_e_1+sum_e_2)^2/(csize_1+csize_2+sigma_ratio) - sum_e_1^2/(csize_1+sigma_ratio) -
                        sum_e_2^2/(csize_2+sigma_ratio)) / sigmasq_y
  }
  return(logdetdiff + quaddiff)
}

# function to copy decision trees
CopyTree <- function(decision_root) {
  decision_root_new = decision_root$clone(deep = TRUE)
  # DFS
  stack = collections::stack()
  stack$push(list(decision_root_new, decision_root))
  while (stack$size() > 0) {
    node_new = stack$peek()[[1]]
    node = stack$peek()[[2]]
    stack$pop()
    if (!node$is_terminal) {
      # copy children
      node_new$children[[1]] = node$children[[1]]$clone(deep = TRUE)
      node_new$children[[2]] = node$children[[2]]$clone(deep = TRUE)

      stack$push(list(node_new$children[[2]], node$children[[2]]))
      stack$push(list(node_new$children[[1]], node$children[[1]]))
    }
  }
  return(decision_root_new)
}

# function to deep copy partitions
CopyPartitions <- function(partitions, keep_spanning_tree = FALSE) {
  partitions_copy = list()
  for(i in 1:length(partitions)) {
    partitions_copy = c(partitions_copy, partitions[[i]]$clone(deep = TRUE))
    partitions_copy[[i]]$decision_root = CopyTree(partitions[[i]]$decision_root)
    if(!keep_spanning_tree)
      partitions_copy[[i]]$spanning_tree = NULL
  }
  return(partitions_copy)
}

Model <- R6Class('Model', list(
  Y = NULL,
  X = NULL,
  coords = NULL,
  graphs = NULL,
  std_par = NULL,       # parameters to unstandardize Y
  X_cutpoints = NULL,   # cutting points of X (p * numcut matrix)
  projections = NULL,   # mapping from observed locations to knots
  n_knots = NULL,       # number of knots

  hyperpar = NULL,  # hyperparameters
  init_val = NULL,  # initial values

  ## for out-of-sample prediction
  X_new = NULL,
  coords_new = NULL,
  projections_new = NULL,  # array with dimension (nrow(X_new), 1, M)

  ## MCMC samples
  partition_out = NULL,   # samples of tree partition
  sigmasq_y_out = NULL,   # samples of noise variance
  g_out = NULL,           # samples of fitted value from each tree
  importance_out = NULL,  # samples of feature importance
  Y_new_out = NULL,       # samples of predicted value

  ## MCMC settings
  MCMC = NULL,
  BURNIN = NULL,
  THIN = NULL,

  initialize = function(Y, X, graphs, projections, hyperpar,
                        X_new = NULL, projections_new = NULL) {
    self$Standardize(Y)
    self$hyperpar = hyperpar
    self$graphs = graphs
    self$projections = projections
    self$n_knots = sapply(graphs, vcount)

    self$X_new = X_new
    self$projections_new = array(projections_new, dim = c(nrow(projections_new), 1, ncol(projections_new)))

    for (m in 1:length(graphs)) {
      if('name' %in% names(vertex_attr(self$graphs[[m]]))) {
        self$graphs[[m]] = delete_vertex_attr(self$graphs[[m]], 'name')
      }
    }

    # find lambda_s
    nu = self$hyperpar['nu']; q = self$hyperpar['q']
    quant = qchisq(1-q, nu)
    self$hyperpar['lambda_s'] = quant * var(self$Y) / nu

    # get cutting points of X
    numcut = self$hyperpar['numcut']
    res = BART::bartModelMatrix(X, numcut, rm.const = T)
    self$X = res$X
    self$X_cutpoints = res$xinfo

    # remove constant covariate in test data
    if (!is.null(X_new))
      X_new = X_new[, res$rm.const]
  },

  # function to fit model by MCMC
  Fit = function(init_val, MCMC, BURNIN, THIN, seed = 1234, save_partitions = F) {
    set.seed(seed)

    self$init_val = init_val
    self$MCMC = MCMC
    self$BURNIN = BURNIN
    self$THIN = THIN

    # check if we need to do prediction
    prediction_flag = !(is.null(self$X_new) | is.null(self$projections_new))

    # set initial spanning tree as minimal spanning tree / forest
    if(is.null(self$init_val$spanning_tree)) {
      self$init_val$spanning_tree = list()
      for (m in 1:length(self$graphs))
        self$init_val$spanning_tree[[m]] = mst(self$graphs[[m]])
    }
    for (m in 1:length(self$graphs)) {
      self$graphs[[m]] = delete_edge_attr(self$graphs[[m]], 'weight')
      self$init_val$spanning_tree[[m]] = delete_edge_attr(self$init_val$spanning_tree[[m]], 'weight')
    }

    n = nrow(self$X)
    p = ncol(self$X)
    # hyper-parameters
    M = self$hyperpar['M']  # number of trees
    sigmasq_mu = self$hyperpar['sigmasq_mu']
    lambda_s = self$hyperpar['lambda_s']
    nu = self$hyperpar['nu']
    alpha = self$hyperpar['alpha']
    beta = self$hyperpar['beta']
    prob_split_by_x_0 = self$hyperpar['prob_split_by_x']

    # check sizes
    if(length(self$graphs) != M)
      stop('Number of spatial graphs should be equal to the number of trees')

    # initialize
    sigmasq_y = init_val[['sigmasq_y']]
    partitions = list()
    numcut = apply(self$X_cutpoints, 1, function(x) max(which(!is.na(x))))  # effective number of cutpoints
    for(m in 1:M) {
      partitions[[m]] = Partition$new(self$init_val$spanning_tree[[m]],
                                      ncol(self$X), numcut,
                                      self$projections[, m], self$n_knots[m])
    }
    g = matrix(0, nrow = n, ncol = M) # n*M matrix of fitted mu's
    Y_hat = rep(0, n)

    cnt_split = rep(0, p + 1)  # number of splits by each covariate and spatial coords

    # setup result containers
    if (save_partitions)
      self$partition_out = list()
    self$sigmasq_y_out = numeric((MCMC-BURNIN)/THIN)
    self$g_out = array(0, dim = c((MCMC-BURNIN)/THIN, n, M))
    self$importance_out = matrix(0, nrow = (MCMC-BURNIN)/THIN, ncol = p + 1)
    if (prediction_flag) {
      n_new = nrow(self$X_new)
      self$Y_new_out = matrix(0, nrow = (MCMC-BURNIN)/THIN, ncol = n_new)
    }

    ## Run MCMC
    for(iter in 1:MCMC) {
      for(m in 1:M) {
        e_m = self$Y - (Y_hat - g[, m])

        # get splittable terminal nodes
        splittables = partitions[[m]]$GetSplittableNodes(type = 'all')

        # propose a move
        rs = 0; rhy = 0  # depreciated
        if(partitions[[m]]$n_terminals == 1) {
          rb = 1 - (rs + rhy)
          rd = 0
        } else if( length(splittables$x) == 0 & length(splittables$loc) == 0 ) {
          rb = 0
          rd = 1 - (rs + rhy)
        } else {
          rb = (1 - (rs + rhy)) / 2
          rd = rb
        }
        move = sample(4, 1, prob = c(rb, rd, rs, rhy))

        if(move == 1) { ## birth move
          # split a terminal node
          if(length(splittables$x) > 0 & length(splittables$loc) > 0) {
            prob_split_by_x = prob_split_by_x_0
          } else if(length(splittables$x) == 0) {
            prob_split_by_x = 0
          } else {
            prob_split_by_x = 1
          }

          split_proposal = partitions[[m]]$ProposeSplit(splittables, prob_split_by_x, self$X, self$X_cutpoints,
                                                        alpha, beta, rb, rs, rhy, e_m, sigmasq_y, sigmasq_mu,
                                                        self$projections[, m])
          acc_prob = split_proposal$acc_prob
          if(runif(1) < acc_prob){
            # accept
            partitions[[m]]$Split(split_proposal, self$projections[, m])

            # update covariate split counts
            split_rule = split_proposal$split_rule
            if (class(split_rule)[1] == 'SplitRuleX') {
              cnt_split[split_rule$xid] = cnt_split[split_rule$xid] + 1
            } else {
              cnt_split[p + 1] = cnt_split[p + 1] + 1
            }
          }
        }

        if(move == 2) { ## death move
          # merge a node
          merge_proposal = partitions[[m]]$ProposeMerge(alpha, beta, rd, rs, rhy, e_m, sigmasq_y, sigmasq_mu,
                                                        length(splittables$x), length(splittables$loc))
          acc_prob = merge_proposal$acc_prob
          if(runif(1) < acc_prob){
            # update covariate split counts
            split_rule = merge_proposal$node_merge$split_rule
            if (class(split_rule)[1] == 'SplitRuleX') {
              cnt_split[split_rule$xid] = cnt_split[split_rule$xid] - 1
            } else {
              cnt_split[p + 1] = cnt_split[p + 1] - 1
            }

            # accept
            partitions[[m]]$Merge(merge_proposal)
          }
        }

        # update mu_m and partial residual
        Y_hat = Y_hat - g[, m]
        g[, m] = partitions[[m]]$UpdateMu(e_m, sigmasq_y, sigmasq_mu)
        Y_hat = Y_hat + g[, m]
      }

      # update sigmasq_y
      rate = 0.5*(nu*lambda_s + sum((self$Y - Y_hat)^2))
      sigmasq_y = 1/rgamma(1, shape = (n+nu)/2, rate = rate)

      # save result
      if(iter > BURNIN & (iter - BURNIN) %% THIN == 0) {
        if (save_partitions)
          self$partition_out[[(iter-BURNIN)/THIN]] = CopyPartitions(partitions)
        self$sigmasq_y_out[(iter-BURNIN)/THIN] = sigmasq_y
        self$g_out[(iter-BURNIN)/THIN, , ] = g
        self$importance_out[(iter-BURNIN)/THIN, ] = cnt_split

        # out-os-sample prediction
        if (prediction_flag) {
          g_new = matrix(0, nrow = n_new, ncol = M)
          for(m in 1:M)
            g_new[, m] = partitions[[m]]$Predict2(self$X_new, self$projections_new[, , m])
          self$Y_new_out[(iter-BURNIN)/THIN, ] = rowSums(g_new)
        }
      }

      if(iter %% 100 == 0)
        cat('Iteration', iter, 'done\n')
    }

    # post-processing
    colnames(self$importance_out) = c(colnames(self$X), 'spatial')
    self$sigmasq_y_out = self$Unstandardize(self$sigmasq_y_out, nomean = T, s2 = T)
    if (prediction_flag)
      self$Y_new_out = self$Unstandardize(self$Y_new_out)

  },

  # function to standardize Y
  Standardize = function(Y) {
    ymean = mean(Y)
    Y = Y - ymean
    yscale = 2 * max(abs(Y))
    self$Y = Y / yscale
    self$std_par = c('mean' = ymean, 'scale' = yscale)
  },

  # function to unstandardize
  Unstandardize = function(x, nomean = F, s2 = F) {
    if(s2) {
      x = x * self$std_par['scale'] ^ 2
    } else {
      x = x * self$std_par['scale']
    }
    if(!nomean) x = x + self$std_par['mean']
    return(x)
  },

  # function to compute WAIC
  ComputeWAIC = function(penalty = 2) {
    Y_post = apply(self$g_out, c(1, 2), sum)
    Y_post = self$Unstandardize(Y_post)
    Y = self$Unstandardize(self$Y)
    n = ncol(Y_post); n_post = nrow(Y_post)

    # compute log likelihood for each observation at each posterior sample
    residual_sq = t(apply(Y_post, 1, function(Y_hat) (Y - Y_hat) ^ 2))  # n_post * n
    log_like = -log(self$sigmasq_y_out)/2 - residual_sq / (2*self$sigmasq_y_out) - log(2*pi)/2

    # compute WAIC
    log_mean_like = apply(log_like, 2, logSumExp) - log(n_post)
    mean_log_like = colMeans(log_like)
    sum_log_mean_like = sum(log_mean_like)

    p_WAIC1 = 2 * sum(log_mean_like - mean_log_like)
    WAIC1 = -2 * sum_log_mean_like + penalty * p_WAIC1

    p_WAIC2 = sum( apply(log_like, 2, var) )
    WAIC2 = -2 * sum_log_mean_like + penalty * p_WAIC2

    return(c('WAIC1' = WAIC1, 'WAIC2' = WAIC2))
  },


  # function to predict
  # when arguments are NULL, return in-sample prediction
  # using knots for out-of-sample prediction
  Predict2 = function(X_new = NULL, projections_new = NULL, return_all = F, seed = 12345) {
    if(is.null(X_new) | is.null(projections_new)) {
      ## in-sample prediction using posterior means
      Y_post = apply(self$g_out, c(1, 2), sum)
      Y_hat = self$Unstandardize(Y_post)
      if (!return_all)
        Y_hat = colMeans(Y_hat)
      return(Y_hat)
    } else {
      ## out-of-sample prediction using posterior means
      if (length(self$partitions_out) == 0)
        stop("Partitions were not saved")
      set.seed(seed)

      M = self$hyperpar['M']
      n_post = length(self$sigmasq_y_out)
      g_new = array(0, dim = c(n_post, nrow(X_new), M))

      for(i in 1:n_post) {
        partitions = self$partition_out[[i]]
        for(m in 1:M)
          g_new[i, , m] = partitions[[m]]$Predict2(X_new, projections_new[, , m])
      }
      Y_new_post = apply(g_new, c(1, 2), sum)
      Y_new_hat = self$Unstandardize(Y_new_post)
      if (!return_all)
        Y_new_hat = colMeans(Y_new_hat)
      return(Y_new_hat)
    }
  }
))

### Utils -----

# function to compute log-sum-exp
logSumExp <- function(x) {
  x_max = max(x)
  return(x_max + log( sum(exp(x - x_max)) ))
}


bamdt_as_binary_adjacency <- function(A, name) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) {
    stop(name, " must be a square adjacency matrix")
  }

  A[is.na(A)] <- 0
  A <- 1 * (A > 0)
  diag(A) <- 0
  if (!isTRUE(all.equal(A, t(A)))) {
    A <- pmax(A, t(A))
  }
  storage.mode(A) <- "numeric"
  A
}


bamdt_order_adjacencies <- function(A, A_full, n_train, n_test,
                                    verbose_data_out = NULL) {
  A_full <- bamdt_as_binary_adjacency(A_full, "A_full")
  expected_dim <- c(n_train + n_test, n_train + n_test)

  if (identical(dim(A_full), expected_dim)) {
    A_full_ordered <- A_full
  } else {
    nodeids <- c(verbose_data_out$nodeids, verbose_data_out$nodeids_test)
    if (length(nodeids) != sum(expected_dim) / 2) {
      stop("A_full is not ordered as train/test and node IDs are unavailable.")
    }
    if (!is.null(rownames(A_full)) &&
        all(as.character(nodeids) %in% rownames(A_full))) {
      node_idx <- as.character(nodeids)
    } else {
      node_idx <- suppressWarnings(as.integer(nodeids))
      if (any(is.na(node_idx)) ||
          any(node_idx < 1) ||
          any(node_idx > nrow(A_full))) {
        stop("Could not map node IDs into A_full.")
      }
    }
    A_full_ordered <- A_full[node_idx, node_idx, drop = FALSE]
  }

  if (!is.null(A) && identical(dim(as.matrix(A)), c(n_train, n_train))) {
    A_train <- bamdt_as_binary_adjacency(A, "A")
  } else {
    A_train <- A_full_ordered[seq_len(n_train), seq_len(n_train), drop = FALSE]
  }

  list(A = A_train, A_full = A_full_ordered)
}


bamdt_adjacency_to_graph <- function(A, name) {
  graph <- igraph::graph_from_adjacency_matrix(
    A > 0, mode = "undirected", diag = FALSE
  )

  cc <- igraph::components(graph)
  if (cc$no > 1) {
    reps <- vapply(
      seq_len(cc$no),
      function(k) which(cc$membership == k)[1],
      integer(1)
    )
    bridge_edges <- as.vector(rbind(reps[-length(reps)], reps[-1]))
    graph <- igraph::add_edges(graph, bridge_edges)
  }

  if (igraph::ecount(graph) > 0) {
    igraph::E(graph)$weight <- rep(1, igraph::ecount(graph))
    igraph::E(graph)$eid <- as.integer(seq_len(igraph::ecount(graph)))
  }
  igraph::V(graph)$vid <- as.integer(seq_len(igraph::vcount(graph)))
  graph
}


bamdt_nearest_train_projections <- function(A_full, n_train, n_test, M, seed) {
  if (!identical(dim(A_full), c(n_train + n_test, n_train + n_test))) {
    stop("A_full must have dimensions (nrow(x) + nrow(x_test)) squared")
  }
  if (n_test == 0) {
    return(matrix(integer(), nrow = 0, ncol = M))
  }

  full_graph <- bamdt_adjacency_to_graph(A_full, "A_full")
  dist_to_train <- igraph::distances(
    full_graph,
    v = n_train + seq_len(n_test),
    to = seq_len(n_train)
  )

  set.seed(seed)
  projections_test <- matrix(0L, nrow = n_test, ncol = M)
  for (i in seq_len(n_test)) {
    min_dist <- min(dist_to_train[i, ], na.rm = TRUE)
    if (!is.finite(min_dist)) {
      stop("Test observation ", i, " is not connected to any training node")
    }

    nearest <- which(dist_to_train[i, ] == min_dist)
    projections_test[i, ] <- sample(nearest, M, replace = TRUE)
  }

  projections_test
}
