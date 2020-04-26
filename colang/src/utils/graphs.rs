//! Various extension routines for graph handling with `petgraph`.

use petgraph::graph::IndexType;
use petgraph::graph::NodeIndex;
use petgraph::prelude::*;
use petgraph::visit::{depth_first_search, Control, DfsEvent};

/// Finds any of the simple cycles containing the vertex `start`. It is assumed that `start` is
/// a part of a cycle.
pub fn find_any_cycle<N, E, Ix: IndexType>(
    graph: &DiGraph<N, E, Ix>,
    start: NodeIndex<Ix>,
) -> Vec<NodeIndex<Ix>> {
    let mut predecessor = vec![NodeIndex::end(); graph.node_count()];

    depth_first_search(graph, Some(start), |event| match event {
        DfsEvent::TreeEdge(u, v) => {
            predecessor[v.index()] = u;
            Control::Continue
        }
        DfsEvent::BackEdge(u, v) if v == start => {
            predecessor[v.index()] = u;
            Control::Break(())
        }
        _ => Control::Continue,
    });

    let mut cycle = Vec::new();
    let mut next = predecessor[start.index()];
    while next != start {
        cycle.push(next);
        next = predecessor[next.index()];
    }
    cycle.push(start);

    cycle.reverse();
    cycle
}

#[cfg(test)]
mod tests {
    use super::find_any_cycle;
    use petgraph::graph::node_index as n;
    use petgraph::prelude::*;

    #[test]
    fn finds_single_cycle() {
        let graph: Graph<(), ()> = Graph::from_edges(&[(0, 1), (1, 2), (2, 3), (3, 0)]);

        let cycle = find_any_cycle(&graph, n(2));
        assert_eq!(cycle, vec![n(2), n(3), n(0), n(1)]);
    }

    #[test]
    fn doesnt_include_other_cycles() {
        let graph: Graph<(), ()> =
            Graph::from_edges(&[(0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3)]);

        let cycle = find_any_cycle(&graph, n(3));
        assert_eq!(cycle, vec![n(3), n(4), n(5)]);
    }

    #[test]
    fn finds_self_cycle() {
        let graph: Graph<(), ()> = Graph::from_edges(&[(0, 1), (1, 1)]);

        let cycle = find_any_cycle(&graph, n(1));
        assert_eq!(cycle, vec![n(1)]);
    }
}
