use aoc_lib::utils::parsing_input;
use core::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edge(String, String);

impl FromStr for Edge {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split('-');
        let n1 = iter.next().ok_or("no items in string")?;
        let n2 = iter.next().ok_or("no second item in string")?;
        iter.next()
            .is_none()
            .then(move || Edge(n1.to_string(), n2.to_string()))
            .ok_or("more than 2 items in string")
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum CaveType {
    Small,
    Big,
    Terminal,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Node {
    id: usize,
    cave_type: CaveType,
}

pub const NODE_START: Node = Node {
    id: 0,
    cave_type: CaveType::Terminal,
};
pub const NODE_END: Node = Node {
    id: 1,
    cave_type: CaveType::Terminal,
};

impl Node {
    fn make_builder() -> impl (FnMut(String) -> Self) {
        let mut node_id_lookup = std::collections::HashMap::<String, Node>::new();
        node_id_lookup.insert("start".to_string(), NODE_START);
        node_id_lookup.insert("end".to_string(), NODE_END);

        move |s| {
            if let Some(&node) = node_id_lookup.get(&s) {
                node
            } else {
                let node = Self {
                    id: node_id_lookup.len(),
                    cave_type: if s.to_uppercase() == s {
                        CaveType::Big
                    } else {
                        CaveType::Small
                    },
                };
                println!("node mapping: {:?} == {:?}", s, node.id);
                node_id_lookup.insert(s, node);

                node
            }
        }
    }
}

#[derive(Debug)]
pub struct Graph {
    edges: Vec<Vec<usize>>,
    cave_types: Vec<CaveType>,
}

impl Graph {
    pub fn vertices(&self) -> usize {
        self.edges.len()
    }
    pub fn new<I: Iterator<Item = Edge>>(iter: I) -> Self {
        let mut builder = Node::make_builder();
        let result = Self {
            edges: vec![Vec::new(), Vec::new()],
            cave_types: vec![CaveType::Terminal, CaveType::Terminal],
        };

        iter.fold(result, |mut result, Edge(s1, s2)| {
            let n1 = builder(s1);
            if n1.id >= result.edges.len() {
                result.edges.push(Vec::new());
                result.cave_types.push(n1.cave_type)
            }

            let n2 = builder(s2);
            if n2.id >= result.edges.len() {
                result.edges.push(Vec::new());
                result.cave_types.push(n2.cave_type)
            }

            result.edges[n1.id].push(n2.id);
            result.edges[n2.id].push(n1.id);

            result
        })
    }
}

#[derive(Debug, Clone)]
pub struct PathStreamIter<'a> {
    graph: &'a Graph,
    path: Vec<usize>,
    node_exits: Vec<usize>,
    visited: std::collections::HashMap<usize, usize>,
    visited_small_twice: bool,
}

impl<'a> PathStreamIter<'a> {
    pub fn new(graph: &'a Graph) -> PathStreamIter<'a> {
        let mut s = Self {
            graph,
            path: Vec::new(),
            node_exits: Vec::new(),
            visited: std::collections::HashMap::new(),
            visited_small_twice: false,
        };
        s.push_if_valid(NODE_START.id, 0);

        s
    }

    fn push_if_valid(&mut self, new_head: usize, last_exit: usize) -> bool {
        if match self.graph.cave_types[new_head] {
            CaveType::Big => true,
            CaveType::Small => {
                *self.visited.get(&new_head).unwrap_or(&0) < {
                    if self.visited_small_twice {
                        1
                    } else {
                        2
                    }
                }
            }
            CaveType::Terminal => *self.visited.get(&new_head).unwrap_or(&0) == 0,
        } {
            self.path.push(new_head);
            self.node_exits.push(last_exit);

            let new_count = 1 + self.visited.get(&new_head).unwrap_or(&0);
            self.visited.insert(new_head, new_count);

            match (self.graph.cave_types[new_head], new_count) {
                (_, 0) => unreachable!(),
                (CaveType::Big, _) => {}
                (CaveType::Small, 2) => {
                    self.visited_small_twice = true;
                }
                (CaveType::Small | CaveType::Terminal, 1) => {}
                (CaveType::Small | CaveType::Terminal, _) => unreachable!(),
            }

            true
        } else {
            false
        }
    }

    fn pop(&mut self) -> Option<(Node, usize)> {
        let path_head = self.path.pop()?;
        let last_exit = self
            .node_exits
            .pop()
            .expect("`path` and `node_exits` should be the same length");
        let old_count = self.visited.remove(&path_head).unwrap();
        if old_count >= 1 {
            self.visited.insert(path_head, old_count - 1);
        } else {
            unreachable!();
        }
        if self.graph.cave_types[path_head] == CaveType::Small && old_count == 2 {
            self.visited_small_twice = false;
        }

        Some((
            Node {
                id: path_head,
                cave_type: self.graph.cave_types[path_head],
            },
            last_exit,
        ))
    }

    fn head(&self) -> Option<(usize, &usize)> {
        let path_head = (!self.path.is_empty()).then(|| &self.path[self.path.len() - 1])?;
        let last_exit = &self.node_exits[self.path.len() - 1];

        Some((*path_head, last_exit))
    }

    pub fn next_ref(&mut self) -> Option<&Vec<usize>> {
        loop {
            let (path_head, &last_exit) = self.head()?;

            if path_head == NODE_END.id || last_exit == self.graph.edges[path_head].len() {
                self.pop();
                continue;
            }

            for exit in last_exit..self.graph.edges[path_head].len() {
                self.node_exits[self.path.len() - 1] = exit + 1;
                let next_node = self.graph.edges[path_head][exit];

                if self.push_if_valid(next_node, 0) {
                    if next_node == NODE_END.id {
                        return Some(&self.path);
                    }

                    break;
                }
            }
        }
    }

    /*
    fn iter<'b>(&'b mut self) -> impl 'b + Iterator<Item = &'b Vec<Node>> {
        std::iter::repeat(()).filter_map(move |_| self.next())
    }
    */
}

pub fn count_paths(graph: &Graph) -> usize {
    let mut path_iterlike = PathStreamIter::new(graph);

    let mut result = 0;
    while let Some(_path_ref) = path_iterlike.next_ref() {
        result += 1;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_graph_eg1() {
        let cave_edges = vec![
            Edge("start".to_string(), "A".to_string()),
            Edge("start".to_string(), "b".to_string()),
            Edge("A".to_string(), "c".to_string()),
            Edge("A".to_string(), "b".to_string()),
            Edge("b".to_string(), "d".to_string()),
            Edge("A".to_string(), "end".to_string()),
            Edge("b".to_string(), "end".to_string()),
        ];
        let graph = Graph::new(cave_edges.into_iter());

        assert_eq!(
            graph.edges,
            vec![
                vec![2, 3],
                vec![2, 3],
                vec![0, 4, 3, 1],
                vec![0, 2, 5, 1],
                vec![2],
                vec![3],
            ]
        );
    }

    #[test]
    fn test_streamiter_paths_eg1() {
        let graph = Graph {
            edges: vec![
                vec![2, 3],
                vec![2, 3],
                vec![0, 3, 4, 1],
                vec![0, 2, 5, 1],
                vec![2],
                vec![3],
            ],
            cave_types: vec![
                NODE_START.cave_type,
                NODE_END.cave_type,
                CaveType::Big,
                CaveType::Small,
                CaveType::Small,
                CaveType::Small,
            ],
        };
        println!("graph: {:?}\n", graph);

        let mut path_streamiter = PathStreamIter::new(&graph);

        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 3, 2, 3, 2, 4, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 3, 2, 3, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 3, 2, 3, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 3, 2, 4, 2, 3, 2, 1])
        );
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 3, 2, 4, 2, 3, 1])
        );
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 3, 2, 4, 2, 4, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 3, 2, 4, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 3, 2, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 3, 5, 3, 2, 4, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 3, 5, 3, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 3, 5, 3, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 3, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 4, 2, 3, 2, 3, 2, 1])
        );
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 4, 2, 3, 2, 3, 1])
        );
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 4, 2, 3, 2, 4, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 4, 2, 3, 2, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 4, 2, 3, 5, 3, 2, 1])
        );
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 4, 2, 3, 5, 3, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 4, 2, 3, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 4, 2, 4, 2, 3, 2, 1])
        );
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 2, 4, 2, 4, 2, 3, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 4, 2, 4, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 4, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 2, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 3, 2, 3, 2, 4, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 2, 3, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 2, 3, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 3, 2, 4, 2, 3, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 2, 4, 2, 3, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 3, 2, 4, 2, 4, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 2, 4, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 2, 1]));
        assert_eq!(
            path_streamiter.next_ref(),
            Some(&vec![0, 3, 5, 3, 2, 4, 2, 1])
        );
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 5, 3, 2, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 5, 3, 1]));
        assert_eq!(path_streamiter.next_ref(), Some(&vec![0, 3, 1]));
        assert_eq!(path_streamiter.next_ref(), None);
    }

    #[test]
    fn test_count_paths_eg2() {
        let cave_edges = vec![
            Edge("dc".to_string(), "end".to_string()),
            Edge("HN".to_string(), "start".to_string()),
            Edge("start".to_string(), "kj".to_string()),
            Edge("dc".to_string(), "start".to_string()),
            Edge("dc".to_string(), "HN".to_string()),
            Edge("LN".to_string(), "dc".to_string()),
            Edge("HN".to_string(), "end".to_string()),
            Edge("kj".to_string(), "sa".to_string()),
            Edge("kj".to_string(), "HN".to_string()),
            Edge("kj".to_string(), "dc".to_string()),
        ];
        let graph = Graph::new(cave_edges.into_iter());
        println!("graph: {:?}", graph);

        let result = count_paths(&graph);
        assert_eq!(result, 103);
    }

    #[test]
    fn test_count_paths_eg3() {
        let cave_edges = vec![
            Edge("fs".to_string(), "end".to_string()),
            Edge("he".to_string(), "DX".to_string()),
            Edge("fs".to_string(), "he".to_string()),
            Edge("start".to_string(), "DX".to_string()),
            Edge("pj".to_string(), "DX".to_string()),
            Edge("end".to_string(), "zg".to_string()),
            Edge("zg".to_string(), "sl".to_string()),
            Edge("zg".to_string(), "pj".to_string()),
            Edge("pj".to_string(), "he".to_string()),
            Edge("RW".to_string(), "he".to_string()),
            Edge("fs".to_string(), "DX".to_string()),
            Edge("pj".to_string(), "RW".to_string()),
            Edge("zg".to_string(), "RW".to_string()),
            Edge("start".to_string(), "pj".to_string()),
            Edge("he".to_string(), "WI".to_string()),
            Edge("zg".to_string(), "he".to_string()),
            Edge("pj".to_string(), "fs".to_string()),
            Edge("start".to_string(), "RW".to_string()),
        ];
        let graph = Graph::new(cave_edges.into_iter());
        println!("graph: {:?}", graph);

        let result = count_paths(&graph);
        assert_eq!(result, 3509);
    }
}

fn main() {
    println!("Enter input:");
    let stdin = std::io::stdin();
    let parsed_inputs = parsing_input::<_, Edge>(stdin.lock());

    let graph = Graph::new(parsed_inputs);
    let result = count_paths(&graph);
    println!("paths count: {:?}", result);
}
