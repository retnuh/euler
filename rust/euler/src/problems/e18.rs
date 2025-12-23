#[cfg(test)]
use crate::debug_println;
#[cfg(test)]
use indoc::indoc;

pub type Coordinate = (usize, usize);

pub struct TriangleGraph {
    weights: Vec<Vec<u64>>,
    distances: Vec<Vec<u64>>,
}

impl TriangleGraph {
    pub fn max_sum(&mut self) -> u64 {
        self.distances[0][0] = self.weights[0][0];
        for x in 0..self.weights.len() - 1 {
            for y in 0..self.weights[x].len() {
                // If going through current gives a longer path to each neighbor, update it
                self.distances[x + 1][y] =
                    self.distances[x + 1][y].max(self.distances[x][y] + self.weights[x + 1][y]);
                self.distances[x + 1][y + 1] = self.distances[x + 1][y + 1]
                    .max(self.distances[x][y] + self.weights[x + 1][y + 1]);
            }
        }
        *self.distances[self.distances.len() - 1]
            .iter()
            .max()
            .unwrap()
    }
}

impl TriangleGraph {
    pub fn new(weights: Vec<Vec<u64>>) -> TriangleGraph {
        let distances: Vec<Vec<u64>> = weights.iter().map(|v| vec![0; v.len()]).collect();
        TriangleGraph { weights, distances }
    }
}

#[test]
fn test_triangle_graph() {
    let data = vec![vec![3], vec![7, 4], vec![2, 4, 6], vec![8, 5, 9, 3]];
    let mut graph = TriangleGraph::new(data);
    let x = graph.max_sum();
    debug_println!("x: {:?}", x);
    assert_eq!(23, x);
}

pub fn maximum_path_sum(triangle_text: &str) -> u64 {
    let data: Vec<Vec<u64>> = triangle_text
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|value| value.parse::<u64>().unwrap())
                .collect()
        })
        .collect();
    let mut graph = TriangleGraph::new(data);
    graph.max_sum()
}

#[test]
fn test_maximum_path_sum() {
    let sample = indoc! {"
        3
        7 4
        2 4 6
        8 5 9 3
    "};
    assert_eq!(23, maximum_path_sum(sample));
}

static E18_DATA: &str = include_str!("resources/e18.txt");

pub fn main() {
    maximum_path_sum(E18_DATA);
}
