use reflection::{Member, Id};
use trees::{tr, fr, Tree, Forest, Node};

use std::fmt::{self, Display, Formatter};
use std::ops::Range;

#[derive( Debug, Clone, PartialEq, Eq )]
pub struct Column {
    pub(crate) member : Member,
    pub(crate) range  : Range<usize>,
}

impl Column {
    fn new( member: Member, range: Range<usize> ) -> Self { Column{ member, range }}
}

impl Display for Column {
    fn fmt( &self, f: &mut Formatter ) -> fmt::Result {
        write!( f, "{}<{},{}>", self.member, self.range.start, self.range.end )
    }
}

pub fn generate_columns( node: &Node<Member> ) -> Tree<Column> {
    fn forest( nodes: ::trees::Iter<Member>, start: usize ) -> ( Forest<Column>, Range<usize> ) {
        let mut end = start;
        let mut forest = fr::<Column>();
        for node in nodes {
            let col = tree( node, end );
            end = col.root().data.range.end;
            forest.push_back( col );
        }
        ( forest, start..end )
    }

    fn tree( node: &Node<Member>, start: usize ) -> Tree<Column> {
        if node.is_leaf() {
            tr( Column::new( node.data.clone(), start..start+1 ))
        } else {
            let ( children, range ) = forest( node.children(), start );
            tr( Column::new( node.data.clone(), range )) / children
        }
    }

    tree( node, 0 )
}

pub fn column_header( columns: &Tree<Column> ) -> String {
    struct ColNode<'a,'b:'a> {
        next   : usize,
        parent : usize,
        degree : usize,
        node   : &'a Node<&'b Column>,
    }

    fn is_readable( id: Id ) -> bool {
        if let Some(ch) = id.chars().next() {
            if ch.is_alphabetic() {
                return true;
            }
        }
        false
     }

    fn readable_descendants( node: &Node<Column> ) -> ( Forest<&Column>, usize ) {
        node.children().fold( (Forest::<&Column>::new(),0), |(forest,node_cnt), child|
            if is_readable( child.data.member.id() ) {
                let sub_root = Tree::new( &child.data );
                let ( sub_forest, sub_node_cnt ) = readable_descendants( child );
                ( forest - ( sub_root/sub_forest ), node_cnt+sub_node_cnt+1 )
            } else {
                let ( sub_forest, sub_node_cnt ) = readable_descendants( child );
                ( forest-sub_forest, node_cnt+sub_node_cnt )
            }
        )
    }

    let ( readable_columns, node_cnt ) = readable_descendants( columns );
    if node_cnt == 0 {
        return String::new();
    }

    let root_col = Tree::new( &columns.root().data );
    let width = root_col.data.range.end;
    let columns = root_col / readable_columns;

    let mut nodes = Vec::with_capacity( node_cnt );
    nodes.push( ColNode{ next: 1, parent: 0, degree: 0, node: columns.root() });

    let ( mut i, mut j, mut k ) = ( 0_usize, 1_usize, 1_usize ); // i..j for parents, j..k for children
    loop {
        while i < j {
            let node = nodes[i].node;
            for child in node.children() {
                nodes[i].degree += 1;
                nodes.push( ColNode{ next: k+1, parent: i, degree: 0, node: child });
                k += 1;
            }
            i += 1;
        }
        j = k;
        if i+1 >= node_cnt { break; }
        if i == j { break; }
    }

    nodes.last_mut().map( |last| last.next = 0 );
    let mut table = Vec::new();

    while nodes[0].degree != 0 {
        let mut row = Vec::<Id>::with_capacity( width );
        for _ in 0..width { row.push( &"" ); }

        let ( mut prev, mut curr ) = ( 0_usize, 0_usize );
        loop {
            if nodes[ curr ].degree == 0 {
                let next = nodes[ curr ].next;
                nodes[ prev ].next = next;
                let column = &nodes[ curr ].node.data;
                let id = column.member.id();
                row[ column.range.start ] = id;

                let parent = nodes[ curr ].parent;
                nodes[ parent ].degree -= 1;
                curr = next;
            } else {
                prev = curr;
                curr = nodes[ curr ].next;
            }
            if curr == 0 { break; }
        }

        table.push( row );
    }

    fn row_to_string( row: &Vec<Id> ) -> String {
        if let Some( grid ) = row.iter().next() {
            let mut output = grid.to_string();
            for grid in row.iter().skip(1) {
                output.push_str( &("\t".to_string() + *grid ));
            }
            return output;
        } 
        String::new()
    }

    if let Some( row ) = table.iter().rev().next() {
        let mut output = row_to_string( row );
        for row in table.iter().rev().skip(1) {
            output.push_str( &("\n".to_string() + &row_to_string( row )));
        }
        return output + &"\n";
    }
    String::new()
}
