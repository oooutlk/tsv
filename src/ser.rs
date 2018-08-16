//! Serialize a Rust data structure into tsv data.

use error::{Error, Result};
use table::{Column, generate_columns, column_header};
use trees::{TreeWalk, Visit};
use reflection::{Reflection, Schema, Member};
use serde::ser::{self, Serialize};
use std::ops::Range;

use super::Config;

/// A structure for serializing Rust values into tsv.
pub struct Serializer {
    columns : TreeWalk<Column>,
    width   : usize,
    header  : Option<String>,
    grids   : Vec<Vec<String>>,
    stack   : Vec<Range<usize>>,
    row     : usize, // row of cursor
    col     : usize, // column of cursor
    config  : Config,
}

impl Serializer {
    /// Creates a new tsv serializer.
    #[inline]
    fn new( schemata: Schema, config: Config ) -> Self {
        let columns = generate_columns( schemata.root() );
        let width   = columns.root().data.range.end;
        let header  = if config.with_header { Some( column_header( &columns ))} else { None };
        Serializer {
            columns : TreeWalk::from( columns ),
            width   ,
            header  ,
            grids   : Vec::default(),
            stack   : vec![ 0..0 ], // ensure non-empty
            row     : 0,
            col     : 0,
            config  ,
        }
    }

    fn write_in_row<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        self.next_column();
        self.row = self.stack.last().unwrap().start;
        self.do_serialize( value )
    }

    fn write_in_col<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        self.print_column();
        self.row = self.stack.last().unwrap().end;
        self.do_serialize( value )
    }

    fn do_serialize<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        let enum_col_end = self.set_column();
        let result = value.serialize( &mut *self );
        enum_col_end.map( |end| self.col = end );
        let row = self.row;
        self.grow_row( row );
        result
    }

    fn write_in_grid( &mut self, field: String ) -> Result<()> {
        if self.set_column().is_some() {
            self.grids[ self.row ][ self.col ] = field;
        } else {
            let row = &mut self.grids[ self.row ];
            for _ in row.len() .. self.col+1 {
                row.push( String::new() );
            }
            row[ self.col ] = field;
        }

        Ok(())
    }

    fn set_column( &mut self ) -> Option<usize> {
        for _ in self.grids.len() .. self.row+1 {
            self.grids.push( Vec::<String>::with_capacity( self.width ));
        }

        let row = &mut self.grids[ self.row ];
        if let Some( visit ) = self.columns.get() {
            let col = &visit.node().data;
            self.col = col.range.start;
            if let Member::Variant(_) = col.member {
                if let Some( parent ) = self.columns.get_parent() {
                    for _ in parent.data.range.start .. parent.data.range.end {
                        row.push( String::new() );
                    }
                    return Some( parent.data.range.end );
                }
            }
        }
        None
    }

    fn push_stack( &mut self ) {
        self.stack.push( self.row..self.row );
    }

    fn pop_stack( &mut self ) -> Result<()> {
        let range = self.stack.pop().unwrap();
        self.grow_row( range.end-1 );
        Ok(())
    }

    fn grow_row( &mut self, row: usize ) {
        let parent = self.stack.last_mut().unwrap();
        if parent.end <= row {
            parent.end = row + 1;
        }
    }

    fn end_col( &mut self ) -> Result<()> {
        self.next_column();

        self.row = self.stack.last().unwrap().end;

        for _ in self.grids.len() .. self.row+1 {
            self.grids.push( Vec::<String>::with_capacity( self.width ));
        }

        let mut range = None;
        if let Some( visit ) = self.columns.get() {
            let col = &visit.node().data;
            if let Member::Variant(_) = col.member {
                if let Some( parent ) = self.columns.get_parent() {
                    range = Some( parent.data.range.clone() );
                }
            }
            if range.is_none() {
                range = Some( col.range.clone() );
            }
        }
 
        range.map( |range| {
            let start = self.grids[ self.row ].len();
            for _ in start..range.end {
                self.grids[ self.row ].push( String::new() );
            }
        });

        let row = self.row;
        self.grow_row( row );

        self.pop_stack()
    }

    fn end_row( &mut self ) -> Result<()> {
        self.next_column();
        self.pop_stack()
    }

    #[cfg(test)]
    #[inline]
    fn next_column( &mut self ) -> Option<Visit<Column>> { self.columns.next().map( |visit| { eprintln!( "next_column: {:?}", visit ); visit })}

    #[cfg(not(test))]
    #[inline]
    fn next_column( &mut self ) -> Option<Visit<Column>> { self.columns.next() }

    #[cfg(test)]
    #[inline]
    fn print_column( &mut self ) { self.columns.get().map( |visit| eprintln!( "next_column: {:?}", visit )); }

    #[cfg(not(test))]
    #[inline]
    fn print_column( &mut self ) {}
}

fn row_to_string( row: &Vec<String> ) -> String {
    if let Some( grid ) = row.iter().next() {
        let mut output = grid.clone();
        for grid in row.iter().skip(1) {
            output.push_str( &("\t".to_string() + grid) );
        }
        return output;
    } 
    String::new()
}

fn table_to_string( rows: &Vec<Vec<String>> ) -> String {
    if let Some( row ) = rows.iter().next() {
        let mut output = row_to_string( row );
        for row in rows.iter().skip(1) {
            output.push_str( &("\n".to_string() + &row_to_string( row )));
        }
        return output;
    }
    String::new()
}

/// Serialize the given data structure as a String of tsv.
pub fn to_string<T:Reflection+Serialize>( value: &T, config: Config ) -> Result<String> {
    let mut serializer = Serializer::new( T::schemata(), config );
    value.serialize( &mut serializer )?;
    Ok( serializer.header.unwrap_or_default() + &table_to_string( &serializer.grids ))
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();
    type Error = Error;
    type SerializeSeq           = Self;
    type SerializeTuple         = Self;
    type SerializeTupleStruct   = Self;
    type SerializeTupleVariant  = Self;
    type SerializeMap           = Self;
    type SerializeStruct        = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool( self, v: bool) -> Result<()> {
        let s = if v { self.config.true_str.to_string() } else { self.config.false_str.to_string() };
        self.write_in_grid( s )
    }

    fn serialize_i8 ( self, v: i8  ) -> Result<()> { self.serialize_i64( v as i64 )}
    fn serialize_i16( self, v: i16 ) -> Result<()> { self.serialize_i64( v as i64 )}
    fn serialize_i32( self, v: i32 ) -> Result<()> { self.serialize_i64( v as i64 )}
    fn serialize_i64( self, v: i64 ) -> Result<()> { self.write_in_grid( v.to_string() )}

    fn serialize_u8 ( self, v: u8  ) -> Result<()> { self.serialize_u64( v as u64 )}
    fn serialize_u16( self, v: u16 ) -> Result<()> { self.serialize_u64( v as u64 )}
    fn serialize_u32( self, v: u32 ) -> Result<()> { self.serialize_u64( v as u64 )}
    fn serialize_u64( self, v: u64 ) -> Result<()> { self.write_in_grid( v.to_string() )}

    fn serialize_f32( self, v: f32 ) -> Result<()> { self.serialize_f64( v as f64 )}
    fn serialize_f64( self, v: f64 ) -> Result<()> { self.write_in_grid( v.to_string() )}

    fn serialize_char( self, v: char ) -> Result<()> { self.serialize_str( &v.to_string() )}

    fn serialize_str( self, v: &str ) -> Result<()> {
        self.write_in_grid( 
            if v.is_empty() {
                "\\".to_string()
            } else {
                let mut s = String::with_capacity( v.len()*2 );
                for ch in v.chars() {
                    match ch {
                        '\\' => s.push_str( "\\\\" ),
                        '\t' => s.push_str( "\\t" ),
                        '\n' => s.push_str( "\\n" ),
                        _    => s.push( ch ),
                    }
                }
                s
            }
        )
    }

    fn serialize_bytes( self, _v: &[u8] ) -> Result<()> { unimplemented!(); }

    fn serialize_none( self ) -> Result<()> {
        self.serialize_newtype_variant( "", 0, "", &() )
    }

    fn serialize_some<T:?Sized+Serialize>( self, value: &T ) -> Result<()> {
        self.serialize_newtype_variant( "", 1, "", value )
    }

    fn serialize_unit( self ) -> Result<()> {
        let s = self.config.unit_str.clone();
        self.write_in_grid( s )
    }

    fn serialize_unit_struct( self, _name: &'static str ) -> Result<()> { self.serialize_unit() }

    fn serialize_unit_variant( self, _name: &'static str, _variant_index: u32, variant: &'static str ) -> Result<()> { self.serialize_str( variant )}

    fn serialize_newtype_struct<T:?Sized+Serialize>( self, _name: &'static str, value: &T ) -> Result<()> {
        self.next_column();
        value.serialize( self )
    }

    fn serialize_newtype_variant<T:?Sized+Serialize>( self, _name: &'static str, variant_index: u32, _variant: &'static str, value: &T ) -> Result<()> {
        self.columns.to_child( variant_index as usize );
        self.print_column();
        value.serialize( &mut *self )?;
        self.columns.to_parent();
        Ok(())
    }

    fn serialize_seq( self, _len: Option<usize> ) -> Result<Self::SerializeSeq> {
        self.push_stack();
        self.next_column();
        Ok( self )
    }

    fn serialize_tuple( self, len: usize ) -> Result<Self::SerializeTuple> {
        self.serialize_struct( "", len )
    }

    fn serialize_tuple_struct( self, _name: &'static str, len: usize) -> Result<Self::SerializeTupleStruct> {
        self.serialize_struct( "", len )
    }

    fn serialize_tuple_variant( self, _name: &'static str, _variant_index: u32, variant: &'static str, _len: usize ) -> Result<Self::SerializeTupleVariant> {
        variant.serialize( &mut *self )?;
        Ok( self )
    }

    fn serialize_map( self, _len: Option<usize> ) -> Result<Self::SerializeMap> {
        self.push_stack();
        self.print_column();
        Ok( self )
    }

    fn serialize_struct( self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        self.push_stack();
        Ok( self )
    }

    fn serialize_struct_variant( self, _name: &'static str, _variant_index: u32, variant: &'static str, _len: usize ) -> Result<Self::SerializeStructVariant> {
        variant.serialize( &mut *self )?;
        Ok( self )
    }
}

impl<'a> ser::SerializeSeq for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        self.columns.revisit();
        self.write_in_col( value )
    }

    fn end( self ) -> Result<()> {
        self.end_col()
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        self.write_in_row( value )
    }

    fn end( self ) -> Result<()> {
        self.end_row()
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        self.write_in_row( value )
    }

    fn end( self ) -> Result<()> { self.end_row() }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        self.write_in_row( value )
    }

    fn end( self ) -> Result<()> {
        self.end_row()
    }
}

impl<'a> ser::SerializeMap for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T:?Sized+Serialize>( &mut self, key: &T ) -> Result<()> {
        self.row = self.stack.last().unwrap().end;
        self.columns.revisit();
        self.next_column();
        self.do_serialize( key )?;
        self.print_column();
        Ok(())
    }

    fn serialize_value<T:?Sized+Serialize>( &mut self, value: &T ) -> Result<()> {
        self.next_column();
        self.do_serialize( value )?;
        self.columns.to_parent();
        self.print_column();
        Ok(())
    }

    fn end( self ) -> Result<()> {
        self.end_col()
    }
}

impl<'a> ser::SerializeStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T:?Sized+Serialize>(&mut self, _key: &'static str, value: &T) -> Result<()> {
        self.write_in_row( value )
    }

    fn end( self ) -> Result<()> {
        self.end_row()
    }
}

impl<'a> ser::SerializeStructVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T:?Sized+Serialize>(&mut self, _key: &'static str, value: &T) -> Result<()> {
        self.write_in_row( value )
    }

    fn end( self ) -> Result<()> {
        self.end_row()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::fs::read_file;
    use reflection::Reflection;
    use ::std::collections::BTreeMap;

    #[test]
    fn test_unit() {
        let result = to_string( &(), Config::default() ).unwrap();
        let expected = "-";
        assert_eq!( result, expected );
    }

    #[test]
    fn test_unit_struct() {
        #[derive(Serialize,Reflection)]
        struct Test;

        let test = Test;
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = "-";
        assert_eq!( result, expected );
    }

    #[test]
    fn test_struct() {
        #[derive(Serialize,Reflection)]
        struct UnitStruct;

        #[derive(Serialize,Reflection)]
        struct TestStruct { b: bool, u: (), i: i32, s: &'static str, us: UnitStruct, text: String }

        #[derive(Serialize,Reflection)]
        struct Wrapper{ ts : TestStruct }

        let test = Wrapper{ ts: TestStruct{ b:true, u:(), i:42, s:"a\tb\nc", us:UnitStruct, text:String::new() }};
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/struct.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_vec_1d() {
        let test = vec![ "a", "b", "c" ];
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/vec_1d.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_vec_2d() {
        let test = vec![ vec![ "a", "b" ], vec![ "x", "y" ]];
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/vec_2d.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_vec_3d() {
        let test = vec![ vec![ vec![ "a", "b" ], vec![ "c", "d" ]], vec![ vec![ "e", "f" ], vec![ "g", "h" ]]];
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/vec_3d.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_tuple() {
        let test = vec![ ("a", "b" ), ( "x", "y" ), ];
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/tuple.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_misc() {
        #[derive(Serialize,Reflection)]
        struct S{ b: bool, vs: Vec<&'static str>, i: i32, s: &'static str };

        #[derive(Serialize,Reflection)]
        struct Wrapper( Vec<S> );
        let test = Wrapper( vec![ S{ b:true,  vs:vec![ "a", "b", "c" ], i:42, s:"rust" }
                                 ,S{ b:false, vs:vec![ "x", "y", "z" ], i:21, s:"lang" } ]);

        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/misc.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_enum() {
        #[derive(Serialize,Reflection)]
        enum E { I( i32 ), S( String ), }

        let test = vec![ E::I(42), E::S("ab".to_string()), E::I(21), E::S("cd".to_string()) ];
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/enum.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_option() {
        let test = vec![ None, Some("ab".to_string()), None, Some("cd".to_string()) ];
        let result = to_string( &test, Config::default() ).unwrap();
        let expected = "None\tSome\n-\t\n\tab\n-\t\n\tcd\n\t";
        assert_eq!( result, expected );
    }

    #[test]
    fn test_map() {
        use ::std::collections::BTreeMap;
        type Test = BTreeMap<String,u32>;

        let mut test = Test::new();
        test.insert( "abcd".to_string(), 4u32 );
        test.insert( "efg".to_string(), 3u32 );

        let result = to_string( &test, Config::default() ).unwrap();
        let expected = read_file( "test/map.tsv" ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn cargo_tsv() {
        #[derive(Serialize,Reflection)]
        struct Package {
            name    : String,
            version : String,
            authors : Vec<String>,
            keyword : Vec<String>,
        }

        #[derive(Serialize,Reflection)]
        struct Lib {
            #[serde(rename="macro")]
            proc_macro : bool,
        }

        #[derive(Serialize,Reflection)]
        enum PkgSpecifier {
            Version( String ),
            Path( String ),
        }

        #[derive(Serialize,Reflection)]
        struct CargoTsv {
            package : Package,
            lib     : Lib,
            deps    : BTreeMap<String,PkgSpecifier>,
        }

        let mut deps = BTreeMap::new();
        deps.insert( "serde".to_string(), PkgSpecifier::Version( "1.0".to_string() ));
        deps.insert( "trees".to_string(), PkgSpecifier::Path( "~/trees".to_string() ));

        let cargo_tsv = CargoTsv {
            package : Package {
                name    : "tsv".to_string(),
                version : "0.1.0".to_string(),
                authors : vec![ "oooutlk".to_string() ],
                keyword : vec![ "tsv".to_string(), "tab".to_string(), "table".to_string(), "serde".to_string() ],
            },
            lib: Lib{ proc_macro: false },
            deps,
        };
        let result = to_string( &cargo_tsv, Config::default() ).unwrap();
        let expected = read_file( "test/cargo.tsv" ).unwrap();
        assert_eq!( result, expected );
    }
}
