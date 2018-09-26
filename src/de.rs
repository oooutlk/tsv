//! Deserialize tsv to a Rust data structure.

use table::{Column, generate_columns, column_header};
use trees::{TreeWalk, Visit};
use reflection::Schema;
use error::{Error, ErrorCode, Result};
use std::ops::{AddAssign, MulAssign, Neg,Range};

use serde::de::{
    self, Deserialize, DeserializeSeed, EnumAccess, IntoDeserializer,
    MapAccess, SeqAccess, VariantAccess, Visitor,
};

use super::Env;

#[derive( Copy, Clone, Debug, PartialEq, Eq )]
enum FrameType { Field, Variant }

#[derive( Debug )]
struct Frame {
    range : Range<usize>,
    ty    : FrameType,
}

impl Frame {
    fn new( range: Range<usize>, ty: FrameType ) -> Self { Frame{ range, ty }}
}

/// A structure for deserializing tsv into Rust values.
pub struct Deserializer<'de> {
    columns : TreeWalk<Column>,
    width   : usize,
    grids   : Vec<Vec<&'de str>>,
    stack   : Vec<Frame>,
    row     : usize, // row of cursor
    col     : usize, // column of cursor
    headers : usize, // row count of column headers
    env     : &'de mut Env,
}

fn row_from_str<'de>( input: &'de str, width: usize, line: usize, headers: usize ) -> Result<Vec<&'de str>> {
    let mut row: Vec<&str> = input.split_terminator('\t').collect();
    if row.len() > width {
        Err( Error::new( ErrorCode::ExtraColumns, line+headers+1, width ))
    } else {
        for _ in row.len()..width {
            row.push( "" );
        }
        Ok( row )
    }
}

fn table_from_str<'de>( input: &'de str, width: usize, headers: usize ) -> Result<Vec<Vec<&'de str>>> {
    let rows: Vec<&str> = input.split_terminator('\n').collect();
    let mut table = Vec::new();
    for ( line, row ) in rows.iter().enumerate() {
        match row_from_str( row, width, line, headers ) {
            Ok( row ) => table.push( row ),
            Err( error ) => return Err( error ),
        }
    }
    Ok( table )
}

impl<'de> Deserializer<'de> {
    /// Tries to construct a tsv deserializer.
    ///
    /// # Errors
    ///
    /// The construction can fail with wrong column headers or extra column(s) given.
    pub fn from_str( input: &'de str, schemata: Schema, env: &'de mut Env ) -> Result<Self> {
        let columns = generate_columns( schemata.root() );
        let width = columns.root().data.range.end;
        let mut headers = 0_usize;
        let mut input = input;
        if env.config.with_header {
            let h = column_header( &columns );
            let mut len = 0_usize;
            let ( mut row, mut col ) = ( 0_usize, 0_usize );
            for ( a, b ) in h.chars().zip( input.chars() ) {
                len += 1;
                match a {
                    '\n' => { col = 0; row += 1; },
                    '\t' => { col += 1; }
                    _ => (),
                }
                if a != b { break; }
            }
            if len == h.len() {
                input = &input[len..];
                headers = row;
            } else {
                return Err( Error::new( ErrorCode::HeaderMistach, row+1, col+1 ));
            }
        }
        Ok( Deserializer {
            columns : TreeWalk::from( columns ),
            width   ,
            grids   : table_from_str( input, width, headers )?,
            stack   : vec![ Frame::new( 0..0, FrameType::Field )],
            row     : 0,
            col     : 0,
            headers ,
            env     ,
        })
    }

    fn push_stack( &mut self, ty: FrameType ) {
        self.stack.push( Frame::new( self.row..self.row, ty ));
    }

    fn check_variant( &mut self ) {
        let ty = self.stack.last().unwrap().ty;
        if ty == FrameType::Variant {
            self.to_parent_column();
            self.columns.revisit();
        }
    }

    fn pop_stack( &mut self ) -> Result<()> {
        let frame = self.stack.pop().unwrap();

        if frame.ty == FrameType::Variant {
            self.to_parent_column();
            self.pop_stack()?;
        }

        if frame.range.end > 0 {
            self.grow_row( frame.range.end-1 );
        }

        Ok(())
    }

    #[cfg(test)]
    #[inline]
    fn next_column( &mut self ) { self.columns.next().map( |visit| { eprintln!( "next_column: {:?}", visit ); visit }); }

    #[cfg(not(test))]
    #[inline]
    fn next_column( &mut self ) { self.columns.forward() }

    #[cfg(test)]
    #[inline]
    fn to_parent_column( &mut self ) -> Option<Visit<Column>> { self.columns.to_parent().map( |visit| { eprintln!( "to_parent: {:?}", visit ); visit })}

    #[cfg(not(test))]
    #[inline]
    fn to_parent_column( &mut self ) -> Option<Visit<Column>> { self.columns.to_parent() }

    fn error( &self, code: ErrorCode ) -> Error {
        Error::new( code, self.row + self.headers + 1, self.col+1 )
    }
}

/// Deserialize an instance of type `T` from a string of tsv.
///
/// # Errors
///
/// This conversion can fail for various reason. See `ErrorCode` definition in error.rs for details.
pub fn from_str<'a, T:Deserialize<'a>>( s: &'a str, schemata: Schema, env: &'a mut Env ) -> Result<T> {
    let de = Deserializer::from_str( s, schemata, env );
    match de {
        Ok( mut de ) => T::deserialize( &mut de ),
        Err( error ) => Err( error ),
    }
}

enum ParsedStr<'a> {
    Borrowed( &'a str ),
    Empty( &'a str ),
    Unescaped( &'a str ),
}

impl<'de> Deserializer<'de> {
    #[cfg(test)]
    fn get_str( &self, row: usize, col: usize ) -> &'de str {
        if row < self.grids.len() && col < self.width {
            eprintln!( "read @ ({},{}) : {}", row, col, self.grids[row][col] );
            self.grids[row][col]
        } else {
            ""
        }
    }

    #[cfg(not(test))]
    fn get_str( &self, row: usize, col: usize ) -> &'de str {
        if row < self.grids.len() && col < self.width {
            self.grids[row][col]
        } else {
            ""
        }
    }

    fn peek_str( &self ) -> &'de str { self.get_str( self.row, self.col )}

    fn column_range( &mut self ) -> Option<Range<usize>> {
        if self.stack.last().unwrap().ty == FrameType::Variant {
            if let Some( parent ) = self.columns.get_parent() {
                return Some( parent.data.range.clone() );
            }
        } else {
            if let Some( visit ) = self.columns.get() {
                return Some( visit.node().data.range.clone() );
            }
        }
        None
    }

    fn set_column( &mut self ) {
        if let Some( visit ) = self.columns.get() {
            self.col = visit.node().data.range.clone().start;
        }
    }

    fn grow_row( &mut self, row: usize ) {
        let parent = &mut self.stack.last_mut().unwrap().range;
        if parent.end <= row {
            parent.end = row + 1;
        }
    }

    fn parse_bool( &self ) -> Result<bool> {
        let input = self.peek_str();
        if input.starts_with( &self.env.config.true_str ) {
            Ok( true )
        } else if input.starts_with( &self.env.config.false_str ) {
            Ok( false )
        } else {
            Err( self.error( ErrorCode::ExpectedBoolean ))
        }
    }

    fn parse_unsigned<T>( &self ) -> Result<T>
        where T: AddAssign<T> + MulAssign<T> + From<u8> + ::std::str::FromStr,
    {
        let s = self.peek_str();
        match s.parse::<T>() {
            Ok( result ) => Ok( result ), 
            Err(_) => Err( self.error( ErrorCode::ExpectedUnsigned )),
        }
    }

    fn parse_signed<T>( &self ) -> Result<T>
        where T: Neg<Output = T> + AddAssign<T> + MulAssign<T> + From<i8> + ::std::str::FromStr,
    {
        match self.peek_str().parse::<T>() {
            Ok( result ) => Ok( result ),
            Err(_) => Err( self.error(  ErrorCode::ExpectedInteger )),
        }
    }

    fn parse_float<T>( &self ) -> Result<T>
        where T: AddAssign<T> + MulAssign<T> + From<f32> + ::std::str::FromStr,
    {
        match self.peek_str().parse::<T>() {
            Ok( result ) => Ok( result ), 
            Err(_) => Err( self.error( ErrorCode::ExpectedFloat )),
        }
    }

    fn parse_str( &mut self ) -> ::std::result::Result<ParsedStr<'de>,Error> {
        let s = self.peek_str();

        if s == "\\" {
            Ok( ParsedStr::Empty( unsafe{ &*( &self.env.escapes[0] as &str as *const str )}))
        } else {
            let mut index = 0_usize;
            let mut escaped_char = char::default();
            let mut escaping = false;

            for (i,ch) in s.char_indices() {
                if escaping {
                    match ch {
                        '\\' => { index = i; escaped_char = '\\'; break; },
                        't'  => { index = i; escaped_char = '\t'; break; },
                        'n'  => { index = i; escaped_char = '\n'; break; },
                        _ => { return Err( self.error( ErrorCode::UnsupportedEscape(ch) )); },
                    }
                }
                else if ch == '\\' {
                    escaping = true;
                }
            }

            if escaping {
                let mut escaped_str = String::with_capacity( s.len()-1 );
                escaped_str.push_str( &s[..index-1] );
                escaped_str.push( escaped_char );
                escaping = false;
                for ch in (&s[index+1..]).chars() {
                    if escaping {
                        match ch {
                            '\\' => { escaped_str.push('\\'); escaping = false; },
                            't'  => { escaped_str.push('\t'); escaping = false; },
                            'n'  => { escaped_str.push('\n'); escaping = false; },
                            _ => { return Err( self.error( ErrorCode::UnsupportedEscape(ch) )); },
                        }
                    }
                    else if ch == '\\' {
                        escaping = true;
                    } else {
                        escaped_str.push( ch );
                    }
                }
                self.env.escapes.push( escaped_str );
                Ok( ParsedStr::Unescaped( unsafe{ &*( self.env.escapes.last().unwrap() as &str as *const str )}))
            }
            else {
                Ok( ParsedStr::Borrowed( s ))
            }
        }
    }
}

impl<'de,'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any     <V:Visitor<'de>>( self, _visitor:V ) -> Result<V::Value> { unimplemented!() }
    fn deserialize_bool    <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_bool( self.parse_bool()    ? )}
    fn deserialize_i8      <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_i8(   self.parse_signed()  ? )}
    fn deserialize_i16     <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_i16(  self.parse_signed()  ? )}
    fn deserialize_i32     <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_i32(  self.parse_signed()  ? )}
    fn deserialize_i64     <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_i64(  self.parse_signed()  ? )}
    fn deserialize_u8      <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_u8(   self.parse_unsigned()? )}
    fn deserialize_u16     <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_u16(  self.parse_unsigned()? )}
    fn deserialize_u32     <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_u32(  self.parse_unsigned()? )}
    fn deserialize_u64     <V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> { visitor.visit_u64(  self.parse_unsigned()? )}
    fn deserialize_f32     <V:Visitor<'de>>( self, visitor:V  ) -> Result<V::Value> { visitor.visit_f32(  self.parse_float()   ? )}
    fn deserialize_f64     <V:Visitor<'de>>( self, visitor:V  ) -> Result<V::Value> { visitor.visit_f64(  self.parse_float()   ? )}
    fn deserialize_bytes   <V:Visitor<'de>>( self, _visitor:V ) -> Result<V::Value> { unimplemented!() }
    fn deserialize_byte_buf<V:Visitor<'de>>( self, _visitor:V ) -> Result<V::Value> { unimplemented!() }

    fn deserialize_char<V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> {
        let s = self.peek_str();
        match s {
            "\\"   => visitor.visit_char( char::default() ),
            "\\\\" => visitor.visit_char( '\\' ),
            "\\t"  => visitor.visit_char( '\t' ),
            "\\n"  => visitor.visit_char( '\n' ),
            _      => {
                if s.len() == 1 {
                    visitor.visit_char( s.chars().next().unwrap() )
                } else {
                    Err( self.error( ErrorCode::ExpectedChar ))
                }
            },
        }
    }

    fn deserialize_str<V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> {
        match self.parse_str() {
            Ok( parsed ) => match parsed {
                ParsedStr::Borrowed( s) => visitor.visit_borrowed_str(s),
                ParsedStr::Empty(    s) => visitor.visit_borrowed_str(s),
                ParsedStr::Unescaped(s) => visitor.visit_borrowed_str(s),
            }
            Err( error ) => Err( error ),
        }
    }

    fn deserialize_string<V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> {
        match self.parse_str() {
            Ok( parsed ) => match parsed {
                ParsedStr::Borrowed( s) => visitor.visit_borrowed_str( s ),
                ParsedStr::Empty(    _) => visitor.visit_string( String::new() ),
                ParsedStr::Unescaped(_) => visitor.visit_string( self.env.escapes.pop().unwrap() ),
            }
            Err( error ) => Err( error ),
        }
    }

    fn deserialize_option<V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> {
        if self.peek_str().is_empty() {
            self.col += 1;
            visitor.visit_some( self )
        } else {
            visitor.visit_none()
        }
    }

    fn deserialize_unit<V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> {
        if self.peek_str() == self.env.config.unit_str {
            visitor.visit_unit()
        } else {
            Err( self.error(  ErrorCode::ExpectedUnit ))
        }
    }

    fn deserialize_unit_struct<V:Visitor<'de>>( self, _name: &'static str, visitor: V ) -> Result<V::Value> {
        self.deserialize_unit( visitor )
    }

    fn deserialize_newtype_struct<V:Visitor<'de>>( self, _name: &'static str, visitor: V ) -> Result<V::Value> {
        self.next_column();
        visitor.visit_newtype_struct( self )
    }

    fn deserialize_seq<V:Visitor<'de>>( mut self, visitor: V ) -> Result<V::Value> {
        self.next_column();
        self.push_stack( FrameType::Field );
        let result = visitor.visit_seq( ElementsAccess::new( &mut self ));
        self.pop_stack()?;
        result
    }

    fn deserialize_tuple<V:Visitor<'de>>( mut self, len: usize, visitor: V ) -> Result<V::Value> {
        self.push_stack( FrameType::Field );
        let result = visitor.visit_seq( FieldsAccess::new( &mut self, len ));
        self.pop_stack()?;
        result
    }

    fn deserialize_tuple_struct<V:Visitor<'de>>( self, _name: &'static str, len: usize, visitor: V ) -> Result<V::Value> {
        self.deserialize_tuple( len, visitor )
    }

    fn deserialize_map<V:Visitor<'de>>( mut self, visitor: V ) -> Result<V::Value> {
        self.push_stack( FrameType::Field );
        let value = visitor.visit_map( ElementsAccess::new( &mut self ))?;
        self.pop_stack()?;
        Ok( value )
    }

    fn deserialize_struct<V:Visitor<'de>>( self, _name: &'static str, fields: &'static [&'static str], visitor: V ) -> Result<V::Value> {
        self.deserialize_tuple( fields.len(), visitor )
    }

    fn deserialize_enum<V:Visitor<'de>>( self, _name: &'static str, _variants: &'static [&'static str], visitor: V ) -> Result<V::Value> {
        visitor.visit_enum( Enum::new( self ))
    }

    fn deserialize_identifier<V:Visitor<'de>>( self, visitor: V ) -> Result<V::Value> {
        self.deserialize_str( visitor )
    }

    fn deserialize_ignored_any<V:Visitor<'de>>( self, _visitor: V ) -> Result<V::Value> {
        unimplemented!();
    }
}

struct FieldsAccess<'a, 'de:'a> {
    de  : &'a mut Deserializer<'de>,
    len : usize,
}

impl<'a, 'de:'a> FieldsAccess<'a,'de> {
    fn new( de: &'a mut Deserializer<'de>, len: usize ) -> Self {
        FieldsAccess { de, len }
    }
}

impl<'a,'de:'a> SeqAccess<'de> for FieldsAccess<'a,'de> {
    type Error = Error;

    fn next_element_seed<T:DeserializeSeed<'de>>( &mut self, seed: T ) -> Result<Option<T::Value>> {
        self.de.next_column();
        self.de.set_column();
        self.de.row = self.de.stack.last().unwrap().range.start;

        let result = DeserializeSeed::deserialize( seed, &mut *self.de )?;

        self.len -= 1;
        if self.len == 0 {
            self.de.next_column();
        }
        let row = self.de.row;
        self.de.grow_row( row );
        Ok( Some( result ))
    }

    fn size_hint( &self ) -> Option<usize> { Some( self.len )}
}

struct ElementsAccess<'a, 'de:'a> {
    de  : &'a mut Deserializer<'de>,
}

impl<'a,'de:'a> ElementsAccess<'a,'de> {
    fn new( de: &'a mut Deserializer<'de> ) -> Self { ElementsAccess { de }}

    fn next_element_in_row<T:DeserializeSeed<'de>>( &mut self, seed: T ) -> Result<T::Value> {
        self.de.next_column();
        self.de.row = self.de.stack.last().unwrap().range.start;
        self.de.set_column();

        let result = DeserializeSeed::deserialize( seed, &mut *self.de )?;

        self.de.columns.next();
        let row = self.de.row;
        self.de.grow_row( row );

        Ok( result )
    }

    fn next_element_in_col<T:DeserializeSeed<'de>>( &mut self, seed: T ) -> Result<Option<T::Value>> {
        self.de.columns.revisit();

        self.de.check_variant();
        self.de.row = self.de.stack.last().unwrap().range.end;
        self.de.set_column();

        let range = self.de.column_range().unwrap();
        let ( mut col, end ) = ( range.start, range.end );
        while col < end {
            if !self.de.get_str( self.de.row, col ).is_empty() {
                break;
            }
            col += 1;
        }
        let result = if col == end {
            self.de.next_column();
            Ok( None )
        } else {
            Ok( seed.deserialize( &mut *self.de ).map( Some )? )
        };

        let row = self.de.row;
        self.de.grow_row( row );
        result
    }
}

impl<'a,'de:'a> SeqAccess<'de> for ElementsAccess<'a,'de> {
    type Error = Error;

    fn next_element_seed<T:DeserializeSeed<'de>>( &mut self, seed: T ) -> Result<Option<T::Value>> {
        self.next_element_in_col( seed )
    }
}

impl<'a,'de:'a> MapAccess<'de> for ElementsAccess<'a,'de> {
    type Error = Error;

    fn next_key_seed<K:DeserializeSeed<'de>>( &mut self, seed: K ) -> Result<Option<K::Value>> {
        self.de.columns.next();
        self.de.push_stack( FrameType::Field ); // start key-value pair
        self.next_element_in_col( seed )
    }

    #[allow( unused_must_use )]
    fn next_value_seed<V:DeserializeSeed<'de>>( &mut self, seed: V ) -> Result<V::Value> {
        let result = self.next_element_in_row( seed )?;
        self.de.next_column();
        self.de.row += 1;
        self.de.pop_stack(); // finish key-value pair
        self.de.next_column();
        self.de.columns.revisit();
        Ok( result )
    }
}

struct Enum<'a, 'de: 'a> {
    de            : &'a mut Deserializer<'de>,
    variant_index : usize,
}

impl<'a,'de:'a> Enum<'a,'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        Enum{ de, variant_index:0 }
    }
}

impl<'a,'de:'a> EnumAccess<'de> for Enum<'a,'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V:DeserializeSeed<'de>>( mut self, seed: V ) -> Result<( V::Value, Self::Variant )> {
        let range = self.de.column_range().unwrap();
        let mut col = range.start;

        while col < range.end && self.de.grids[ self.de.row ][ col ] == "" {
            col += 1;
        }

        if col == range.end {
            return Err( self.de.error( ErrorCode::ExpectedVariant ));
        }

        self.de.next_column();
        let mut index: usize = 0;
        loop {
            if let Some( visit ) = self.de.columns.get() {
                let range = visit.node().data.range.clone();
                if col >= range.start && col < range.end {
                    break;
                }
            }
            if self.de.columns.to_sib(1).is_none() {
                panic!();
            }
            index += 1;
        }

        self.variant_index = index;
        self.de.col += index;

        self.de.push_stack( FrameType::Variant );
        Ok(( seed.deserialize( index.into_deserializer() )?, self ))
    }
}

impl<'de,'a> VariantAccess<'de> for Enum<'a,'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T:DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value> {
        seed.deserialize( self.de )
    }

    fn tuple_variant<V:Visitor<'de>>(self, len: usize, visitor: V) -> Result<V::Value> {
        de::Deserializer::deserialize_tuple( self.de, len, visitor )
    }

    fn struct_variant<V:Visitor<'de>>( self, fields: &'static [&'static str], visitor: V ) -> Result<V::Value> {
        de::Deserializer::deserialize_tuple( self.de, fields.len(), visitor )
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
        let input = "-";
        let expected = ();
        let mut env = Env::default();
        let result: () = from_str( input, <()>::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_unit_struct() {
        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct Test;

        let input = "-";
        let expected = Test;
        let mut env = Env::default();
        let result: Test = from_str( input, Test::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_struct() {
        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct UnitStruct;

        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct TestStruct<'a> { b: bool, u: (), i: i32, s: &'a str, us: UnitStruct, text: String }

        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct Wrapper<'a>{ #[serde(borrow)] ts: TestStruct<'a> };

        let input = &read_file( "test/struct.tsv" ).unwrap();
        let expected = Wrapper{ ts: TestStruct{ b:true, u:(), i:42, s:"a\tb\nc", us:UnitStruct, text:String::new() }};
        let mut env = Env::default();
        let result: Wrapper = from_str( input, Wrapper::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_vec_1d() {
        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct TestVec<'a>( #[serde(borrow)] Vec<&'a str> );

        let input = &read_file( "test/vec_1d.tsv" ).unwrap();
        let expected = TestVec( vec![ "a", "b", "c" ] );
        let mut env = Env::default();
        let result: TestVec = from_str( input, TestVec::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_vec_2d() {
        let input = &read_file( "test/vec_2d.tsv" ).unwrap();
        let expected = vec![ vec![ "a", "b" ], vec![ "x", "y" ]];
        type Test<'a> = Vec<Vec<&'a str>>;
        let mut env = Env::default();
        let result: Test = from_str( input, Test::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_vec_3d() {
        let input = &read_file( "test/vec_3d.tsv" ).unwrap();
        let expected = vec![ vec![ vec![ "a", "b" ], vec![ "c", "d" ]], vec![ vec![ "e", "f" ], vec![ "g", "h" ]]];
        type Test<'a> = Vec<Vec<Vec<&'a str>>>;
        let mut env = Env::default();
        let result: Test = from_str( input, Test::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_tuple() {
        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct Test<'a>( #[serde(borrow)] Vec<(&'a str,&'a str)> );

        let input = &read_file( "test/tuple.tsv" ).unwrap();
        let expected = Test( vec![ ("a", "b" ), ( "x", "y" ) ]);
        let mut env = Env::default();
        let result: Test = from_str( input, Test::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_misc() {
        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct S<'a>{
            b  : bool,
            vs : Vec<&'a str>,
            i  : i32,
            s  : &'a str
        };

        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct Test<'a>( #[serde(borrow)] Vec<S<'a>> );

        let input = &read_file( "test/misc.tsv" ).unwrap();
        let expected = Test( vec![ S{ b:true,  vs:vec![ "a", "b", "c" ], i:42, s:"rust" }
                                  ,S{ b:false, vs:vec![ "x", "y", "z" ], i:21, s:"lang"  } ]);
        let mut env = Env::default();
        let result: Test = from_str( input, Test::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_enum() {
        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        enum E { I( i32 ), S( String ), }

        let input = &read_file( "test/enum.tsv" ).unwrap();
        let expected = vec![ E::I(42), E::S("ab".to_string()), E::I(21), E::S("cd".to_string()) ];
        let mut env = Env::default();
        let result: Vec<E> = from_str( input, Vec::<E>::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_enum2() {
        #[derive(Deserialize, Reflection, PartialEq, Debug)]
        enum E {
            Unit,
            Newtype( u32 ),
            Tuple( u32, u32 ),
            Struct{ a: u32 },
        }

        let j = "\t\t\t\tStruct\nUnit\tNewtype\tTuple\t\ta\n-\t\t\t\t\t";
        let expected = E::Unit;
        let mut env = Env::default();
        assert_eq!( expected, from_str( j, E::schemata(), &mut env ).unwrap() );

        let j = "\t\t\t\tStruct\nUnit\tNewtype\tTuple\t\ta\n\t1\t\t\t\t";
        let expected = E::Newtype(1);
        let mut env = Env::default();
        assert_eq!( expected, from_str( j, E::schemata(), &mut env ).unwrap() );

        let j = "\t\t\t\tStruct\nUnit\tNewtype\tTuple\t\ta\n\t\t1\t2\t\t";
        let expected = E::Tuple(1, 2);
        let mut env = Env::default();
        assert_eq!( expected, from_str( j, E::schemata() , &mut env ).unwrap() );

        let j = "\t\t\t\tStruct\nUnit\tNewtype\tTuple\t\ta\n\t\t\t\t1\t";
        let expected = E::Struct { a: 1 };
        let mut env = Env::default();
        assert_eq!( expected, from_str( j, E::schemata(), &mut env ).unwrap() );
    }

    #[test]
    fn test_option() {
        let input = "None\tSome\n-\t\n\tab\n-\t\n\tcd\n";
        let expected = vec![ None, Some("ab".to_string()), None, Some("cd".to_string()) ];

        type Test = Vec<Option<String>>;

        let mut env = Env::default();
        let result: Test = from_str( input, Test::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn test_map() {
        type Test = BTreeMap<String,u32>;

        let mut expected = Test::new();
        expected.insert( "abcd".to_string(), 4_u32 );
        expected.insert( "efg".to_string(), 3_u32 );

        let input = read_file( "test/map.tsv" ).unwrap();
        let mut env = Env::default();
        let result: Test = from_str( &input, Test::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }

    #[test]
    fn cargo_tsv() {
        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct Package {
            name    : String,
            version : String,
            authors : Vec<String>,
            keyword : Vec<String>,
        }

        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct Lib {
            #[serde(rename="macro")]
            proc_macro : bool,
        }

        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        enum PkgSpecifier {
            Version( String ),
            Path( String ),
        }

        #[derive(Deserialize,Reflection,Debug,PartialEq)]
        struct CargoTsv {
            package : Package,
            lib     : Lib,
            deps    : BTreeMap<String,PkgSpecifier>,
        }

        let mut deps = BTreeMap::new();
        deps.insert( "serde".to_string(), PkgSpecifier::Version( "1.0".to_string() ));
        deps.insert( "trees".to_string(), PkgSpecifier::Path( "~/trees".to_string() ));

        let expected = CargoTsv {
            package: Package {
                name    : "tsv".to_string(),
                version : "0.1.0".to_string(),
                authors : vec![ "oooutlk".to_string() ],
                keyword : vec![ "tsv".to_string(), "tab".to_string(), "table".to_string(), "serde".to_string() ],
            },
            lib: Lib{ proc_macro: false },
            deps,
        };
        let input = read_file( "test/cargo.tsv" ).unwrap();
        let mut env = Env::default();
        let result: CargoTsv = from_str( &input, CargoTsv::schemata(), &mut env ).unwrap();
        assert_eq!( result, expected );
    }
}
