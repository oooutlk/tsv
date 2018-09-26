//! # tsv (tab-seperated-values) library for serde (serialization/deserialization).
//!
//! The tsv format is right out and uses human-readable text to interchange data of arbitrary schema.
//!
//! # Example (Map)
//!
//!   ```rust
//!   extern crate tsv;
//!
//!   extern crate reflection;
//!   use reflection::Reflection;
//!    
//!   use std::collections::HashMap;
//!
//!   type Map = HashMap<String,u32>;
//!                                                                                        
//!   let mut map = Map::new();
//!   map.insert( "abcd".to_string(), 42_u32 );
//!   map.insert( "efg".to_string(), 23_u32 );
//!                                                                                        
//!   let str_repr = tsv::ser::to_string( &map, tsv::Config::default() ).unwrap();
//!   let mut env = tsv::Env::default();
//!   let map_from_str: Map = tsv::de::from_str( &str_repr, Map::schemata(), &mut env ).unwrap();
//!   assert_eq!( map, map_from_str );
//!   ```
//!
//!   The `str_repr`( stands for "string representation" ) looks like something as follows( with spaces replacing tabs ):
//!
//!   ```text
//!   name    value
//!   abcd    42
//!   def     23
//!
//!   ```
//!
//! # Example (Cargo.tsv)
//!
//!  A cargo configuration file written in tsv format could look like the following table( with spaces replacing tabs ):
//!
//!  ```text
//!                                          deps
//!  package                         lib             value    
//!  name    version authors keyword macro   name    Version Path
//!  tsv     0.1.0   oooutlk tsv     X       serde   1.0
//!                          tab             trees           ~/trees
//!                          table
//!                          serde
//!
//!  ```
//!
//!  See [serialization example](https://github.com/oooutlk/tsv/blob/master/src/ser.rs#L546) 
//!  and [deserialization example](https://github.com/oooutlk/tsv/blob/master/src/de.rs#L804).

extern crate trees;

extern crate reflection;

extern crate serde;

pub mod ser;
pub use self::ser::{Serializer,to_string};

pub mod de;
pub use self::de::{Deserializer,from_str};

pub mod error;
pub use error::{Error,Result};
pub(crate) use error::ErrorCode;

mod table;

#[cfg(test)]
#[macro_use]
extern crate reflection_derive;

#[cfg(test)]
#[macro_use]
extern crate serde_derive;

const RESERVED_CHAR: [char;3] = [ '\\', '\t', '\n' ];

/// A structure for configuring tsv serializer.
pub struct Config {
    with_header : bool,
    unit_str    : String,
    true_str    : String,
    false_str   : String,
}

impl Config {
    /// Tries to creates a new `Config` structure. Result is Ok if the config does not violate the spec, otherwise Err.
    pub fn make_config( 
        with_header : bool,
        unit_str    : String,
        true_str    : String,
        false_str   : String
    ) -> Result<Self>
    {
        if true_str == false_str || unit_str.is_empty() || true_str.is_empty() || false_str.is_empty() {
            return Err( Error::new( ErrorCode::InvalidConfig, 0, 0 ));
        } else {
            for i in 0..3 {
                if unit_str.find(  RESERVED_CHAR[i] ).is_some()
                || true_str.find(  RESERVED_CHAR[i] ).is_some()
                || false_str.find( RESERVED_CHAR[i] ).is_some()
                {
                    return Err( Error::new( ErrorCode::InvalidConfig, 0, 0 ));
                }
            }
        }
        Ok( Config{ with_header, unit_str, true_str, false_str })
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {
            with_header : true,
            unit_str    : "-".to_string(),
            true_str    : "O".to_string(),
            false_str   : "X".to_string(),
        }
    }
}

/// A structure for configuring tsv deserializer and holding escaped `String`s being referenced  as `&str` by deserialized value, if any.
pub struct Env {
    config  : Config,
    escapes : Vec<String>,
}

impl Env {
    /// Tries to creates a new `Env` structure. Result is Ok if the config does not violate the spec, otherwise Err.
    pub fn make_env( 
        with_header : bool,
        unit_str    : String,
        true_str    : String,
        false_str   : String
    ) -> Result<Self>
    {
        let config = Config::make_config( with_header, unit_str, true_str, false_str )?;
        let escapes = vec![ String::new() ];
        Ok( Env{ config, escapes })
    }
}

impl Default for Env {
    fn default() -> Self {
        Env{ config: Config::default(), escapes: vec![ String::new() ]}
    }
}

/// File operations.
pub mod fs {
    use std::fs::File;
    use std::io::Read;

    /// read file contents to `String`, trailing newline chomped.
    pub fn read_file( name: &'static str ) -> ::std::io::Result<String> {
        let mut file = File::open( name )?;
        let mut contents = String::new();
        file.read_to_string( &mut contents )?;
        contents.pop(); // remove trailing newline
        Ok( contents )
    }
}
