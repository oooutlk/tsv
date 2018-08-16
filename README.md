The tsv project introduces a new format for data serialization/deserialization, which is text-based and deals with tabular data.

# The problem

At [serde](https://serde.rs)'s point of view, the classic [tsv](https://www.iana.org/assignments/media-types/text/tab-separated-values) is only applicable to the schema of (a sequence of) a `struct` composed of primitives( integer, floats, strings etc). The specification has to be extended to allow arbitrary schemas, such as a `struct` of a `struct`.

# The solution

This project extends the spec by placing sequences in columns. See tsv-spec.txt for specification.
It uses serde crate for serialization/deserialization, and reflection crate for generating column names and dealing with `enum`s.

## Notice

If you impl Serialize/Deserialize for your types to tell serde they are sequences/maps, do make sure their `schemata()` and `Vec::schemata()`/`HashMap::schemata()` are isomorphic. 

# Pros

1. Simple.
  The only requirement for end users to use tsv files is to understand what a table is. It is deadly simple as a configuration file format for non-technical users.

2. Available.
  You can use Microsoft Excel, OpenOffice/LibreOffie Calc and text editors that support [elastic tabstops](http://nickgravgaard.com/elastic-tabstops/) to view/edit tsv files.
  And it is easy to write tsv by hand if you have read all the 63 lines of the spec.

# Cons

1. Not efficiency-oriented.

2. Not self-descripting.

# License

Under MIT.

# Example

  A cargo configuration file written in tsv format could look like the following table( with spaces replacing tabs ):

                                          deps
  package                         lib             value    
  name    version authors keyword macro   name    Version Path
  tsv     0.1.0   oooutlk tsv     X       serde   1.0
                          tab             trees           ~/trees
                          table
                          serde

See [serialization example](https://github.com/oooutlk/tsv/blob/master/tsv/src/ser.rs#L548) 
and [deserialization example](https://github.com/oooutlk/tsv/blob/master/tsv/src/de.rs#L802).
