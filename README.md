This library defines a uniform interface for reading the contents of
compressed archives (zip files and tarballs).

It currently supports zip files and tarballs.  Tarballs can be
uncompressed or compressed with gzip or bzip2.

See the Codec.Archive module haddocks for API information and
usage examples.

# TODO

 * Add LZMA support
 * Switch to conduit-based IO (need a conduit-bzip2 package first)
