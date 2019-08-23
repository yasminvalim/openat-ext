//! # Extension methods for openat::Dir
//!
//! ```
//! use openat_ext::*;
//! ```
//!
//! The `openat` crate is a low-level API, generally just exposing
//! thin wrappers for the underlying system call.  This crate offers
//! a number of common higher level convenience functions.

use openat;
use std::{fs, io};

/// Helper functions for openat::Dir
pub trait OpenatDirExt {
    /// Checking for nonexistent files (`ENOENT`) is by far the most common case of inspecting error
    /// codes in Unix.  Rust has a nice `Option<>` type, so this helper makes use of it and returns `Ok(None)`
    /// for nonexistent files.  All other errors are `Err`, and extant files are `Ok(Some<file>))`
    /// of course.
    fn open_file_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<fs::File>>;

    /// Like `open_file_optional()` except opens a directory via `openat::dir::sub_dir`.
    fn sub_dir_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<openat::Dir>>;

    /// On modern filesystems the directory entry contains the type; if available,
    /// return it.  Otherwise invoke `stat()`.
    fn get_file_type(&self, e: &openat::Entry) -> io::Result<openat::SimpleType>;
}

impl OpenatDirExt for openat::Dir {
    fn open_file_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<fs::File>> {
        match self.open_file(p) {
            Ok(f) => Ok(Some(f)),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(e)
                }
            }
        }
    }

    fn sub_dir_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<openat::Dir>> {
        match self.sub_dir(p) {
            Ok(d) => Ok(Some(d)),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(e)
                }
            }
        }
    }

    fn get_file_type(&self, e: &openat::Entry) -> io::Result<openat::SimpleType> {
        if let Some(ftype) = e.simple_type() {
            Ok(ftype)
        } else {
            Ok(self.metadata(e.file_name())?.simple_type())
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::{error, result};
    use tempfile;

    type Result<T> = result::Result<T, Box<error::Error>>;

    #[test]
    fn open_file_optional() -> Result<()> {
        let td = tempfile::tempdir()?;
        let d = openat::Dir::open(td.path())?;
        assert!(d.open_file_optional("foo")?.is_none());
        d.write_file("foo", 0o644)?.sync_all()?;
        assert!(d.open_file_optional("foo")?.is_some());
        Ok(())
    }

    #[test]
    fn get_file_type() -> Result<()> {
        let td = tempfile::tempdir()?;
        let d = openat::Dir::open(td.path())?;
        d.write_file("foo", 0o644)?.sync_all()?;
        for x in d.list_dir(".")? {
            let x = x?;
            assert_eq!("foo", x.file_name());
            let t = d.get_file_type(&x)?;
            assert_eq!(openat::SimpleType::File, t);
        }
        Ok(())
    }
}