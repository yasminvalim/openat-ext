use openat;
use openat_ext::*;
use std::{error, result};
use tempfile;

type Result<T> = result::Result<T, Box<error::Error>>;

#[test]
fn basic() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    assert!(d.open_file_optional("bar")?.is_none());
    d.write_file("bar", 0o644)?.sync_all()?;
    assert!(d.open_file_optional("bar")?.is_some());
    Ok(())
}
