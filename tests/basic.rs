use openat;
use openat_ext::*;
use std::{error, result};
use tempfile;

type Result<T> = result::Result<T, Box<error::Error>>;

#[test]
fn open_file_optional() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    assert!(d.open_file_optional("bar")?.is_none());
    d.write_file("bar", 0o644)?.sync_all()?;
    assert!(d.open_file_optional("bar")?.is_some());
    Ok(())
}

#[test]
fn subdir_optional() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    assert!(d.sub_dir_optional("bar")?.is_none());
    d.create_dir("bar", 0o755)?;
    let bar = d.sub_dir_optional("bar")?.expect("bar");
    assert_eq!(0, bar.list_dir(".")?.count());
    Ok(())
}
