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
fn dir_tests() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    assert!(d.sub_dir_optional("bar")?.is_none());
    assert!(!d.exists("bar")?);
    d.create_dir("bar", 0o755)?;
    assert!(d.exists("bar")?);
    d.ensure_dir("bar", 0o755)?;
    let bar = d.sub_dir_optional("bar")?.expect("bar");
    assert_eq!(0, bar.list_dir(".")?.count());

    assert!(!d.exists("baz")?);
    d.ensure_dir("baz", 0o755)?;

    Ok(())
}

#[test]
fn exists() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    assert!(!d.exists("l")?);
    d.symlink("l", "enoent")?;
    assert!(d.exists("l")?);

    Ok(())
}
