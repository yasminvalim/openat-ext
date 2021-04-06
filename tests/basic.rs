use anyhow::Context;
use openat;
use openat_ext::*;
use std::fs::File;
use std::io::prelude::*;
use std::{error, result};
use tempfile;

type Result<T> = result::Result<T, Box<dyn error::Error>>;

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
    assert!(!d.remove_dir_optional("baz")?);
    d.ensure_dir("baz", 0o755)?;
    assert!(d.remove_dir_optional("baz")?);
    assert!(!d.exists("baz")?);

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

#[test]
fn copy() -> Result<()> {
    let td = tempfile::tempdir()?;
    let src_p = td.path().join("testfile");
    let dest_p = td.path().join("testfiledest");
    let contents = "somefilecontents";
    std::fs::write(&src_p, contents)?;
    let src = File::open(&src_p)?;
    {
        let dest = File::create(&dest_p)?;
        src.copy_to(&dest)?;
    }
    let testf_contents = std::fs::read_to_string(&dest_p)?;
    assert_eq!(contents, testf_contents.as_str());
    Ok(())
}

#[test]
fn write_file_with() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    let testname = "testfile";
    let testcontents = "hello world";
    d.write_file_with("testfile", 0o644, |w| -> std::io::Result<()> {
        w.write_all(testcontents.as_bytes())
    })?;
    let actual_contents = std::fs::read_to_string(td.path().join(testname))?;
    assert_eq!(testcontents, actual_contents.as_str());
    let testcontents2 = "updated world";
    d.write_file_with("testfile", 0o644, |w| -> anyhow::Result<()> {
        w.write_all(testcontents2.as_bytes())?;
        Ok(())
    })?;
    let actual_contents = std::fs::read_to_string(td.path().join(testname))?;
    assert_eq!(testcontents2, actual_contents.as_str());
    let srcf = d.open_file("testfile")?;
    d.write_file_with("testfile2", 0o644, |w| -> anyhow::Result<()> {
        let mut bufr = std::io::BufReader::new(srcf);
        std::io::copy(&mut bufr, w)?;
        Ok(())
    })?;
    let actual_contents = std::fs::read_to_string(td.path().join("testfile2"))?;
    assert_eq!(testcontents2, actual_contents.as_str());
    Ok(())
}

#[test]
fn write_file_with_complex() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    let testname = "testfile";
    let testcontents = "hello world";
    let mut written = false;
    d.write_file_with_sync("testfile", 0o644, |w| -> std::io::Result<()> {
        written = true;
        w.write_all(testcontents.as_bytes())
    })?;
    let actual_contents = std::fs::read_to_string(td.path().join(testname))?;
    assert_eq!(testcontents, actual_contents.as_str());
    assert_eq!(written, true);
    Ok(())
}

#[test]
fn write_file_contents() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    let testname = "testfile";
    let testcontents = "hello world";
    d.write_file_contents("testfile", 0o644, testcontents)?;
    let actual_contents = std::fs::read_to_string(td.path().join(testname))?;
    assert_eq!(testcontents, actual_contents.as_str());
    Ok(())
}

#[test]
fn file_writer() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    let testname = "testfile";
    let testcontents = "hello world";
    let mut fw = d.new_file_writer(0o644)?;
    fw.writer.write_all(testcontents.as_bytes())?;
    fw.complete(testname)?;
    let actual_contents = std::fs::read_to_string(td.path().join(testname))?;
    assert_eq!(testcontents, actual_contents.as_str());
    Ok(())
}

#[test]
fn file_writer_abandon() -> Result<()> {
    let td = tempfile::tempdir()?;
    let d = openat::Dir::open(td.path())?;
    let testname = "testfile";
    let testcontents = "hello world";
    {
        let mut fw = d.new_file_writer(0o644)?;
        fw.writer.write_all(testcontents.as_bytes())?;
    }
    assert!(d.open_file_optional(testname)?.is_none());
    Ok(())
}

#[test]
fn rmrf() -> anyhow::Result<()> {
    use std::fs::create_dir_all;
    use std::fs::write as fswrite;
    use std::os::unix::fs::symlink;

    let tempdir = tempfile::tempdir()?;
    let d = openat::Dir::open(tempdir.path())?;

    let td = tempdir.path().join("t");
    for d in &["foo/bar/baz", "foo/bar/blah", "blah/moo", "somedir"] {
        let p = td.join(d);
        create_dir_all(&p)?;
        symlink("/", p.join("somelink"))?;
        symlink("somelink", p.join("otherlink"))?;
        symlink(".", p.join("link2self"))?;
        let linkeddir = p.join("linkdirtarget");
        create_dir_all(&linkeddir)?;
        symlink(&linkeddir, p.join("link2dir"))?;
    }
    for f in &["somefile", "otherfile"] {
        fswrite(td.join("foo/bar").join(f), f)?;
        fswrite(td.join("blah").join(f), f)?;
        fswrite(td.join("blah/moo").join(f), f)?;
    }
    assert!(d.remove_all("t").context("removing t")?);
    assert!(!d.exists("t")?);

    assert!(!d.remove_all("nosuchfile").context("removing nosuchfile")?);
    let l = tempdir.path().join("somelink");
    let regf = tempdir.path().join("regfile");
    fswrite(&regf, "some file contents")?;
    symlink("regfile", &l)?;
    assert!(d.remove_all("somelink")?);
    assert!(!d.remove_all("somelink")?);
    assert!(d.exists("regfile")?);
    assert!(d.remove_all("regfile")?);
    assert!(!d.exists("regfile")?);

    Ok(())
}

fn assert_file_contents<P: AsRef<std::path::Path>>(
    d: &openat::Dir,
    p: P,
    contents: &str,
) -> Result<()> {
    let mut buf = String::new();
    let _ = d.open_file(p.as_ref())?.read_to_string(&mut buf)?;
    assert_eq!(buf, contents);
    Ok(())
}

#[test]
fn copy_path() -> Result<()> {
    let td = tempfile::tempdir()?;
    let td = &openat::Dir::open(td.path())?;

    // Copy plain src to dest in same dir
    td.write_file_contents("srcf", 0o644, "src contents")?;
    td.copy_file("srcf", "destf").context("copy1")?;
    assert_file_contents(td, "destf", "src contents")?;

    // Overwrite
    td.write_file_contents("srcf", 0o644, "src contents v2")?;
    td.copy_file("srcf", "destf").context("copy overwrite")?;
    assert_file_contents(td, "destf", "src contents v2")?;

    // Copy across subdirectories
    td.ensure_dir_all("sub1.1/sub2", 0o755)?;
    td.ensure_dir_all("sub1.2/sub2", 0o755)?;
    td.write_file_contents("sub1.1/sub2/blah", 0o600, "somesecretvalue")?;
    let subtarget = "sub1.2/sub2/blahcopy";
    td.copy_file("sub1.1/sub2/blah", subtarget)
        .context("copy2")?;
    assert_file_contents(td, subtarget, "somesecretvalue")?;
    assert_eq!(td.metadata(subtarget)?.stat().st_mode & 0o777, 0o600);

    // We don't follow links by default
    td.symlink("somelink", "srcf")?;
    assert!(td.copy_file("somelink", "srcf-from-link").is_err());
    // Nonexistent file
    assert!(td.copy_file("enoent", "nosuchdest").is_err());
    // Directory
    assert!(td.copy_file("sub1.1", "nosuchdest").is_err());

    Ok(())
}
