//! # Extension methods for openat::Dir and std::fs::File
//!
//! ```
//! use openat_ext::OpenatDirExt;
//! ```
//!
//! The `openat` crate is a low-level API, generally just exposing
//! thin wrappers for the underlying system call.  This crate offers
//! a number of common higher level convenience functions.
//!
//! More recently, there is also an `FileExt` available; it currently
//! just contains an optimized file copy method that will hopefully
//! go into the standard library.

#![deny(unused_results)]
#![deny(missing_docs)]
#![deny(unsafe_code)]

const TEMPFILE_ATTEMPTS: u32 = 100;

use libc;
use nix;
use openat;
use rand::Rng;
use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::PermissionsExt;
use std::os::unix::io::AsRawFd;
use std::os::unix::prelude::FileExt as UnixFileExt;
use std::path::Path;
use std::{fs, io};

/// Private helper to retry interruptible syscalls.
macro_rules! retry_eintr {
    ($inner:expr) => {
        loop {
            let err = match $inner {
                Err(e) => e,
                val => break val,
            };

            if let Some(errno) = err.raw_os_error() {
                if errno == libc::EINTR {
                    continue;
                }
            }

            break Err(err);
        }
    };
}

/// Helper functions for openat::Dir
pub trait OpenatDirExt {
    /// Checking for nonexistent files (`ENOENT`) is by far the most common case of inspecting error
    /// codes in Unix.  Rust has a nice `Option<>` type, so this helper makes use of it and returns `Ok(None)`
    /// for nonexistent files.  All other errors are `Err`, and extant files are `Ok(Some<file>))`
    /// of course.
    fn open_file_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<fs::File>>;

    /// Like `std::fs::read_to_string()` but with a path relative to the openat::Dir.
    fn read_to_string<P: openat::AsPath>(&self, p: P) -> io::Result<String>;

    /// Like `read_to_string`, but returns `Ok(None)` for nonexistent paths.
    fn read_to_string_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<String>>;

    /// Remove a file from the given directory; does not error if the target does
    /// not exist.  But will return an error if the target is a directory.
    fn remove_file_optional<P: openat::AsPath>(&self, p: P) -> io::Result<bool>;

    /// Remove an empty sub-directory from the given directory; does not error if the target does
    /// not exist.  But will return an error if the target is a file or symlink.
    fn remove_dir_optional<P: openat::AsPath>(&self, p: P) -> io::Result<bool>;

    /// Like `open_file_optional()` except opens a directory via `openat::dir::sub_dir`.
    fn sub_dir_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<openat::Dir>>;

    /// Like `metadata()` except returns `Ok(None)` for nonexistent paths.
    fn metadata_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<openat::Metadata>>;

    /// On modern filesystems the directory entry contains the type; if available,
    /// return it.  Otherwise invoke `stat()`.
    fn get_file_type(&self, e: &openat::Entry) -> io::Result<openat::SimpleType>;

    /// Returns true iff file exists (may be a directory or symlink).  Symbolic links
    /// are not followed.
    fn exists<P: openat::AsPath>(&self, p: P) -> io::Result<bool>;

    /// Create a directory but don't error if it already exists.
    fn ensure_dir<P: openat::AsPath>(&self, p: P, mode: libc::mode_t) -> io::Result<()>;

    /// Create directory and all parents as necessary; no error is returned if directory already exists.
    fn ensure_dir_all<P: openat::AsPath>(&self, p: P, mode: libc::mode_t) -> io::Result<()>;

    /// Remove all content at the target path, returns `true` if something existed there.
    fn remove_all<P: openat::AsPath>(&self, p: P) -> io::Result<bool>;

    /// Synchronize to disk the filesystem containing this directory.
    fn syncfs(&self) -> io::Result<()>;

    /// If `oldpath` exists, rename it to `newpath`. Otherwise do nothing.
    ///
    /// This returns `true` if the old path has been succesfully renamed, `false` otherwise.
    fn local_rename_optional<P: AsRef<Path>, R: AsRef<Path>>(
        &self,
        oldpath: P,
        newpath: R,
    ) -> io::Result<bool>;

    /// Update timestamps (both access and modification) to the current time.
    ///
    /// If the entry at `path` is a symlink, its direct timestamps are updated without
    /// following the link.
    fn update_timestamps<P: openat::AsPath>(&self, path: P) -> io::Result<()>;

    /// Update permissions for the given path (see `fchmodat(2)`).
    ///
    /// If the entry at `path` is a symlink, no action is performed.
    fn set_mode<P: openat::AsPath>(&self, path: P, mode: libc::mode_t) -> io::Result<()>;

    /// Copy a regular file.  The semantics here are intended to match `std::fs::copy()`.
    /// If the target exists, it will be overwritten.  The mode bits (permissions) will match, but
    /// owner/group will be derived from the current process.  Extended attributes are not
    /// copied.  However, symbolic links will not be followed; instead an error is returned.
    /// If the filesystem supports it, reflinks will be used.
    fn copy_file<S: openat::AsPath, D: openat::AsPath>(&self, s: S, d: D) -> io::Result<()>;

    /// Copy a regular file.
    ///
    /// This is the same as `copy_file`, but can copy to an arbitrary target directory outside
    /// of self.
    fn copy_file_at<S: openat::AsPath, D: openat::AsPath>(
        &self,
        s: S,
        target_dir: &openat::Dir,
        d: D,
    ) -> io::Result<()>;

    /// Create a `FileWriter` which provides a `std::io::BufWriter` and then atomically creates
    /// the file at the destination, renaming it over an existing one if necessary.
    fn new_file_writer<'a>(&'a self, mode: libc::mode_t) -> io::Result<FileWriter>;

    /// Atomically create or replace the destination file, calling the provided
    /// function to generate the contents.  Note that the contents of the
    /// file will not be explicitly sync'd to disk; if you want to do so you
    /// need to invoke `writer.flush()?; writer.get_ref().sync_all()` for example.
    fn write_file_with<P: AsRef<Path>, F, T, E>(
        &self,
        destname: P,
        mode: libc::mode_t,
        gen_content_fn: F,
    ) -> Result<T, E>
    where
        F: FnOnce(&mut std::io::BufWriter<std::fs::File>) -> Result<T, E>,
        E: From<io::Error>,
    {
        let mut w = self.new_file_writer(mode)?;
        gen_content_fn(&mut w.writer).and_then(|t| {
            w.complete(destname)?;
            Ok(t)
        })
    }

    /// Like `write_file_with()` but explicitly synchronizes the target to disk.
    fn write_file_with_sync<P: AsRef<Path>, F, T, E>(
        &self,
        destname: P,
        mode: libc::mode_t,
        gen_content_fn: F,
    ) -> Result<T, E>
    where
        F: FnOnce(&mut std::io::BufWriter<std::fs::File>) -> Result<T, E>,
        E: From<io::Error>,
    {
        let mut w = self.new_file_writer(mode)?;
        gen_content_fn(&mut w.writer).and_then(|t| {
            w.complete_with(destname, |f| f.sync_all())?;
            Ok(t)
        })
    }

    /// Atomically create or replace the destination file with
    /// the provided contents.
    fn write_file_contents<P: AsRef<Path>, C: AsRef<[u8]>>(
        &self,
        destname: P,
        mode: libc::mode_t,
        contents: C,
    ) -> io::Result<()> {
        self.write_file_with(destname, mode, |w| w.write_all(contents.as_ref()))
    }
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

    fn read_to_string<P: openat::AsPath>(&self, p: P) -> io::Result<String> {
        impl_read_to_string(self.open_file(p)?)
    }

    fn read_to_string_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<String>> {
        if let Some(f) = self.open_file_optional(p)? {
            Ok(Some(impl_read_to_string(f)?))
        } else {
            Ok(None)
        }
    }

    fn remove_file_optional<P: openat::AsPath>(&self, p: P) -> io::Result<bool> {
        Ok(impl_remove_file_optional(self, p)?)
    }

    fn remove_dir_optional<P: openat::AsPath>(&self, p: P) -> io::Result<bool> {
        match self.remove_dir(p) {
            Ok(_) => Ok(true),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    Ok(false)
                } else {
                    Err(e)
                }
            }
        }
    }

    fn metadata_optional<P: openat::AsPath>(&self, p: P) -> io::Result<Option<openat::Metadata>> {
        match self.metadata(p) {
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

    fn exists<P: openat::AsPath>(&self, p: P) -> io::Result<bool> {
        match self.metadata(p) {
            Ok(_) => Ok(true),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    Ok(false)
                } else {
                    Err(e)
                }
            }
        }
    }

    fn ensure_dir<P: openat::AsPath>(&self, p: P, mode: libc::mode_t) -> io::Result<()> {
        match self.create_dir(p, mode) {
            Ok(_) => Ok(()),
            Err(e) => {
                if e.kind() == io::ErrorKind::AlreadyExists {
                    Ok(())
                } else {
                    Err(e)
                }
            }
        }
    }

    fn ensure_dir_all<P: openat::AsPath>(&self, p: P, mode: libc::mode_t) -> io::Result<()> {
        let p = to_cstr(p)?;
        let p = p.as_ref();
        // Convert to a `Path` basically just so that we can call `.parent()` on it.
        let p = Path::new(OsStr::from_bytes(p.to_bytes()));
        // Our first system call here is a direct `mkdirat()` - if that succeeds or
        // we get EEXIST then we're done.
        match self.create_dir(p, mode) {
            Ok(_) => {}
            Err(e) => match e.kind() {
                io::ErrorKind::AlreadyExists => {}
                // Otherwise, the expensive path
                io::ErrorKind::NotFound => impl_ensure_dir_all(self, p, mode)?,
                _ => return Err(e),
            },
        }
        Ok(())
    }

    fn copy_file<S: openat::AsPath, D: openat::AsPath>(&self, s: S, d: D) -> io::Result<()> {
        let src = self.open_file(s)?;
        impl_copy_regfile(&src, self, d)
    }

    fn copy_file_at<S: openat::AsPath, D: openat::AsPath>(
        &self,
        s: S,
        target_dir: &openat::Dir,
        d: D,
    ) -> io::Result<()> {
        let src = self.open_file(s)?;
        impl_copy_regfile(&src, target_dir, d)
    }

    fn remove_all<P: openat::AsPath>(&self, p: P) -> io::Result<bool> {
        impl_remove_all(self, p)
    }

    #[allow(unsafe_code)]
    fn syncfs(&self) -> io::Result<()> {
        // syncfs(2) does not work with `O_PATH` FDs, so `self` cannot
        // be directly used. Thus we have to materialize a FD for the
        // directory first.
        let dirfd = self.open_file(".")?;
        let ret = unsafe { libc::syncfs(dirfd.as_raw_fd()) };
        if ret == 0 {
            Ok(())
        } else {
            Err(std::io::Error::last_os_error())
        }
    }

    // NOTE(lucab): this isn't strictly an atomic operation, because
    // unfortunately `renameat` overloads `ENOENT` for multiple error cases.
    fn local_rename_optional<P: AsRef<Path>, R: AsRef<Path>>(
        &self,
        oldpath: P,
        newpath: R,
    ) -> io::Result<bool> {
        if self.exists(oldpath.as_ref())? {
            self.local_rename(oldpath.as_ref(), newpath.as_ref())
                .and(Ok(true))
        } else {
            Ok(false)
        }
    }

    fn update_timestamps<P: openat::AsPath>(&self, p: P) -> io::Result<()> {
        use nix::sys::stat::{utimensat, UtimensatFlags};
        use nix::sys::time::TimeSpec;

        let path = p
            .to_path()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "null byte in path"))?;
        let now = TimeSpec::from(libc::timespec {
            tv_nsec: libc::UTIME_NOW,
            tv_sec: 0,
        });
        retry_eintr!(utimensat(
            Some(self.as_raw_fd()),
            path.as_ref(),
            &now,
            &now,
            UtimensatFlags::NoFollowSymlink,
        )
        .map_err(map_nix_error))
    }

    fn set_mode<P: openat::AsPath>(&self, p: P, mode: libc::mode_t) -> io::Result<()> {
        use nix::sys::stat::{fchmodat, FchmodatFlags, Mode};
        use openat::SimpleType;

        let path = p
            .to_path()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "null byte in path"))?;

        {
            // NOTE(lucab): `AT_SYMLINK_NOFOLLOW` used to short-circuit to `ENOTSUP`
            // in older glibc versions, so we don't use it. Instead we try to detect
            // any symlink, and skip it.
            let entry_meta = self.metadata(path.as_ref())?;
            if entry_meta.simple_type() == SimpleType::Symlink {
                return Ok(());
            };
        }

        let perms = Mode::from_bits_truncate(mode);
        fchmodat(
            Some(self.as_raw_fd()),
            path.as_ref(),
            perms,
            FchmodatFlags::FollowSymlink,
        )
        .map_err(map_nix_error)?;

        Ok(())
    }

    fn new_file_writer<'a>(&'a self, mode: libc::mode_t) -> io::Result<FileWriter> {
        let (tmpf, name) = if let Some(tmpf) = self.new_unnamed_file(mode).ok() {
            (tmpf, None)
        } else {
            // FIXME allow this to be configurable
            let (tmpf, name) = tempfile_in(self, ".tmp", ".tmp", mode)?;
            (tmpf, Some(name))
        };
        Ok(FileWriter::new(self, tmpf, name))
    }
}

fn impl_read_to_string(mut f: File) -> io::Result<String> {
    let mut buf = String::new();
    let _ = f.read_to_string(&mut buf)?;
    Ok(buf)
}

fn map_nix_error(e: nix::Error) -> io::Error {
    io::Error::from_raw_os_error(e as i32)
}

fn copy_regfile_inner(
    src: &File,
    srcmeta: &std::fs::Metadata,
    dest: &mut FileWriter,
) -> io::Result<()> {
    // Go directly to the raw file so we can reflink
    let destf = dest.writer.get_mut();
    let _ = src.copy_to(destf)?;
    let nixmode = nix::sys::stat::Mode::from_bits_truncate(srcmeta.permissions().mode());
    nix::sys::stat::fchmod(destf.as_raw_fd(), nixmode).map_err(map_nix_error)?;
    Ok(())
}

fn impl_copy_regfile<D: openat::AsPath>(
    src: &File,
    target_dir: &openat::Dir,
    d: D,
) -> io::Result<()> {
    let d = to_cstr(d)?;
    let d = OsStr::from_bytes(d.as_ref().to_bytes());
    let meta = src.metadata()?;
    // Start in mode 0600, we will replace with the actual bits after writing
    let mut w = target_dir.new_file_writer(0o600)?;
    copy_regfile_inner(src, &meta, &mut w).and_then(|t| {
        w.complete(d)?;
        Ok(t)
    })
}

fn impl_remove_file_optional<P: openat::AsPath>(d: &openat::Dir, path: P) -> io::Result<bool> {
    match d.remove_file(path) {
        Ok(_) => Ok(true),
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                Ok(false)
            } else {
                Err(e)
            }
        }
    }
}

pub(crate) fn random_name(rng: &mut rand::rngs::ThreadRng, prefix: &str, suffix: &str) -> String {
    let mut tmpname = prefix.to_string();
    for _ in 0..8 {
        tmpname.push(rng.sample(rand::distributions::Alphanumeric).into());
    }
    tmpname.push_str(suffix);
    tmpname
}

pub(crate) fn tempfile_in(
    d: &openat::Dir,
    prefix: &str,
    suffix: &str,
    mode: libc::mode_t,
) -> io::Result<(fs::File, String)> {
    for _ in 0..TEMPFILE_ATTEMPTS {
        let tmpname = random_name(&mut rand::thread_rng(), prefix, suffix);
        match d.new_file(tmpname.as_str(), mode) {
            Ok(f) => return Ok((f, tmpname)),
            Err(ref e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(e) => Err(e)?,
        }
    }
    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        format!(
            "Exhausted {} attempts to create temporary file",
            TEMPFILE_ATTEMPTS
        ),
    ))
}

/// Walk up the path components, creating each directory in turn.  This is
/// pessimistic, assuming no components exist.  But we already handled the
/// optimal case where all components exist above.
pub(crate) fn impl_ensure_dir_all(d: &openat::Dir, p: &Path, mode: libc::mode_t) -> io::Result<()> {
    if let Some(parent) = p.parent() {
        if parent.as_os_str().len() > 0 {
            impl_ensure_dir_all(d, parent, mode)?;
        }
    }
    d.ensure_dir(p, mode)?;
    Ok(())
}

pub(crate) fn remove_children(d: &openat::Dir, iter: openat::DirIter) -> io::Result<()> {
    for entry in iter {
        let entry = entry?;
        match d.get_file_type(&entry)? {
            openat::SimpleType::Dir => {
                let subd = d.sub_dir(&entry)?;
                remove_children(&subd, subd.list_dir(".")?)?;
                let _ = d.remove_dir_optional(&entry)?;
            }
            _ => {
                let _ = d.remove_file_optional(entry.file_name())?;
            }
        }
    }
    Ok(())
}

fn impl_remove_all<P: openat::AsPath>(d: &openat::Dir, p: P) -> io::Result<bool> {
    let cp = to_cstr(p)?;
    let cp = cp.as_ref();
    match impl_remove_file_optional(d, cp) {
        Ok(b) => Ok(b),
        Err(e) => {
            if let Some(ecode) = e.raw_os_error() {
                match ecode {
                    libc::ENOENT => Ok(false),
                    libc::EISDIR => {
                        let iter = d.list_dir(cp)?;
                        let subd = d.sub_dir(cp)?;
                        remove_children(&subd, iter)?;
                        d.remove_dir(cp)?;
                        Ok(true)
                    }
                    _ => Err(e),
                }
            } else {
                unreachable!("Unexpected non-OS error from openat::sub_dir: {}", e)
            }
        }
    }
}

/// A wrapper for atomically replacing a file.  The primary field
/// to access here is the `writer`.  You can also configure the
/// temporary prefix and suffix used for the temporary file before
/// it is moved over the final destination.
///
/// Call `complete()` to `rename()` the file into place.
pub struct FileWriter<'a> {
    /// Write to the destination file.
    pub writer: std::io::BufWriter<std::fs::File>,
    /// This string will be used as a prefix for the temporary file
    pub tmp_prefix: String,
    /// This string will be used as a suffix for the temporary file
    pub tmp_suffix: String,

    /// The target directory
    dir: &'a openat::Dir,
    /// Our temporary file name
    tempname: Option<String>,
}

impl<'a> FileWriter<'a> {
    fn new(dir: &'a openat::Dir, f: std::fs::File, tempname: Option<String>) -> Self {
        Self {
            writer: std::io::BufWriter::new(f),
            tempname,
            tmp_prefix: ".tmp.".to_string(),
            tmp_suffix: ".tmp".to_string(),
            dir,
        }
    }

    fn linkat(
        dir: &openat::Dir,
        fd: fs::File,
        rng: &mut rand::rngs::ThreadRng,
        prefix: &str,
        suffix: &str,
    ) -> io::Result<String> {
        // Unfortunately there's no linkat(LINKAT_REPLACE) yet, so we need
        // to generate a tempfile as the penultimate step.
        for _ in 0..TEMPFILE_ATTEMPTS {
            let tmpname = random_name(rng, prefix, suffix);
            match dir.link_file_at(&fd, tmpname.as_str()) {
                Ok(()) => {
                    return Ok(tmpname);
                }
                Err(e) => {
                    if e.kind() == io::ErrorKind::AlreadyExists {
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
        Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            format!(
                "Exhausted {} attempts to create temporary file",
                TEMPFILE_ATTEMPTS
            ),
        ))
    }

    /// Flush any outstanding buffered data and rename the temporary
    /// file into place.  The provided closure will be invoked
    /// on the real underlying file descriptor before it is
    /// renamed into place.  You can use this to change file attributes.
    /// For example, you can change the mode, extended attributes, or invoke
    /// `fchmod()` to change ownership, etc.
    pub fn complete_with<P: AsRef<Path>, F>(self, dest: P, f: F) -> io::Result<()>
    where
        F: Fn(&fs::File) -> io::Result<()>,
    {
        let dest = dest.as_ref();
        let dir = self.dir;
        let prefix = self.tmp_prefix;
        let suffix = self.tmp_suffix;
        let fd = self.writer.into_inner()?;
        f(&fd)?;
        let mut rng = rand::thread_rng();
        // If we already have a temporary file name (no O_TMPFILE) then
        // use it.  Otherwise since we're done writing, try to link it into place.
        let tmpname = if let Some(t) = self.tempname {
            t
        } else {
            Self::linkat(&dir, fd, &mut rng, prefix.as_str(), suffix.as_str())?
        };
        let tmpname = tmpname.as_str();
        // Then rename it into place.
        match self.dir.local_rename(tmpname, dest) {
            Ok(()) => Ok(()),
            Err(e) => {
                // If we failed, clean up
                let _ = self.dir.remove_file(tmpname);
                Err(e)
            }
        }
    }

    /// Flush any outstanding buffered data and rename the temporary
    /// file into place.
    pub fn complete<P: AsRef<Path>>(self, dest: P) -> io::Result<()> {
        self.complete_with(dest, |_f| Ok(()))
    }
}

// Our methods take &self, not &mut self matching the other raw
// file methods.  We can't use io::copy because it expects mutable
// references, so just reimplement it here.
pub(crate) fn fallback_file_copy(src: &File, dest: &File) -> io::Result<u64> {
    let mut off: u64 = 0;
    let mut buf = [0u8; 8192];
    loop {
        let n = src.read_at(&mut buf, off)?;
        if n == 0 {
            return Ok(off);
        }
        dest.write_all_at(&buf[0..n], off)?;
        off += n as u64;
    }
}

/// Helper functions for std::fs::File
pub trait FileExt {
    /// Copy the entire contents of `self` to `to`.  This uses operating system
    /// specific fast paths if available.
    fn copy_to(&self, to: &File) -> io::Result<u64>;

    /// Update timestamps (both access and modification) to the current time.
    fn update_timestamps(&self) -> io::Result<()>;

    /// Read the exact number of bytes required to fill `buf` starting from `position`,
    /// without affecting file offset.
    fn pread_exact(&self, buf: &mut [u8], position: usize) -> io::Result<()>;
}

impl FileExt for File {
    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    fn copy_to(&self, to: &File) -> io::Result<u64> {
        fallback_file_copy(self, to)
    }

    // Derived from src/libstd/sys/unix/fs.rs in Rust
    #[cfg(any(target_os = "linux", target_os = "android"))]
    fn copy_to(&self, to: &File) -> io::Result<u64> {
        use nix::errno::Errno;
        use nix::fcntl::copy_file_range;
        use std::sync::atomic::{AtomicBool, Ordering};

        // Kernel prior to 4.5 don't have copy_file_range
        // We store the availability in a global to avoid unnecessary syscalls
        static HAS_COPY_FILE_RANGE: AtomicBool = AtomicBool::new(true);

        let len = self.metadata()?.len();

        let has_copy_file_range = HAS_COPY_FILE_RANGE.load(Ordering::Relaxed);
        let mut written = 0u64;
        while written < len {
            let copy_result = if has_copy_file_range {
                let bytes_to_copy = std::cmp::min(len - written, usize::MAX as u64) as usize;
                // We actually don't have to adjust the offsets,
                // because copy_file_range adjusts the file offset automatically
                let copy_result =
                    copy_file_range(self.as_raw_fd(), None, to.as_raw_fd(), None, bytes_to_copy);
                if let Err(ref copy_err) = copy_result {
                    match copy_err {
                        Errno::ENOSYS | Errno::EPERM => {
                            HAS_COPY_FILE_RANGE.store(false, Ordering::Relaxed);
                        }
                        _ => {}
                    }
                }
                copy_result
            } else {
                Err(Errno::ENOSYS)
            };
            match copy_result {
                Ok(ret) => written += ret as u64,
                Err(err) => {
                    match err {
                        os_err
                            if os_err == Errno::ENOSYS
                                || os_err == Errno::EXDEV
                                || os_err == Errno::EINVAL
                                || os_err == Errno::EPERM =>
                        {
                            // Try fallback io::copy if either:
                            // - Kernel version is < 4.5 (ENOSYS)
                            // - Files are mounted on different fs (EXDEV)
                            // - copy_file_range is disallowed, for example by seccomp (EPERM)
                            // - copy_file_range cannot be used with pipes or device nodes (EINVAL)
                            assert_eq!(written, 0);
                            return fallback_file_copy(self, to);
                        }
                        os_err => return Err(map_nix_error(os_err)),
                    }
                }
            }
        }
        Ok(written)
    }

    fn update_timestamps(&self) -> io::Result<()> {
        use nix::sys::{stat::futimens, time::TimeSpec};

        let now = TimeSpec::from(libc::timespec {
            tv_nsec: libc::UTIME_NOW,
            tv_sec: 0,
        });
        retry_eintr!(futimens(self.as_raw_fd(), &now, &now,).map_err(map_nix_error))
    }

    fn pread_exact(&self, buf: &mut [u8], start_pos: usize) -> io::Result<()> {
        use nix::sys::uio::pread;

        if buf.len() == 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "zero-sized buffer in input",
            ));
        }

        let mut total_bytes_read = 0;
        while total_bytes_read < buf.len() {
            let remaining_buf = &mut buf[total_bytes_read..];
            let cur_offset = start_pos.saturating_add(total_bytes_read);
            let bytes_read =
                retry_eintr!(
                    pread(self.as_raw_fd(), remaining_buf, cur_offset as libc::off_t)
                        .map_err(map_nix_error)
                )?;
            total_bytes_read += bytes_read;
            if bytes_read == 0 {
                break;
            }
        }

        if total_bytes_read < buf.len() {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                format!("pread reached EOF after {} bytes", total_bytes_read),
            ));
        }

        Ok(())
    }
}

fn to_cstr<P: openat::AsPath>(path: P) -> io::Result<P::Buffer> {
    path.to_path()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "nul byte in file name"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::{Path, PathBuf};
    use std::time::Duration;
    use std::{error, result, thread};
    use tempfile;

    type Result<T> = result::Result<T, Box<dyn error::Error>>;

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
    fn read_to_string() -> Result<()> {
        let td = tempfile::tempdir()?;
        let d = openat::Dir::open(td.path())?;
        assert!(d.read_to_string("foo").is_err());
        d.write_file_contents("foo", 0o644, "bar")?;
        assert_eq!(d.read_to_string("foo")?, "bar");
        Ok(())
    }

    #[test]
    fn read_to_string_optional() -> Result<()> {
        let td = tempfile::tempdir()?;
        let d = openat::Dir::open(td.path())?;
        assert!(d.read_to_string_optional("foo")?.is_none());
        d.write_file_contents("foo", 0o644, "bar")?;
        assert!(d.read_to_string_optional("foo")?.is_some());
        assert_eq!(d.read_to_string_optional("foo")?.unwrap(), "bar");
        Ok(())
    }

    #[test]
    fn remove_file_optional() -> Result<()> {
        let td = tempfile::tempdir()?;
        let d = openat::Dir::open(td.path())?;
        d.write_file("foo", 0o644)?.sync_all()?;
        assert!(d.open_file_optional("foo")?.is_some());
        let removed = d.remove_file_optional("foo")?;
        assert!(removed);
        assert!(d.open_file_optional("foo")?.is_none());
        Ok(())
    }

    #[test]
    fn metadata_optional() -> Result<()> {
        let td = tempfile::tempdir()?;
        let d = openat::Dir::open(td.path())?;
        assert!(d.metadata_optional("foo")?.is_none());
        d.write_file("foo", 0o644)?.sync_all()?;
        assert!(d.metadata_optional("foo")?.is_some());
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

    #[test]
    fn ensure_dir_all() -> Result<()> {
        let td = tempfile::tempdir()?;
        let d = openat::Dir::open(td.path())?;
        let mode = 0o755;
        let p = Path::new("foo/bar/baz");
        d.ensure_dir_all(p, mode)?;
        assert_eq!(d.metadata(p)?.stat().st_mode & !libc::S_IFMT, mode);
        d.ensure_dir_all(p, mode)?;
        d.ensure_dir_all("foo/bar", mode)?;
        d.ensure_dir_all("foo", mode)?;
        d.ensure_dir_all("bar", 0o700)?;
        assert_eq!(d.metadata("bar")?.stat().st_mode & !libc::S_IFMT, 0o700);
        Ok(())
    }

    #[test]
    fn test_local_rename() {
        let td = tempfile::tempdir().unwrap();
        let d = openat::Dir::open(td.path()).unwrap();

        {
            d.ensure_dir_all("src/foo", 0o755).unwrap();
            let renamed = d.local_rename_optional("src", "dst").unwrap();
            assert_eq!(renamed, true);
            assert_eq!(d.exists("src").unwrap(), false);
            assert_eq!(d.exists("dst/foo").unwrap(), true);
            let noent = d.local_rename_optional("src", "dst").unwrap();
            assert_eq!(noent, false);
            assert_eq!(d.remove_all("dst").unwrap(), true);
        }
        {
            let noent = d.local_rename_optional("missing", "dst").unwrap();
            assert_eq!(noent, false);
        }
        {
            d.ensure_dir_all("src/foo", 0o755).unwrap();
            let renamed = d.local_rename_optional("src", "dst").unwrap();
            assert_eq!(renamed, true);
            assert_eq!(d.exists("dst/foo").unwrap(), true);
            d.ensure_dir_all("src", 0o755).unwrap();
            let _ = d.local_rename_optional("src", "dst").unwrap_err();
            assert_eq!(d.exists("dst/foo").unwrap(), true);
            assert_eq!(d.remove_all("dst").unwrap(), true);
        }
    }

    #[test]
    fn test_syncfs() {
        let td = tempfile::tempdir().unwrap();
        let d = openat::Dir::open(td.path()).unwrap();
        d.ensure_dir_all("foo/bar", 0o755).unwrap();
        d.syncfs().unwrap();
        assert_eq!(d.exists("foo/bar").unwrap(), true);
    }

    #[test]
    fn test_update_timestamps() {
        // File timestamps can not be updated faster than kernel ticking granularity,
        // so this test has to artificially sleep through several timer interrupts.
        const TICKING_PAUSE: Duration = Duration::from_millis(100);

        let td = tempfile::tempdir().unwrap();
        let d = openat::Dir::open(td.path()).unwrap();

        {
            d.ensure_dir("foo", 0o755).unwrap();
            let before = d.metadata("foo").unwrap();
            thread::sleep(TICKING_PAUSE);
            d.update_timestamps("foo").unwrap();
            let after = d.metadata("foo").unwrap();
            if before.stat().st_mtime == after.stat().st_mtime {
                assert_ne!(before.stat().st_mtime_nsec, after.stat().st_mtime_nsec);
            }
        }
        {
            use nix::sys::stat::fstat;

            let bar = d.update_file("bar", 0o644).unwrap();
            let before = fstat(bar.as_raw_fd()).unwrap();
            thread::sleep(TICKING_PAUSE);
            bar.update_timestamps().unwrap();
            let after = fstat(bar.as_raw_fd()).unwrap();
            if before.st_mtime == after.st_mtime {
                assert_ne!(before.st_mtime_nsec, after.st_mtime_nsec);
            }
        }
    }

    #[test]
    fn test_fchmodat() {
        let td = tempfile::tempdir().unwrap();
        let d = openat::Dir::open(td.path()).unwrap();
        d.ensure_dir("foo", 0o777).unwrap();
        d.set_mode("foo", 0o750).unwrap();
        assert_eq!(
            d.metadata("foo").unwrap().stat().st_mode & !libc::S_IFMT,
            0o750
        );
        d.set_mode("foo", 0o700).unwrap();
        assert_eq!(
            d.metadata("foo").unwrap().stat().st_mode & !libc::S_IFMT,
            0o700
        );

        d.symlink("bar", "foo").unwrap();
        d.set_mode("bar", 0o000).unwrap();
        assert_ne!(
            d.metadata("bar").unwrap().stat().st_mode & !libc::S_IFMT,
            0o000
        );
        assert_ne!(
            d.metadata("foo").unwrap().stat().st_mode & !libc::S_IFMT,
            0o000
        );
    }

    fn find_test_file(tempdir: &Path) -> Result<PathBuf> {
        for p in ["/proc/self/exe", "/usr/bin/bash"].iter() {
            let p = Path::new(p);
            if p.exists() {
                return Ok(p.into());
            }
        }
        let fallback = tempdir.join("testfile-fallback");
        std::fs::write(&fallback, "some test data")?;
        Ok(fallback)
    }

    #[test]
    fn copy_fallback() -> Result<()> {
        use std::io::Read;
        let td = tempfile::tempdir()?;
        let src_p = find_test_file(td.path())?;
        let dest_p = td.path().join("bash");
        {
            let src = File::open(&src_p)?;
            let dest = File::create(&dest_p)?;
            let _ = fallback_file_copy(&src, &dest)?;
        }
        let mut src = File::open(&src_p)?;
        let mut srcbuf = Vec::new();
        let _ = src.read_to_end(&mut srcbuf)?;
        let mut destbuf = Vec::new();
        let mut dest = File::open(&dest_p)?;
        let _ = dest.read_to_end(&mut destbuf)?;
        assert_eq!(srcbuf.len(), destbuf.len());
        assert_eq!(&srcbuf, &destbuf);
        Ok(())
    }

    #[test]
    fn copy_file_at() {
        let src_td = tempfile::tempdir().unwrap();
        let src_dir = openat::Dir::open(src_td.path()).unwrap();
        src_dir
            .write_file_contents("foo", 0o644, "test content")
            .unwrap();

        let dst_td = tempfile::tempdir().unwrap();
        let dst_dir = openat::Dir::open(dst_td.path()).unwrap();

        assert_eq!(dst_dir.exists("bar").unwrap(), false);
        src_dir.copy_file_at("foo", &dst_dir, "bar").unwrap();
        assert_eq!(dst_dir.exists("bar").unwrap(), true);

        let srcbuf = {
            let mut src = src_dir.open_file("foo").unwrap();
            let mut srcbuf = Vec::new();
            let _ = src.read_to_end(&mut srcbuf).unwrap();
            srcbuf
        };
        let destbuf = {
            let mut destbuf = Vec::new();
            let mut dest = dst_dir.open_file("bar").unwrap();
            let _ = dest.read_to_end(&mut destbuf).unwrap();
            destbuf
        };
        assert_eq!(&srcbuf, b"test content");
        assert_eq!(&srcbuf, &destbuf);
    }

    #[test]
    fn test_pread_exact() {
        let td = tempfile::tempdir().unwrap();
        let d = openat::Dir::open(td.path()).unwrap();
        static TESTINPUT: &str = "test1 test2 test3";
        d.write_file_contents("foo", 0o700, TESTINPUT).unwrap();
        let mut testfile = d.open_file("foo").unwrap();
        {
            let mut buf = [0; 0];
            let _ = testfile.pread_exact(&mut buf, 0).unwrap_err();
        }
        {
            let mut buf = [0; 18];
            let _ = testfile.pread_exact(&mut buf, 0).unwrap_err();
        }
        {
            let mut buf = [0; 1];
            let _ = testfile.pread_exact(&mut buf, 2000).unwrap_err();
        }
        {
            let mut buf1 = [0; 5];
            let mut buf2 = [0; 5];
            let _ = testfile.pread_exact(&mut buf1, 6).unwrap();
            let _ = testfile.pread_exact(&mut buf2, 6).unwrap();
            assert_eq!(buf1, "test2".as_bytes());
            assert_eq!(buf1, buf2);

            let mut str_buf = String::new();
            let _ = testfile.read_to_string(&mut str_buf).unwrap();
            assert_eq!(str_buf, TESTINPUT);
        }
    }
}
