use nix;
use std::fs::File;
use std::io;

/// Helper functions for std::fs::File
pub trait FileExt {
    /// Copy the entire contents of `self` to `to`.  This uses operating system
    /// specific fast paths if available.
    fn copy_to(&mut self, to: &mut File) -> io::Result<u64>;
}

impl FileExt for File {
    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    fn copy_to(&mut self, to: &mut File) -> io::Result<u64> {
        return io::copy(self, &mut to);
    }

    // Derived from src/libstd/sys/unix/fs.rs in Rust
    #[cfg(any(target_os = "linux", target_os = "android"))]
    fn copy_to(&mut self, to: &mut File) -> io::Result<u64> {
        use nix::fcntl::copy_file_range;
        use nix::errno::Errno;
        use std::os::unix::io::AsRawFd;
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
                    match copy_err.as_errno() {
                        Some(Errno::ENOSYS) | Some(Errno::EPERM) => {
                            HAS_COPY_FILE_RANGE.store(false, Ordering::Relaxed);
                        }
                        _ => {}
                    }
                }
                copy_result
            } else {
                Err(nix::Error::from_errno(Errno::ENOSYS))
            };
            match copy_result {
                Ok(ret) => written += ret as u64,
                Err(err) => {
                    match err.as_errno() {
                        Some(os_err)
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
                            return io::copy(self, to);
                        }
                        Some(os_err) => return Err(io::Error::from_raw_os_error(os_err as i32)),
                        _ => return Err(io::Error::new(io::ErrorKind::Other, err))
                    }
                }
            }
        }
        Ok(written)
    }
}
