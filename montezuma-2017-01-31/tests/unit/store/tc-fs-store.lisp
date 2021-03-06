(in-package #:montezuma)

(deftestfun test-fs-store
  (let* ((path (make-test-directory "fs-store/"))
         (dir (make-fs-directory path :create-p t)))
    (do-test-basic-file-ops dir)
    (do-test-rename dir)
    (do-test-modified dir)
    (do-test-rw-bytes dir)
    (do-test-rw-ints dir)
    (do-test-rw-longs dir)
    (do-test-rw-uints dir)
    (do-test-rw-ulongs dir)
    (do-test-rw-vints dir)
    (do-test-rw-vlongs dir)
    (do-test-rw-strings dir)
    (do-test-buffer-seek dir)
    (do-test-read-bytes dir)
    (do-test-clone dir)))

