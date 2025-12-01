;; come file metto input non habeo sbatti di fare argv

(with-open-file (is "input"
                    :direction :input)
  (do ((c (read-char is nil)
          (read-char is nil)))
      ((null c) nil)
    (write-char c)))
