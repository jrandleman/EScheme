; compile.scm -- recursively deletes ALL .class files, then compiles Main.java

(define cwd-prefix-length (+ 1 (length (getcwd))))

(define all-files-from-cwd
  (let get-files-from ((dirname (getcwd)))
    (define entries (directory-entries dirname))
    (define files (filter file? entries))
    (for-each (lambda (dir) (set! files (append files (get-files-from dir))))
      (filter directory? entries))
    files))

(define cwd-class-files 
  (map (lambda (filename) (regex-replace (regex-replace (regex-replace-all (slice filename cwd-prefix-length) "\\$" "\\$") " " "\\ ") "\\." "\\."))
       (filter \(string-contains %1 ".class") all-files-from-cwd)))

(for-each (lambda (file) (system (sprintf "rm %s" file))) cwd-class-files)

; (system "javac -source 11 -target 11 Main.java")