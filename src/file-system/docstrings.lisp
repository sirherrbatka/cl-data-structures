(cl:in-package #:cl-data-structures.file-system)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function line-by-line
    (:description "Opens a text file, and allows reading it line by line."
     :returns "Forward range. Provides access to each line in the file stored as a string."
     :exceptional-situations ("Signalling error from a callback during traverse will close the inner stream automaticly."
                              "Signals errors just as CL:OPEN does or UIOP:LAUNCH-PROGRAM (if instead of file path COMMAND object has been passed.")
     :notes "Stream is opened lazily. Calling either TRAVERSE or ACROSS on the line-by-line range will automaticly close the inner file (in exception safe way). This makes it suitable to use with the aggregation functions without additional code."))

  (type command
    (:description "Instances of this class can be passed to functions in cl-ds expecting a file path (like for instance LINE-BY-LINE)."))

  (function command
    (:description "This function contructs COMMAND object which can be then passed to functions in this package expecting name, for instance LINE-BY-LINE. Passed argumement will be passed to UIOP:LAUNCH-PROGRAM and the process-info-output stream will be used as it would point to a normal file."
     :returns "COMMAND instance."
     :exceptional-situations ("Signals the type error if format-control-string is not of the type STRING.")
     :arguments-and-values ((format-control-string "Control string passed to format, for contructing the effective command.")
                            (format-arguments "Args passed to the format, for constructing the effective command."))
     :notes ("This is particulary useful in conjuction with LINE-BY-LINE, unix tools, and aggreation functions in CL-DS as it allows to combine useful tools present in the unix system (like for instance xz) with CL-DS.ALG:ON-EACH, CL-DS.ALG:GROUP-BY, and different kinds of aggregation functions to quickly handle data processing tasks."
             "Returned COMMAND instance is immutable and can be safely passed as input multiple times")))

  (function find
    (:description "A function somewhat similar to the posix find tool. Depending on the filesystem content and the DESCRIPTION list, returns a forward-range containing pathnames matching the DESCRIPTION list. DESCRIPTION list supports :directory, :regex-file, :regex-directory, :file, :all-directories and :all-files filters. Type of the filter is supplied as the first element, rest of the specifies the additional options in the form of the plist (:PATH, :PREDICATE etc.). Each of those filters selects matching nodes in the file system and handles it to the next filter. The complete DESCRIPTION list usually looks `((:directory :path "/path/to/dir") (:all-files :predicate i-want-this-file-p)). In this example first filter in the list designates the directory to scan for files and the second filter will select all files in the directory that are accepted by i-want-this-file-p function."
     :returns "Forward range. Contains paths to the matched files."
     :notes ((:directory "This filter is used to specify directory by :PATH.")
             (:regex-directory "This filter is used to specify directory by PPCRE in the :PATH. Directories with a maching name will be accepted. Can enter directories recursivly and the argument controlling behavior is :TIMES. By default 1, meaning it enters into directories once. Can be set to NIL in order for a unlimited recursive descent.")
             (:file "This filter is used to specify file by name supplied in :PATH.")
             (:regex-file "This filter is used to specify file by PPCRE in the :PATH. Files with a matching name will be accepted.")
             (:all-directories "Recursivly selects all directories in the directory returned by the previous filter. Follows symlinks.")
             (:all-files "Selects all files in the directory returned by the previous filter. :PREDICATE can be used to supply filtering function.")
             ))))
