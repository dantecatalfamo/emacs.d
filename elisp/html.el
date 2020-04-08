(defun attrs (properties)
  "Construct properties for an html tag from PROPERTIES.
Properties can either be a list of properties or a single property,
with the form (PROPERTY VALUE1 VALUE2 VALUE2)."
  (cond
   ((listp (car properties))
    (apply #'concat (mapcar #'attrs properties)))
   (properties (format " %s=\"%s\"" (car properties) (mapconcat #'identity (cdr properties) " ")))
   (t nil)))

(defmacro deftag (name)
  "Define an HTML tag with name NAME."
  (let* ((tagname (symbol-name name))
         (tagtext (format "<%s%%s>%%s</%s>" tagname tagname)))
    `(defmacro ,name (properties &rest inner)
       `(format ,,tagtext
                (attrs ',properties)
                (apply #'concat (mapcar #'eval ',inner))))))

(defmacro deftags (&rest tags)
  "Define nultiple TAGS in one statement."
  (let (defined)
    (dolist (tag tags defined)
      (push `(deftag ,tag) defined))
    `(progn ,@defined)))

(defmacro deftag-single (name)
  "Define an HTML tag with name NAME with no content or closing tag."
  (let* ((tagname (symbol-name name))
         (tagtext (format "<%s%%s>" tagname)))
    `(defmacro ,name (&rest properties)
       `(format ,,tagtext
                (attrs ',properties)))))

(defmacro deftags-single (&rest tags)
  "Define multiple single TAGS in one statement."
  (let (defined)
    (dolist (tag tags defined)
      (push `(deftag-single ,tag) defined))
    `(progn ,@defined)))

(defmacro deftag-short (name)
  "Define an html tag with name of NAME1 for NAME that takes no properties."
  (let* ((tagname (symbol-name name))
         (shorttag (intern (concat tagname "1"))))
    `(defmacro ,shorttag (&rest inner)
       `(,',name nil ,@inner))))

(defmacro deftags-short (&rest tags)
  "Define multiple short TAGS in one statement."
  (let (defined)
    (dolist (tag tags defined)
      (push `(deftag-short ,tag) defined))
    `(progn ,@defined)))

(defmacro deftags1 (&rest tags)
  "Define multiple long and short TAGS at once."
  `(progn (deftags ,@tags)
          (deftags-short ,@tags)))


(deftags1 html head title body div p b script style span)
(deftags-single br link)

(html1
 (head1 (title1 "This is the page, baby"))
 (body1
  (div1
   (div (class "sauce")
        "here we are, this is the sauce heaven")
   (div (id "thing")
        (span1 "Big"))
   (br)
   (div (class "footer")
        "This is the bottom"))))
