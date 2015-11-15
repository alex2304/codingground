(DEFUN PUT (sym prop val) (setf (get sym prop) val))
(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))
     

(defun create_columns (col_list)
     (let ((DB_COLUMNS '())) 
	     (loop
	       (setq cur_element (CAR col_list))
	       (setq col_name (CAR cur_element))
	       (put col_name 'definition (nth 1 cur_element))
	       (put col_name 'nullable (nth 2 cur_element))
               (setq DB_COLUMNS (append DB_COLUMNS (list col_name)))
	       (setq col_list (CDR col_list))
	       (if (NULL col_list) (return DB_COLUMNS))
	     )
     )
)

(defun get_col_param (columns col_name param) 
     (if (NULL columns) 
        NIL
	    (if (eql (car columns) col_name) (get (car columns) param) (get_col_param (cdr columns) col_name param))
	  )
)

(defun is_nullable (columns column_name) 
     (if (get_col_param columns column_name 'nullable) T NIL)
)

(defun definition (columns column_name) 
     (get_col_param columns column_name 'definition)
)

;user input

(defun read_user_input () (read-line))


(defun read_user_line (col_name definition nullable)
    (write-line (concatenate 'string "Enter value for " (string col_name)))
    (write-line (concatenate 'string "This value means " (string definition)))
    (loop
        (let ((userline (read_user_input)))
            (if (= (length userline) 0)
                (if nullable (return userline) (write-line "Cant be empty! Try again..."))
                (return userline)
            )
        )
    )
)

(defun user_create_new_row (db)
    (let ((column_names (get db 'COLUMNS)) (columns (get db 'COLUMNS)) (result '()))
        (loop
            (if (NULL columns) (return (put db 'ROWS (nconc (get db 'ROWS) (list result)))))
            (let ((cur_col (car columns)))
                (setq result (append result 
                                    (list (read_user_line 
                                        cur_col 
                                        (definition column_names cur_col) 
                                        (is_nullable column_names cur_col)
                                    ))
                              )
                )
                (setq columns (cdr columns))
            )
        )
    )
)

(defun create_new_row (db lst)
    (put db 'ROWS (nconc (get db 'ROWS) (list lst)))
)

(defun create_db (db col_list)
    (put db 'COLUMNS (create_columns col_list))
    (put db 'ROWS ())
)

(defun get_value (db col_name row)
    (let ((columns (get db' COLUMNS)) (i 0))
        (loop
            (if (NULL columns) (return NIL))
            (if (eql (car columns) col_name)
                (return (nth i row)))
            (setq columns (cdr columns))
            (setq i (+ i 1))
        )
    )
)

(defun predicate (nm fn)
    (let (lst)
        (setq lst (append lst (list nm)))
        (setq lst (append lst (list fn)))
        (if T lst)
    )       
)

(defun select_delete (db pairs)
    (let ((result ()) (allrows (get db 'ROWS)) (rows (get db 'ROWS)) (columns (get db' COLUMNS)) row)
        (loop
            (if (NULL rows) (return result))
            (setq row (car rows))
            (let ((ppairs pairs) (matches T) colname filter value)
                (loop
                    (if (NULL ppairs)
                        (if matches 
                            (return NIL)
                            (return (setq result (append result (list row))))
                        )
                    )
                    (setq colname (nth 0 (car ppairs)))
                    (setq filter (nth 1 (car ppairs)))
                    (setq value (get_value db colname row))
                    (if (funcall (eval filter) value) T (setq matches NIL))
                    (setq ppairs (cdr ppairs))
                )
            )
            (setq rows (cdr rows))
        )
        (put db 'ROWS result)
    )
)

(defun select (db pairs)
    (let ((result ()) (rows (get db 'ROWS)) (columns (get db' COLUMNS)) row)
        (loop
            (if (NULL rows) (return result))
            (setq row (car rows))
            (let ((ppairs pairs) (matches T) colname filter value)
                (loop
                    (if (NULL ppairs)
                        (if matches 
                            (return (setq result (append result (list row))))
                            (return NIL)
                        )
                    )
                    (setq colname (nth 0 (car ppairs)))
                    (setq filter (nth 1 (car ppairs)))
                    (setq value (get_value db colname row))
                    (if (funcall (eval filter) value) T (setq matches NIL))
                    (setq ppairs (cdr ppairs))
                )
            )
            (setq rows (cdr rows))
        )
    )
)

(defun select_equals (db pairs)
    (let ((ppairs pairs) lst cur tmp tmp_val)
        (loop
            (if (NULL ppairs) (return (select db lst)))
            (setq cur (car ppairs))
            (setq tmp (predicate (nth 0 cur) ((LAMBDA (tmp_val) (LAMBDA (val) (string= val tmp_val))) (nth 1 cur))))
            (setq lst (append lst (list tmp)))
            (setq ppairs (cdr ppairs))
        )
    )
)

(defun select_date (db date1 date2)
    (let (lst)
        (setq lst (predicate 'DATE ((LAMBDA (date1 date2) (LAMBDA (val) (date_between val date1 date2))) date1 date2)))
        (select db (list lst))
    )
)

(defun select_by_date_and_model (db model date1 date2)
    (let (lst)
        (setq lst (list (predicate 'DATE ((LAMBDA (date1 date2) (LAMBDA (val) (date_between val date1 date2))) date1 date2))))
        (NCONC lst (list (predicate 'MODEL ((LAMBDA (model) (LAMBDA (val) (string= val model))) model))))
        (select db lst)
    )
)

(defun date_bigger (l r)
    (let ((left (split-str l ".")) (right (split-str r ".")) (i 0) (val 0) curL curR)
        (loop
            (if (= i 3) (return val))
            (setq curL (parse-integer (nth i left)))
            (setq curR (parse-integer (nth i right)))
            (cond
                ((> curL curR) (setq val -1))
                ((< curL curR) (setq val 1))
            )
            (setq i (+ i 1))
        )
    )
)

(defun date_between (cur l r)
    (if (or (= (date_bigger l cur) 1) (= (date_bigger l cur) 0))
        (if (or (= (date_bigger r cur) -1) (= (date_bigger r cur) 0)) T NIL)
        NIL
    )
)

(defun integer_between (cur l r)
    (if (or (= l cur) (> cur l))
        (if (or (= r cur) (> r cur)) T NIL)
        NIL
    )
)

;i/o + menu

(defun read_file (file_name)
    (setq in (open file_name))
    (setq result '())
    (loop 
	    (setq line (read in nil))
		(cond					        ;CHECKING FOR INPUT END..
			( (null line) (close in)(return result) )	;IF INPUT ENDS - REVOKE THE INPUT FILE ('DISCONNECT FROM DB')
		)
		(setq result (append result (list line)))
	)
)

;returns new db
(defun load_db (db columns_file rows_file)
    ;CREATE EMPTY DB
	(create_db db (read_file columns_file))
	;FILLING DB FROM FILE
	(setq rows (read_file rows_file))
	(do 					    
		( (i 0 (+ i 1)) )							
		( (= i (length rows)))				
		( create_new_row db (nth i rows) )    
	)
	db
)

(defun save_db (db file_name)
    (setq out (open file_name :direction :output :if-exists :supersede))
    (setq rows (get db 'ROWS))
    (do 
        ( (i 0 (+ i 1)) )	
        ( (= i (length rows)))	
        ( write-line  (write-to-string (nth i rows)) out)
    )
    (close out)
)

(defun init_menu (menu menu_file arg_file)
    (put menu 'ITEMS '())
    (setq menu_items (read_file menu_file))
    (do 					    
		( (i 0 (+ i 1)) )							
		( (= i (length menu_items)))				
		( put menu 'ITEMS (nconc (get menu 'ITEMS) (list (nth i menu_items))) )    
	)
	(put menu 'ARGS '())
	(setq arguments (read_file arg_file))
	(do 					    
		( (i 0 (+ i 1)) )							
		( (= i (length arguments)))				
		( setq command_num (car (nth i arguments)) )
		( put menu 'ARGS (nconc (get menu 'ARGS) (list (nth i arguments))) )    
	)
)

(defun display_menu (menu preview_msg)
    (write-line (string preview_msg))
    (setq menu_items (get menu 'ITEMS))
    (do 					    
		( (i 0 (+ i 1)) )							
		( (= i (length menu_items)))
		(setq row (nth i menu_items))
		( write-line (concatenate 'string  (string (nth 0 row)) " - " (string (nth 1 row))) )  
	)
    (read_user_input)
)

(defun select_menu_item (menu selected)
    (setq args (get menu 'ARGS))
    (setq args_values '())
    (do 					    
		( (i 0 (+ i 1)) )							
		( (= i (length args)))
		(setq arg (nth i args))
		(cond
		    (   (equal (car arg) selected)                                          ;if current command == selected command..
		        (write-line (string (nth 2 arg)))                                   ;display information about inputting data
		        (setq args_values (append args_values (list (read_user_input))))    ;read and save user input
		    )
		)
	)
	args_values
)

(defun do_action (cars cars_sold command args)
    (setq int_com (parse-integer command))
    (cond
        ( 
            (equal int_com 1)                   ;Action 1 (add a new car)
                (user_create_new_row cars)
        )
        (
            (equal int_com 2)                   ;Action 2 
                (f21b cars cars_sold (nth 0 args) (nth 1 args) (nth 2 args))
        )
        (
            (equal int_com 3)                   ;Action 3 
                (f22a cars (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args))
        )
        (
            (equal int_com 4)                   ;Action 4 
                (f22b cars cars_sold (nth 0 args) (nth 1 args))
        )
        (
            (equal int_com 5)                   ;Action 5 
                (f23a cars (nth 0 args))
        )
        (
            (equal int_com 6)                   ;Action 6 (remove a car)
                (f23b cars cars_sold (nth 0 args))
                (select cars NIL)
        )
    )
)

