(load "db.lisp")

(defun f21blen (cars soldcars manufacturer date1 date2)
    (length (f21b cars soldcars manufacturer date1 date2))
)

(defun f21b (cars soldcars manufacturer date1 date2)
    (let ((match (select_equals cars (list (append (list 'MANUFACTURER) (list manufacturer))))) result CAR_ID tmpres)
        (loop
            (if (NULL match) (return result))
            (setq CAR_ID (get_value cars 'ID (car match)))
            (setq tmpres
                (select soldcars (append (list
                                            (predicate 'ID_EXTERNAL ((LAMBDA (CAR_ID) 
                                                                            (LAMBDA (VAL) (string= val CAR_ID)) 
                                                                      ) CAR_ID
                                                                    )
                                            )
                                         )
                                         (list
                                            (predicate 'BUY_DATE ((LAMBDA (DATE1 DATE2) 
                                                                            (LAMBDA (VAL) (date_between val DATE1 DATE2)) 
                                                                      ) date1 DATE2
                                                                  )
                                            )
                                         )
                                 )
                )
            )
            (setq result (append result tmpres))
            (setq match (cdr match))
        )
    )
)

(defun f22a (cars sitsmin sitsmax enginemin enginemax)
    (setq sitsmin (parse-integer sitsmin))
    (setq sitsmax (parse-integer sitsmax))
    (setq enginemin (parse-integer enginemin))
    (setq enginemax (parse-integer enginemax))
    (setq tmpres
        (select cars (append (list
                                    (predicate 'SITS_COUNT ((LAMBDA (sitsmin sitsmax) 
                                                                    (LAMBDA (VAL) (integer_between (parse-integer  VAL) sitsmin sitsmax)) 
                                                              ) sitsmin sitsmax
                                                            )
                                    )
                                 )
                                 (list
                                    (predicate 'ENGINE_POWER ((LAMBDA (enginemin enginemax) 
                                                                    (LAMBDA (VAL) (integer_between (parse-integer  VAL) enginemin enginemax)) 
                                                              ) enginemin enginemax
                                                          )
                                    )
                                 )
                         )
        )
    )
)

(defun f22b (cars soldcars date1 date2)
    (let ((match (select cars NIL)) result CAR_ID tmpres)
        (loop
            (if (NULL match) (return result))
            (setq CAR_ID (get_value cars 'ID (car match)))
            (setq MARKA (get_value cars 'MANUFACTURER (car match)))
            (setq tmpres
                (select soldcars (append (list
                                            (predicate 'ID_EXTERNAL ((LAMBDA (CAR_ID) 
                                                                            (LAMBDA (VAL) (string= val CAR_ID)) 
                                                                      ) CAR_ID
                                                                    )
                                            )
                                         )
                                         (list
                                            (predicate 'BUY_DATE ((LAMBDA (DATE1 DATE2) 
                                                                            (LAMBDA (VAL) (date_between val DATE1 DATE2)) 
                                                                      ) date1 DATE2
                                                                  )
                                            )
                                         )
                                 )
                )
            )
            (setq CAR_ID (get_value cars 'ID (car match)))
            (setq result (append result (list (list MARKA (length tmpres)))))
            (setq match (cdr match))
        )
    )
)

(defun f23a (cars country)
    (let ((match (select_equals cars (list (append (list 'COUNTRY) (list country))))) result)
        (loop
            (if (NULL match) (return result))
            (setq MARKA (get_value cars 'MANUFACTURER (car match)))
            (setq POWER (get_value cars 'ENGINE_POWER (car match)))
            (setq result (append result (list (list MARKA POWER))))
            (setq match (cdr match))
        )
    )
)

(defun f23b (cars soldcars enginenum)
    (let ((pr (list (append (list 'ENGINE_NUMBER) (list enginenum)))) match result CAR_ID tmpres)
        (setq match (select_equals cars pr))
        (loop
            (if (NULL match) (return result))
            (setq CAR_ID (get_value cars 'ID (car match)))
            (select_delete soldcars (list (predicate 'ID_EXTERNAL 
                                        ((LAMBDA (CAR_ID) 
                                            (LAMBDA (VAL) (string= val CAR_ID))) CAR_ID)
                                   )
                              )
            )
            (select_delete cars (list (predicate 'ID 
                                        ((LAMBDA (CAR_ID) 
                                            (LAMBDA (VAL) (string= val CAR_ID))) CAR_ID)
                                   )
                              )
            )
            (setq result (append result tmpres))
            (setq match (cdr match))
        )
    )
)


;init DB and menu
(setq TCARS 'CARS)
(setq TCARSSOLD 'CARS_SOLD)
(setq TMENU 'MENU)
(load_db TCARS "columns_cars.txt" "cars.txt")
(load_db TCARSSOLD "columns_sold_cars.txt" "sold_cars.txt")
(init_menu TMENU "menu.txt" "arguments.txt")

;display menu
(setq selected_menu NIL)
(loop
    (setq selected_menu (display_menu TMENU "Please, choice an action:"))
    (cond
        ( (equal selected_menu "0")(return) )
    )
   (setq args (select_menu_item TMENU selected_menu))                ;user inputs required arguments (for command <selected_menu>)
   (setq result (do_action TCARS TCARSSOLD selected_menu args))       ;executing certain command
   (print "RESULT:")
   (print result)
   (write-line "")(write-line "")
)

;saving
(save_db TCARS "cars_new.txt")
(save_db TCARSSOLD "saled_cars_new.txt")