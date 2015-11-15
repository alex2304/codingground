;unit-testing date comparison
;(print (date_bigger "25.10.2015" "25.11.2015")) ;1
;(print (date_bigger "25.10.2015" "25.10.2015")) ;0
;(print (date_bigger "25.10.2015" "26.10.2015")) ;1
;(print (date_bigger "26.10.2015" "25.10.2015")) ;-1
;(print (date_bigger "25.11.2015" "25.10.2015")) ;-1
;(print (date_bigger "25.10.2016" "25.10.2015")) ;-1
;(print (date_bigger "26.11.2015" "25.10.2016")) ;1
;unit-test date_between
;(print (date_between "25.10.2015" "25.10.2015" "26.10.2015"))
;(print (date_between "24.10.2015" "25.10.2015" "25.11.2015"))