CLASS zbtp_class_01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  METHODS:
    create_itab IMPORTING lr_out TYPE REF TO if_oo_adt_classrun_out ,
    copy_itab IMPORTING lr_out TYPE REF TO if_oo_adt_classrun_out ,
    copy_itab_with_feature IMPORTING lr_out TYPE REF TO if_oo_adt_classrun_out.
  PROTECTED SECTION.
  PRIVATE SECTION.
  Types: Begin of ls_main,
          field1(2) type C,
          field2(2) type C,
          field3(10) type N,
         END OF ls_main,
         begin of ls_filter,
          field3(10) type n,
         END OF ls_filter.
  TYPES:
         tt_main type STANDARD TABLE OF ls_main WITH EMPTY KEY,
         tt_filter type STANDARD TABLE OF ls_filter WITH EMPTY KEY.
  DATA: gt_main type tt_main,
        gt_filter type tt_filter.
ENDCLASS.



CLASS zbtp_class_01 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
   out->write(
     EXPORTING
       data   = 'Hello ABAP Cloud !!'
       name   = 'Tapomay'
     RECEIVING
       output = data(lo_out1)
   ).
   me->create_itab( out ).
   me->copy_itab( out ).
   me->copy_itab_with_feature( out ).
  ENDMETHOD.

  METHOD create_itab.
*&---

  DATA(lt_main) = VALUE tt_main( ( field1 = 'A1' field2 = 'B1' field3 = 1 )
                     ( field1 = 'A2' field2 = 'B2' field3 = 2 )
                     ( field1 = 'A3' field2 = 'B3' field3 = 2 )
  ).
  me->gt_main = lt_main[].
   lr_out->write(
     EXPORTING
       data   = me->gt_main
       name   = 'Base Internal Table'
     RECEIVING
       output = data(lo_out2)
   ).

  ENDMETHOD.

  METHOD copy_itab.
  DATA(lt_copy) = VALUE tt_main( FOR ls_line in me->gt_main
   WHERE ( field1 = 'A2' AND field2 = 'B2'  )
                 ( field1 =  ls_line-field1
                   field2 = ls_line-field2
                   field3 = ls_line-field3 * 5 )  ).
     lr_out->write(
     EXPORTING
       data   = lt_copy
       name   = 'Copy of Base Internal Table'
     RECEIVING
       output = data(lo_out2)
   ).
  ENDMETHOD.

  METHOD copy_itab_with_feature.
  DATA: lt_filter_copy type tt_main.
  me->gt_filter = VALUE #( ( field3 = 2 ) ).
  Loop at me->gt_main ASSIGNING FIELD-SYMBOL(<fs_line>) WHERE field1 = 'A2' AND field2 = 'B2'.
*    check line_exists( me->gt_filter[ field3 = <fs_line>-field3 ] ).
    data(lv_found) = VALUE #( me->gt_filter[ field3 = <fs_line>-field3 ] DEFAULT 'X').
    if lv_found EQ 'X'.
    CONTINUE.
    endif.
    lt_filter_copy = VALUE #( ( <fs_line> ) ).
  ENDLOOP.
*&--
  if  lt_filter_copy[] is initial .
   return.
  endif.
     lr_out->write(
     EXPORTING
       data   = lt_filter_copy
       name   = 'Filter Copy of Base Internal Table'
     RECEIVING
       output = data(lo_out2)
   ).
  ENDMETHOD.

ENDCLASS.
