CLASS zbtp_class_01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zbtp_class_01 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
   out->write(
     EXPORTING
       data   = 'Hello ABAP Cloud  Branch1!!'
       name   = 'Tapomay'
     RECEIVING
       output = data(lo_out)
   ).

  ENDMETHOD.

ENDCLASS.
