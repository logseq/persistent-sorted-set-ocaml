#include <caml/mlvalues.h>

CAMLprim value WeakRef(value node) {
  (void)node;
  return Val_unit;
}

CAMLprim value deref(value weak_ref) {
  (void)weak_ref;
  return Val_int(0);
}
