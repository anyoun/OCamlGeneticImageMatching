#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <stdio.h>
#include <stdlib.h>

#include <magick/ImageMagick.h>

int test_add(int x, int y) {
  return x + y;
}

value ocaml_test_add(value x, value y) {
  return Val_int(test_add(Int_val(x), Int_val(y)));
}

double calculate_fitness(Image *orginal, Image *candidate) {
  double distortion;
  ExceptionInfo exception;
  GetExceptionInfo(&exception);//Init exception
  
  MagickBooleanType didSucceed;
  didSucceed = GetImageChannelDistortion(
    orginal,
    candidate,
    DefaultChannels,
    MeanSquaredErrorMetric,
    &distortion,
    &exception);
    
  if (exception.severity != UndefinedException) {
    failwith( exception.reason );
  }

  DestroyExceptionInfo(&exception);
    
  return distortion;
}

value ocaml_calculate_fitness(value original, value candidate) {
  CAMLparam2(original, candidate) ;

  float fitness = calculate_fitness( (Image *) Field(original,1),
                                     (Image *) Field(candidate,1));

  CAMLreturn ( caml_copy_double(fitness) ) ;
}
