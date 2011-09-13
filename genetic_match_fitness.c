#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <bmpfile.h>

/*
clang /usr/local/lib/libbmp.a genetic_match_fitness.c

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

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
*/

typedef struct {
    int x;
    int y;
} Point;
Point MakePoint(int x, int y) {
    Point p = { x, y };
    return p;
}
rgb_pixel_t MakeColor(uint8_t a, uint8_t r, uint8_t g, uint8_t b) {
    rgb_pixel_t p = { b, g, r, a };
    return p;
}

#define BYTES_PER_PIXEL 4
#define MAX(X,Y) ((X)>(Y)?:(X):(Y))

void blend_pixel(bmpfile_t *bmp, int x, int y, rgb_pixel_t c) {
  rgb_pixel_t original_color = *(bmp_get_pixel(bmp, x, y));
  rgb_pixel_t new_color;
  new_color.red = original_color.alpha * original_color.red + c.alpha * c.red;
  new_color.green = original_color.alpha * original_color.green + c.alpha * c.green;
  new_color.blue = original_color.alpha * original_color.blue + c.alpha * c.blue;
  new_color.alpha = original_color.alpha * original_color.alpha + c.alpha * c.alpha;
  bmp_set_pixel(bmp, x, y, new_color);
}
/*
int lerp(int x0, int y0, int x1, int y1, int index) {
  assert(x1 > x0);
  int steps = abs(x1 - x0);
  if(steps == 0) {
      yield return y0;
  } else {
      int slope = (y1 - y0) / steps;
      int y = y0;
      for(int i = 0; i < steps; ++i)
      {
      }
  }
}
*/
void draw_triangle(bmpfile_t *bmp, Point triangle[], rgb_pixel_t c) {
  
}

int main(){
  /*
  BinaryImage image = create_binary_image(100,100);
  for(size_t i = 0; i < 100; ++i)
  {
    set_pixel(image, MakePoint(i,i), MakeColor(123,123,123,123));
  }
  destroy_binary_image(image);
  */
  bmpfile_t *bmp;
  bmp = bmp_create(100,100,32);
  for(int i = 0; i < 100; ++i)
  {
    rgb_pixel_t pixel = {128, 64, 64, 64};
    blend_pixel(bmp, i, i, pixel);
  }
  Point tri[3];
  tri[0] = MakePoint( 50, 0 );
  tri[1] = MakePoint( 0, 50 );
  tri[2] = MakePoint( 100, 100 );
  draw_triangle(bmp, tri, MakeColor( 128, 255, 0, 0 ));
  bmp_save(bmp, "~/Desktop/temp.bmp");
  bmp_destroy(bmp);
  printf("OK\n");
  return 0;  
}