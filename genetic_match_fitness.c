#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <bmpfile.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

/*
clang /usr/local/lib/libbmp.a -I/opt/godi/lib/ocaml/std-lib  genetic_match_fitness.c
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
    uint32_t x;
    uint32_t y;
} Point;
typedef struct {
    Point start;
    Point end;
} Rect;
typedef struct {
    Point start;
    Point end;
    rgb_pixel_t color;
} ColorRect;
typedef struct {
  ColorRect * Rects;
  int Count;
} ColorRects;
Point MakePoint(uint32_t x, uint32_t y) {
    Point p = { x, y };
    return p;
}
Rect MakeRect(uint32_t x, uint32_t y, uint32_t x2, uint32_t y2) {
    Rect p = { { x, y }, { x2, y2 } };
    return p;
}
rgb_pixel_t MakeColor(uint8_t a, uint8_t r, uint8_t g, uint8_t b) {
    rgb_pixel_t p = { b, g, r, a };
    return p;
}
ColorRect MakeColorRect(uint32_t x, uint32_t y, uint32_t x2, uint32_t y2, uint8_t a, uint8_t r, uint8_t g, uint8_t b) {
    ColorRect p = { { x, y }, { x2, y2 }, { b, g, r, a } };
    return p;
}

#define BYTES_PER_PIXEL 4
#define MAX(X,Y) ((X)>(Y)?:(X):(Y))
#define IMAGE_HEIGHT 400
#define IMAGE_WIDTH 400

void blend_pixel(bmpfile_t *bmp, uint32_t x, uint32_t y, rgb_pixel_t c) {
  rgb_pixel_t original_color = *(bmp_get_pixel(bmp, x, y));
  rgb_pixel_t new_color;
  new_color.red = original_color.alpha * original_color.red + c.alpha * c.red;
  new_color.green = original_color.alpha * original_color.green + c.alpha * c.green;
  new_color.blue = original_color.alpha * original_color.blue + c.alpha * c.blue;
  new_color.alpha = original_color.alpha * original_color.alpha + c.alpha * c.alpha;
  bmp_set_pixel(bmp, x, y, new_color);
}

void draw_rect(bmpfile_t *bmp, Rect r, rgb_pixel_t c) {
	for(uint32_t y = r.start.y; y < r.end.y; ++y) {
		for(uint32_t x = r.start.x; x < r.end.x; ++x)	{
			blend_pixel(bmp, x, y, c);
		}
	}
}
void draw_rects(bmpfile_t *bmp, ColorRects rects) {
  for(uint32_t i = 0; i < rects.Count; ++i) {
    ColorRect r = rects.Rects[i];
    for(uint32_t y = r.start.y; y < r.end.y; ++y) {
      for(uint32_t x = r.start.x; x < r.end.x; ++x) {
        blend_pixel(bmp, x, y, r.color);
      }
    }
  }
}

float calculate_fitness(bmpfile_t *original, bmpfile_t *candidate) {
	float error = 0;
	uint32_t width = bmp_get_width(original);
	uint32_t height = bmp_get_height(original);
	for(uint32_t y = 0; y < height; ++y) {
		for(uint32_t x = 0; x < width; ++x)	{
			rgb_pixel_t orig_pixel = *(bmp_get_pixel(original, x, y));
			rgb_pixel_t cand_pixel = *(bmp_get_pixel(candidate, x, y));
			float pixelError = 
				  (orig_pixel.red - cand_pixel.red) * (orig_pixel.red - cand_pixel.red)
				+ (orig_pixel.blue - cand_pixel.blue) * (orig_pixel.blue - cand_pixel.blue)
				+ (orig_pixel.green - cand_pixel.green) * (orig_pixel.green - cand_pixel.green);
			error += pixelError;
		}
	}
  return 1-error;
}

ColorRects unbox_rects(value boxed) {
  int size = Wosize_val(boxed);
  ColorRects rects;
  rects.Rects = malloc(size * sizeof(ColorRect));
  rects.Count = size;
  for (int i = 0; i < size; ++i)
  {
    value r = Field(boxed, i);
    rects.Rects[i] = MakeColorRect(Field(r,0), Field(r,1), Field(r,2), Field(r,3), Field(r,4), Field(r,5), Field(r,6), Field(r,7));
  }
  return rects;
}

value ocaml_calculate_fitness(value original, value candidate) {
  CAMLparam2(original, candidate) ;

  ColorRects orig_rects = unbox_rects(original);
  bmpfile_t *orig_bmp;
  orig_bmp = bmp_create(IMAGE_WIDTH, IMAGE_HEIGHT, 32);
  draw_rects(orig_bmp, orig_rects);

  ColorRects candidate_rects = unbox_rects(candidate);
  bmpfile_t *candidate_bmp;
  candidate_bmp = bmp_create(IMAGE_WIDTH, IMAGE_HEIGHT, 32);
  draw_rects(candidate_bmp, candidate_rects);

  float fitness = calculate_fitness(orig_bmp, candidate_bmp);

  free(orig_rects.Rects);
  bmp_destroy(orig_bmp);
  free(candidate_rects.Rects);
  bmp_destroy(candidate_bmp);

  CAMLreturn ( caml_copy_double(fitness) ) ;
}

// void finalize_image(value image) {
//   CAMLparam1(image);
//   bmp_destroy((bmpfile_t *) Field(image,1));
// }

// value ocaml_draw_image(value squares, value image, value width, value height) {
// 	CAMLparam4(squares, image, width, height);
// 	CAMLlocal1(new_image);
//   int used = width * height * 8;
//   int max = 1000000000;
// 	new_image = alloc_final(2, (*finalize_image), sizeof(bmpfile_t *), used, max);
// 	CAMLreturn(new_image);
// }

int main(){
  bmpfile_t *bmp;
  bmp = bmp_create(100,100,32);
  for(int i = 0; i < 100; ++i)
  {
    rgb_pixel_t pixel = {128, 64, 64, 64};
    blend_pixel(bmp, i, i, pixel);
  }
  Rect r = MakeRect( 50, 50, 100, 100 );
  draw_rect(bmp, r, MakeColor( 128, 255, 0, 0 ));
  bmp_save(bmp, "temp.bmp");
  bmp_destroy(bmp);
  printf("OK\n");
  return 0;  
}