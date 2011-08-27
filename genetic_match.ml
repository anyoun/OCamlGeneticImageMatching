(*#!/usr/bin/env ocaml
#directory "+libMagick" ;;
#load "bigarray.cma" ;;
#load "magick.cma" ;; 

#load "unix.cma" ;; 

#directory "+lablGL" ;;
#load "lablgl.cma" ;; 
#load "lablglut.cma" ;; 
*)
type point2 = int * int
type tri = (point2*point2*point2);;

(*let x_max = 600.0
let y_max = 600.0

ocamlopt -verbose -ccopt -O3 -ccopt -mfpmath=sse -ccopt -march=core2 -ccopt -I/usr/local/include/ImageMagick bigarray.cmxa -thread -I +libMagick magick.cmxa unix.cmxa threads.cmxa -I +lablGL lablgl.cmxa lablglut.cmxa -o foo.out genetic_match_fitness.c genetic_match.ml


*)

external test_add : int -> int -> int = "ocaml_test_add"
external calculate_fitness : Magick.image_handle -> Magick.image_handle -> float = "ocaml_calculate_fitness"

let fps = 60;;
let num_triangles = 10;;
let num_permutations = 10000;;
let permutation_chance = 0.6;;
let triangle_alpha = 0.5;;
let color_int_max = 65535;;
let triangle_color = Magick.Imper.color_of_rgbo_tuple (color_int_max, color_int_max, color_int_max, int_of_float(float_of_int(color_int_max) *. triangle_alpha));;

let best_triangles =
  [
    ((0,0),(0,300),(300,0));
    ((0,300),(300,600),(600,300))
  ];;
  
let random_tri width height =
  let random_point () = (Random.int(width),Random.int(height)) in
  ( random_point (), random_point (), random_point () )

let rec make_triangles n width height =
  Array.init n (fun i -> random_tri width height)

let draw_triangle_to_image (t:tri) (img:Magick.image_handle) =
  let points = match t with (a,b,c) -> [|a;b;c|] in
  Magick.Imper.draw_polygon img ~coords:points ~fill_color:triangle_color ()

let draw_triangles_to_image tris width height =
  let img = Magick.get_canvas ~width:width ~height:height ~color:"#000000" in
  Array.iter (fun t -> draw_triangle_to_image t img) tris;
  img;;

let fitness orig_raw candidate_raw = 
  let fitness_of_channel (o:int) (c:int) : float = abs_float ((float o) -. (float c)) in
  let fitness_of_pixel o c =
    match o with (orig_r,orig_g,orig_b) -> 
      match c with (cand_r,cand_g,cand_b) ->
        (fitness_of_channel orig_r cand_r) +.
        (fitness_of_channel orig_g cand_g) +.
        (fitness_of_channel orig_b cand_b) in
  let rec fitness_of_line o c col =
    match col with
        0 -> 0.0
      | _ -> (fitness_of_pixel (Array.get o col) (Array.get c col)) +. (fitness_of_line o c (col-1)) in
  let rec fitness_line o c n =
    match n with
        0 -> 0.0
      | _ -> let orig_line = Array.get o n in
             let cand_line = Array.get c n in
             let len = (Array.length orig_line) - 1 in
             (fitness_of_line orig_line cand_line len) +. (fitness_line o c (n-1)) in
  let len = (Array.length orig_raw)-1 in
  fitness_line orig_raw candidate_raw len

let draw_triangles t =
  let conv v = match v with (x,y) -> (float_of_int(x),float_of_int(y)) in
  match t with
    (a,b,c) ->
      GlDraw.begins `triangles;
      GlDraw.vertex2 (conv a);
      GlDraw.vertex2 (conv b);
      GlDraw.vertex2 (conv c);
      GlDraw.ends ()

let display_function() = 
  GlClear.clear [`color;`depth];
  GlDraw.color (1.0,1.0,1.0);
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0.0,600.0) ~y:(0.0,600.0) ~z:(-1.0,1.0);
  Array.iter draw_triangles (make_triangles 6 600 600);
  Gl.flush ();
  Glut.swapBuffers ()

let main_opengl () =
  ignore(Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true () ;
  Glut.initWindowSize ~w:(600) ~h:(600) ;
  ignore(Glut.createWindow ~title:"Genetic Match");
  Glut.displayFunc ~cb:display_function;
  let rec idle ~value = display_function (); Glut.timerFunc ~ms:(1000/fps) ~cb:idle ~value:0 in 
  Glut.timerFunc ~ms:(1000/fps) ~cb:idle ~value:0;
  Glut.specialFunc ~cb:(fun ~key ~x ~y ->
      match key with 
      | Glut.KEY_UP -> GlMat.rotate ~angle:(-5.) ~z:1.0 (); display_function ()
      |	Glut.KEY_DOWN -> GlMat.rotate ~angle:(5.) ~z:1.0 (); display_function ()
      |	Glut.KEY_LEFT -> GlMat.rotate ~angle:(5.) ~x:1.0 (); display_function ()
      |	Glut.KEY_RIGHT -> GlMat.rotate ~angle:(-5.) ~x:1.0 (); display_function ()
      |	_ -> ());
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y ->
      match key with
      |	27 (*esc*) -> exit 0
      | _ -> ());
  Glut.mainLoop ()

let permute_triangles triangles width height =
  let index = Random.int (Array.length triangles) in
  let copy = Array.copy triangles in
  let new_tri = random_tri width height in
  Array.set copy index new_tri;
  copy

let rec find_fittest orig orig_raw triangles best_fitness width height n =
  match n with
    0 -> (best_fitness, triangles)
  | _ -> (*let new_triangles = make_triangles 6 width height in*)
         let new_triangles = permute_triangles triangles width height in
         let candidate = (draw_triangles_to_image new_triangles width height) in
         (*let candidate_raw = Magick.Imper.get_raw_without_alpha candidate in*)
         let other_fitness = calculate_fitness orig candidate in
         (*let other_fitness = fitness orig_raw candidate_raw in*)
         (*print_string "Comparing ";
         print_float best_fitness;
         print_string " to ";
         print_float other_fitness;
         print_newline ();*)
         if other_fitness < best_fitness 
           then find_fittest orig orig_raw new_triangles other_fitness width height (n-1)
           else find_fittest orig orig_raw triangles best_fitness width height (n-1)
let find_fittest_seed orig orig_raw width height =
  let first_triangles = make_triangles num_triangles width height in
  find_fittest orig orig_raw first_triangles max_float width height num_permutations
             
let fitness_search () =
  Random.self_init ();
  let original = Magick.Fun.reduce_noise 2.0 () (Magick.read_image "monalisa_small.png")  in
  Magick.Imper.set_image_type original Magick.Grayscale;  
  (*Magick.display original;*)
  let width = Magick.get_image_width original in
  let height = Magick.get_image_height original in
  let orig_raw = Magick.Imper.get_raw_without_alpha original in
  let start_time = Unix.gettimeofday () in
  let (best_fitness, best_triangles) = find_fittest_seed original orig_raw width height in
  let total_time = (Unix.gettimeofday ()) -. start_time in
  let best_image = draw_triangles_to_image best_triangles width height in
  let image_list = Magick.Imper.new_image_list () in
  Magick.Imper.append_image_to_list image_list original ();
  Magick.Imper.append_image_to_list image_list best_image ();
  Magick.Imper.display_images image_list;
(*  Magick.display original;*)
  print_string "Best fitness: ";
  print_float best_fitness;
  print_string " Total time: ";
  print_float total_time;
  print_string " Time per permutation: ";
  print_float (total_time /. (float_of_int num_permutations));
  print_string " Permutations per second: ";
  print_float ((float_of_int num_permutations) /. total_time);
  print_newline ()

let _ = fitness_search ()
