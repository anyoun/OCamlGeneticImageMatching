type point2 = int * int
type tri = (point2*point2*point2);;

(*
ocamlopt -verbose -ccopt -O3 -ccopt -mfpmath=sse -ccopt -march=core2 -ccopt -I/usr/local/include/ImageMagick bigarray.cmxa -thread -I +libMagick magick.cmxa unix.cmxa threads.cmxa -I +lablGL lablgl.cmxa lablglut.cmxa -o foo.out genetic_match_fitness.c genetic_match.ml

*)

external test_add : int -> int -> int = "ocaml_test_add"
external calculate_fitness : Magick.image_handle -> Magick.image_handle -> float = "ocaml_calculate_fitness"

let fps = 60;;
let initial_num_triangles = 3;;
let permutations_per_new_triangle = 1000;;
let max_num_permutations = 10000;;
let triangle_alpha = 0.3;;
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

let permute_triangles triangles width height =
  let index = Random.int (Array.length triangles) in
  let copy = Array.copy triangles in
  let new_tri = random_tri width height in
  Array.set copy index new_tri;
  copy
let append_triangle triangles width height =
  let new_tri = random_tri width height in
  Array.append triangles [| new_tri |]

let rec find_fittest orig triangles best_fitness width height n =
  if n = 0 then (best_fitness, triangles)
  else (if n mod permutations_per_new_triangle = 0 then (
    let new_triangles = append_triangle triangles width height in
    let new_image = (draw_triangles_to_image new_triangles width height) in
    let new_fitness = calculate_fitness orig new_image in
    find_fittest orig new_triangles new_fitness width height (n-1) )
  else (
     let new_triangles = permute_triangles triangles width height in
     let candidate = (draw_triangles_to_image new_triangles width height) in
     let other_fitness = calculate_fitness orig candidate in
     (*let other_fitness = fitness orig_raw candidate_raw in*)
     (*print_string "Comparing ";
     print_float best_fitness;
     print_string " to ";
     print_float other_fitness;
     print_newline ();*)
     if other_fitness < best_fitness 
       then find_fittest orig new_triangles other_fitness width height (n-1)
       else find_fittest orig triangles best_fitness width height (n-1)))

let find_fittest_seed orig width height =
  let first_triangles = make_triangles initial_num_triangles width height in
  find_fittest orig first_triangles max_float width height max_num_permutations
             
let fitness_search () =
  Random.self_init ();
  let original = Magick.Fun.reduce_noise 2.0 () (Magick.read_image "monalisa_small.png")  in
  Magick.Imper.set_image_type original Magick.Grayscale;  
  (*Magick.display original;*)
  let width = Magick.get_image_width original in
  let height = Magick.get_image_height original in
  let start_time = Unix.gettimeofday () in
  let (best_fitness, best_triangles) = find_fittest_seed original width height in
  let total_time = (Unix.gettimeofday ()) -. start_time in
  let best_image = draw_triangles_to_image best_triangles width height in
  let image_list = Magick.Imper.new_image_list () in
  Magick.Imper.append_image_to_list image_list original ();
  Magick.Imper.append_image_to_list image_list best_image ();
  Magick.Imper.display_images image_list;
  (*Magick.display original;*)
  print_string "Best fitness: ";
  print_float best_fitness;
  print_string " Total time: ";
  print_float total_time;
  print_string " Time per permutation: ";
  print_float (total_time /. (float_of_int max_num_permutations));
  print_string " Permutations per second: ";
  print_float ((float_of_int max_num_permutations) /. total_time);
  print_newline ()

let _ = fitness_search ()
