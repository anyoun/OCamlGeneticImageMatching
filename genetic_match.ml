type point2 = int * int
type tri = (point2*point2*point2);;

(*
ocamlopt -verbose -ccopt -O3 -ccopt -mfpmath=sse -ccopt -march=core2 -ccopt -I/usr/local/include/ImageMagick bigarray.cmxa -thread -I +libMagick magick.cmxa unix.cmxa threads.cmxa -I +lablGL lablgl.cmxa lablglut.cmxa -o foo.out genetic_match_fitness.c genetic_match.ml

*)

external test_add : int -> int -> int = "ocaml_test_add"
external calculate_fitness : Magick.image_handle -> Magick.image_handle -> float = "ocaml_calculate_fitness"

let fps = 60;;
let initial_num_triangles = 3;;
let permutations_per_new_triangle = 5000;;
let max_num_permutations = 10000;;
let triangle_alpha = 0.3;;
let color_int_max = 65535;;
let triangle_color = Magick.Imper.color_of_rgbo_tuple (color_int_max, color_int_max, color_int_max, int_of_float(float_of_int(color_int_max) *. triangle_alpha));;

let best_triangles =
  [
    ((0,0),(0,300),(300,0));
    ((0,300),(300,600),(600,300))
  ];;
  
class view = 
    let image = Magick.Fun.reduce_noise 2.0 () (Magick.read_image "monalisa_small.png")  in
    let _ = Magick.Imper.set_image_type image Magick.Grayscale in
  object (self)
    val original = image
    val width = Magick.get_image_width image
    val height = Magick.get_image_height image
    val mutable best_triangles : tri array = [| ((0,0),(0,300),(300,0)) |]
    val mutable best_fitness = max_float
    val mutable permutation_count = 0
    val start_time = Unix.gettimeofday ()
    
    method get_width = width
    method get_height = height
    method get_best_fitness = best_fitness
    method get_permutation_count = permutation_count
      
    method random_point () = (Random.int(width),Random.int(height))
    method random_tri () = ( self#random_point (), self#random_point (), self#random_point () )
    method make_triangles n = Array.init n (fun i -> self#random_tri ())

    method draw_triangle_to_image (t:tri) (img:Magick.image_handle) =
      let points = match t with (a,b,c) -> [|a;b;c|] in
      Magick.Imper.draw_polygon img ~coords:points ~fill_color:triangle_color ()
    method draw_triangles_to_image tris =
      let img = Magick.get_canvas ~width:width ~height:height ~color:"#000000" in
      Array.iter (fun t -> self#draw_triangle_to_image t img) tris;
      img
  
    method permute_triangles triangles =
      let index = Random.int (Array.length triangles) in
      let copy = Array.copy triangles in
      let old_tri = Array.get copy index in
      let new_tri = match old_tri with (a,b,c) -> 
        match Random.int 3 with 
          0 -> (self#random_point (),b,c)
        | 1 -> (a,self#random_point (),c)
        | 2 -> (a,b,self#random_point ()) in
      Array.set copy index new_tri;
      copy
    method append_triangle triangles =
      let new_tri = self#random_tri () in
      Array.append triangles [| new_tri |]
      
    method next_fittest () =
      permutation_count <- permutation_count + 1;
      if permutation_count mod permutations_per_new_triangle = 0 then (
        (best_triangles <- self#append_triangle best_triangles);
        let new_image = self#draw_triangles_to_image best_triangles in
        best_fitness <- calculate_fitness original new_image;
        ()
      ) else (
        let new_triangles = self#permute_triangles best_triangles in
        let candidate = self#draw_triangles_to_image new_triangles in
        let other_fitness = calculate_fitness original candidate in
        if other_fitness < best_fitness 
          then (best_triangles <- new_triangles; best_fitness <- other_fitness ; ())
          else ()
      )

    method draw_triangles t =
      let conv (x,y) = (float_of_int(x),float_of_int(height - y)) in
      match t with
        (a,b,c) ->
          GlDraw.begins `triangles;
          GlDraw.vertex2 (conv a);
          GlDraw.vertex2 (conv b);
          GlDraw.vertex2 (conv c);
          GlDraw.ends ()
    method display = 
      GlClear.clear [`color;`depth];
      GlDraw.color ~alpha:triangle_alpha (1.0,1.0,1.0);
      GlMat.mode `projection;
      GlMat.load_identity ();
      GlMat.ortho ~x:(0.0,float_of_int width) ~y:(0.0,float_of_int height) ~z:(-1.0,1.0);
      Array.iter self#draw_triangles best_triangles;
      Gl.flush ();
      Glut.swapBuffers ()
    method idle = 
      let last_fitness = best_fitness in
      self#next_fittest (); 
      if best_fitness = last_fitness
        then ()
        else (
          print_string "Best fitness: ";
          print_float best_fitness;
          print_string " after interations: ";
          print_int permutation_count;
          print_string " Interations per second: ";
          print_float ((float_of_int permutation_count) /. ((Unix.gettimeofday ()) -. start_time));
          print_newline ();
          self#save_to_disk;
          Glut.postRedisplay () )
    
    method save_to_disk =
        let file = Unix.openfile "triangles" [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 in
      let chan = Unix.out_channel_of_descr file in
      Marshal.to_channel chan best_triangles [Marshal.No_sharing];
      flush chan;
      Unix.close file
    method read_from_disk =
      try
        let file = Unix.openfile "triangles" [Unix.O_RDONLY] 0o640 in
        let chan = Unix.in_channel_of_descr file in
        let triangles = Marshal.from_channel chan in
        Unix.close file;
        Some triangles
      with x-> None
      
    initializer
      let t : tri array option = self#read_from_disk in
      match t with
        None -> () |
        Some triangles ->
          (best_triangles <- triangles);
          let candidate = self#draw_triangles_to_image best_triangles in
          (best_fitness <- calculate_fitness original candidate);
end;;
    
let main_opengl () =
  Random.self_init ();
  ignore(Glut.init Sys.argv);
  let v = new view in
  Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true () ;
  Glut.initWindowSize ~w:(v#get_width*2) ~h:(v#get_height*2) ;
  ignore(Glut.createWindow ~title:"Genetic Match");
  Gl.enable `blend;
  GlFunc.blend_func `src_alpha `one_minus_src_alpha ;
  Glut.displayFunc ~cb:(fun () -> v#display);
  Glut.idleFunc ~cb:(Some (fun () -> v#idle));
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y ->
      match key with
      |    27 (*esc*) -> exit 0
      | _ -> ());
  
  let start_time = Unix.gettimeofday () in
  let total_time = (Unix.gettimeofday ()) -. start_time in
  
  Glut.mainLoop ();
  
  print_string "Best fitness: ";
  print_float v#get_best_fitness;
  print_string " Total time: ";
  print_float total_time;
  print_string " Time per permutation: ";
  print_float (total_time /. (float_of_int v#get_permutation_count));
  print_string " Permutations per second: ";
  print_float ((float_of_int v#get_permutation_count) /. total_time);
  print_newline ()

let _ = main_opengl ()
