(* drawing.ml
   OpenGL drawing stuff.
*)

open Mass
open Vector

let initLight () =
  let light_ambient = 0.5, 0.5, 0.5, 1.0
  and light_diffuse = 1.0, 1.0, 1.0, 1.0
  and light_specular = 1.0, 1.0, 1.0, 1.0
    (*  light_position is NOT default value	*)
  and light_position = 1.0, 1.0, 1.0, 0.0
  in
    List.iter (GlLight.light ~num:0)
      [ `ambient light_ambient; `diffuse light_diffuse;
	`specular light_specular; `position light_position ];

    Gl.enable `lighting;
    Gl.enable `light0;
    Gl.enable `depth_test;
;;


let initGL w h =
  GlDraw.shade_model `smooth;
  GlDraw.polygon_mode `both `line;
  GlClear.color (0., 0., 0.);
  GlClear.depth 1.0;
  GlClear.clear [`color; `depth];
  Gl.enable `depth_test;
  GlFunc.depth_func `lequal;

  GlMisc.hint `perspective_correction `nicest;

  GlDraw.viewport ~x: 0 ~y: 0 ~w: w ~h: h;
  GlMat.mode `projection;
  GlMat.load_identity ();
  (* Note: this sets the min and max z distances *)
  GluMat.perspective ~fovy: 60. ~aspect: ((float_of_int w) /. (float_of_int h)) ~z: (1., 500.);
  GlMat.mode `modelview;
  GlMat.load_identity ();


  initLight ();

;;




let drawSphere x y z r =
(*  Printf.printf "Drawing sphere at %f %f %f\n" x y z;*)
  GlMat.push ();
(*  GlMat.rotate *)

  GlLight.material ~face: `front (`specular (0.5, 0.5, 0.5, 1.));
  GlLight.material ~face: `front (`shininess 5.);

  GlMat.translate ~x: x ~y: y ~z: z ();
  GluQuadric.sphere ~radius: r ~slices: 8 ~stacks: 8 ();
  GlMat.pop ();
;;

let drawParticle x y z =
  Gl.disable `lighting;
  GlMat.push ();
(*  GlMat.translate ~x: x ~y: y ~z: z (); *)
(*  GluQuadric.sphere ~radius: 0.02 ~slices: 3 ~stacks: 3 (); *)
  GlDraw.color (1., 0., 0.);
  GlDraw.begins `points;
  GlDraw.vertex ~x: x ~y: y ~z: z ();
(*  GlDraw.vertex ~x: (x +. 0.1) ~y: y ~z: z (); *)
(*  GlDraw.vertex ~x: x ~y: (y +. 0.1) ~z: z (); *)
  GlDraw.ends ();
  GlMat.pop ();
  Gl.enable `lighting;
;;



class drawer =
object (self)
  val mutable masses = []
  val mutable scale = 8e10
  val mutable camera = new Camera.camera (new mass 0. (new vector 0. 0. 0.) (new vector 0. 0. 0.))

  val mutable continue = true

  method stop = continue <- false

  initializer

  let _ = Sdlvideo.set_video_mode ~w: 800 ~h: 600 
    ~bpp: 16 [`DOUBLEBUF; `OPENGL] in
    
    initGL 800 600;

  method getCamera = camera

  method drawParticles p =
    let drawP (m : Particles.particle) =
      let x = (m#pos#x /. scale)
      and y = (m#pos#y /. scale)
      and z = (m#pos#z /. scale) in
	drawParticle x y z
    in
      List.iter drawP p


  method drawMasses =
    let drawMass (m : mass) =
      let x = (m#pos#x /. scale)
      and y = (m#pos#y /. scale)
      and z = (m#pos#z /. scale) in
	drawSphere x y z 0.2
    in
      List.iter drawMass masses

  method doCamera =
    (*    GlMat.translate ~x: camerapos#x ~y: camerapos#y ~z: camerapos#z ();  *)
    let f = camera#getFocus in
    let cameravec = camera#getPos#add (f#pos#div scale)
    and cameraview = f#pos#div scale in
      GluMat.look_at ~eye: (cameravec#x, cameravec#y, cameravec#z)
	~center: (* (0., 0., 0.) *) (cameraview#x, cameraview#y, cameraview#z) 
	~up: (0.0, 0., 1.);
	


  method draw items particles =
    masses <- items;

    GlMat.push ();      
    GlClear.clear [`color; `depth];
    self#doCamera; 

    self#drawMasses;
    self#drawParticles particles;


    Sdlgl.swap_buffers ();
    GlMat.pop ();


end;;
