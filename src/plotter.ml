open Core.Std;;

let extra  = 1.05;;

let get_val l comp =
	List.fold_left l ~init:(List.hd_exn l) 
		~f:(fun a x -> if comp x a then x else a)

let get_min l = get_val l (<)
let get_max l = get_val l (>)

class plotter = 
		(*let _ = (Graphics.open_graph ""; Graphics.resize_window 800 500) in*)
		object(self)
		  val mutable height = 500
			val mutable width = 800
			val mutable min_x = 0
			val mutable max_x = 10
			val mutable min_y = 0.
			val mutable max_y = 10.
			val mutable scale_x = 10
			val mutable scale_y = 10.

			method set_height h = height <- h
			method set_width w = width <- w

			method set_scale (data : float list) =
				min_x <- 0; max_x <- List.length data; 
				min_y <- (get_min data) /. extra;
				max_y <- (get_max data) *. extra;
				scale_x <- max_x - min_x;
				scale_y <- max_y -. min_y

			method private to_x_pixel offset (x : int) = 
				(width * (offset + x - min_x) / scale_x)

			method private to_y_pixel (y : float) = 
				Int.of_float ((Float.of_int height) *. (y -. min_y) /. scale_y)

		  method graph_stock ?offset:(offset=0) ?color:(color=Graphics.black) (data : float list) =
				let _ = Graphics.set_color color in
				let ys = List.map data self#to_y_pixel in
				let r = Array.of_list ys in
				let (a,b) = Graphics.current_point () in
				Graphics.moveto (self#to_x_pixel offset 0) r.(0);
				for i = 1 to (Array.length r)-1 do
					let y = r.(i) in Graphics.lineto (self#to_x_pixel offset i) y
				done;
				Graphics.moveto a b

		  method draw_circles ?text:(text=false) circles =
				List.iter 
					circles 
					(fun (x,y,r,c) -> 
						(Graphics.set_color c; Graphics.fill_circle (self#to_x_pixel 0 x) (self#to_y_pixel y) r; 
						let (cx,cy) = Graphics.current_point () in 
						if (text) 
						then (Graphics.moveto (self#to_x_pixel (-20) x) ((self#to_y_pixel y) + 10); 
									Graphics.draw_string (Float.to_string y); 
									Graphics.moveto cx cy)))
  end ;;


