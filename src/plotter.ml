open Core.Std;;

type circle = { x : int; y : int; radius : int; color : int }

let height = 500;;
let width  = 800;;
let extra  = 1.05;;

let get_val l comp =
	List.fold_left l ~init:(List.hd_exn l) 
		~f:(fun a x -> if comp x a then x else a)
	
let get_min l = get_val l (<)
let get_max l = get_val l (>)

let graph_stock ?offset:(offset=0) ?color:(color=Graphics.black) (data : float list) =
	let _ = Graphics.set_color color in
	let min_x = 1 in let max_x = List.length data in
	let min_y = (get_min data) /. extra in let max_y = (get_max data) *. extra in
	let scale_x = max_x - min_x in
	let scale_y = max_y -. min_y in
	let ys = List.map data (fun t -> Int.of_float ((Float.of_int height) *. (t -. min_y) /. scale_y)) in
	let r = Array.of_list ys in
	let (a,b) = Graphics.current_point () in
	Graphics.moveto (width * (offset - min_x) / scale_x) r.(0);
	for i = 1 to (Array.length r)-1 do
		let y = r.(i) in Graphics.lineto (width * (offset + i - min_x) / scale_x) y
	done;
	Graphics.moveto a b;;

let draw_circles (circles : circle list) =
	List.iter circles (fun {x=x;y=y;radius=r;color=c} -> (Graphics.set_color c; Graphics.fill_circle x y r))

(*
let draw_poly r =
let (a,b) = Graphics.current_point () in
let (x0,y0) = r.(0) in Graphics.moveto x0 y0;
for i = 1 to (Array.length r)-1 do
let (x,y) = r.(i) in Graphics.lineto x y
done;
Graphics.lineto x0 y0;
Graphics.moveto a b;;

let pi = 3.1415927;;
let net_points (x,y) l n =
let a = 2. *. pi /. (float n) in
let rec aux (xa,ya) i =
if i > n then []
else
let na = (float i) *. a in
let x1 = xa + (Int.of_float ( cos(na) *. l))
and y1 = ya + (Int.of_float ( sin(na) *. l)) in
let np = (x1,y1) in
np :: (aux np (i+1))
in Array.of_list (aux (x,y) 1);;

let draw_net (x,y) l n sc st =
let r = net_points (x,y) l n in
draw_poly r;
let draw_machine (x,y) =
Graphics.set_color Graphics.background;
Graphics.fill_circle x y sc;
Graphics.set_color Graphics.foreground;
Graphics.draw_circle x y sc
in
Array.iter r draw_machine;
Graphics.fill_circle x y st;;
*)
