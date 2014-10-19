fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (s : string, xs:string list) =
    let fun aux (xs', s, acc) = 
        case xs' of
            [] => NONE
         | s'::ys' => if same_string(s,s') then SOME (acc @ ys') else aux(ys', s, acc @ [s'])
    in 
        aux(xs,s,[])
    end

fun get_substitutions1 (l : string list list, s:string) = 
    case l of
        [] => []
     | xs::ys =>  let val z = all_except_option(s,xs)
                  in
                     case z of
                         SOME l' => l' @ get_substitutions1(ys,s)
                      |  NONE => get_substitutions1(ys,s)
                  end 

fun get_substitutions2 (l : string list list, s:string) = 
    let fun aux (l,s,acc) = 
            case l of
                [] => acc
             |  xs::ys =>  let val z = all_except_option(s,xs)
                           in
                               case z of
                                   SOME l' => aux(ys,s, acc @ l')
                                |  NONE => aux(ys,s, acc)
                            end 
    in
        aux(l,s,[])
    end

fun similar_names(l : string list list, full_name :{first : string, middle: string, last:string}) = 
    let fun list_sub_names(names : string list) = 
        case names of
            [] => []
         |  x::xs =>  [{first = x, middle = (#middle full_name), last = (#last full_name)}] @ list_sub_names(xs)
    in 
       [full_name] @ list_sub_names(get_substitutions2(l, #first full_name))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (x : card) = 
    case x of
        (Clubs, _) => Black
     |  (Spades,_) => Black
     |   _ => Red      

fun card_value (x:card) = 
    case #2 x of
        Num(n) => n
     | Ace => 11
     | _ => 10 

fun remove_card(cs : card list, c : card, e) = 
    let fun aux (cs', c', e, acc) = 
        case cs' of
            [] => raise e
         | d::ds => if d = c' then acc @ ds else aux(ds, c', e, acc @ [d])
    in 
        aux(cs,c,e,[])
    end

fun all_same_color(cs: card list) =
    case cs of
        [] => true
     | x::[] => true
     | x::y::ys => card_color(x)=card_color(y)  andalso all_same_color(y::ys)
   
fun sum_cards(cs : card list) = 
    let fun aux(cs,acc) = 
        case cs of
            [] => acc
         | x::xs => aux(xs,card_value(x)+acc)
    in
        aux(cs,0)
    end 

fun score(cs: card list, goal: int) = 
    let val sum = sum_cards(cs)
        val b = goal < sum
        val sc = all_same_color(cs) 
    in 
        case (b,sc) of
            (true,true) => 3*(sum-goal) div 2
         | (false,true) => (goal-sum) div 2
         | (true,false) => 3*(sum-goal)
         |  _ => goal-sum
    end

fun officiate(cs : card list, ms : move list, goal: int) = 
    let fun state(cs,ms,goal,hand) = 
        case ms of
            [] => hand
          | Discard(c)::xs => state(cs,xs,goal,remove_card(hand,c,IllegalMove))
          | Draw::xs => case cs of
                           [] => hand
                        | c::rest => if card_value(c) + sum_cards(hand) > goal then c::hand else state(rest,xs,goal,c::hand)
    in
        score(state(cs,ms,goal,[]),goal)
    end
   

        
