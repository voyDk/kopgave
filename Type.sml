structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  type pos = int*int

  datatype Type = Int | Char | Ref of Type

  fun convertType (S100.Int _) = Int
    | convertType (S100.Char _) = Char

  fun promote Char = Int (* promote char to int *)
    | promote t = t

  fun getName (S100.Val (f,p)) = f
    | getName (S100.Ref (f,p)) = f

  fun getType t (S100.Val (f,p)) = convertType t
    | getType t (S100.Ref (f,p)) = Ref (convertType t)

  fun showTypes [] = "\n"
    | showTypes (Int :: ts) = "int " ^ showTypes ts
    | showTypes (Char :: ts) = "char " ^ showTypes ts
    | showTypes (Ref t :: ts) = "*" ^ showTypes (t ::ts)

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x []
        = NONE
    | lookup x ((y,v)::table)
        = if x=y then SOME v else lookup x table


  fun checkExp e vtable ftable =
    case e of
      S100.NumConst _ => Int
    | S100.CharConst _ => Int (* char is promoted to int in expressions *)
    | S100.StringConst _ => Ref Char
    | S100.LV lv => promote (checkLval lv vtable ftable)
    | S100.Assign (lv,e1,p) =>
        let
	  val t1 = promote (checkLval lv vtable ftable)
	  val t2 = checkExp e1 vtable ftable
	in
	  if t1=t2 then t2
	  else raise Error ("Type mismatch in assignment",p)
	end
    | S100.Plus (e1,e2,p) =>
        (case (checkExp e1 vtable ftable,
	       checkExp e2 vtable ftable) of
	   (Int, Int) => Int
	 | (Int, Ref t) => Ref t
	 | (Ref t, Int) => Ref t
	 | (Ref _, Ref _) => raise Error ("Can't add two references",p)
	 | _ => raise Error ("Invalid types",p))
    | S100.Minus (e1,e2,p) =>
        (case (checkExp e1 vtable ftable,
	       checkExp e2 vtable ftable) of
	   (Int, Int) => Int
	 | (Int, Ref t) => raise Error ("Can't subtract reference from int",p)
	 | (Ref t, Int) => Ref t
	 | (Ref t1, Ref t2) =>
	     if t1=t2 then Int
	     else raise Error ("Can't subtract references of different types",p)
	 | _ => raise Error ("Invalid types",p))
    | S100.Equal (e1,e2,p) =>
        if checkExp e1 vtable ftable = checkExp e2 vtable ftable
	then Int else raise Error ("Can't compare different types",p)
    | S100.Less (e1,e2,p) =>
        if checkExp e1 vtable ftable = checkExp e2 vtable ftable
	then Int else raise Error ("Can't compare different types",p)
    | S100.Call (f,es,p) =>
        (case lookup f ftable of
	   NONE => raise Error ("Unknown function: "^f,p)
	 | SOME (parT,resultT) =>
	     let
	       val parT1 = List.map promote parT
			   (* char passed as int *)
	       val argT = List.map (fn e => checkExp e vtable ftable) es
	     in
	       if parT1 = argT then promote resultT
	       else raise Error ("Arguments don't match declaration of "^f
			         ^"\n"^ showTypes parT ^ showTypes argT,
				 p)
	     end)

  and checkLval lv vtable ftable =
    case lv of
      S100.Var (x,p) =>
        (case lookup x vtable of
	   SOME t => t
	 | NONE => raise Error ("Unknown variable: "^x,p))
    | S100.Deref (x,p) =>
        (case lookup x vtable of
	   SOME (Ref t) => t
	 | SOME _ => raise Error (x^" is not a reference variable",p)
	 | NONE => raise Error ("Unknown variable: "^x,p))
    | S100.Lookup (x,e,p) =>
        (case lookup x vtable of
	   SOME (Ref t) => if checkExp e vtable ftable = Int
			   then t
			   else raise Error ("Index is not integer",p)
	 | SOME _ => raise Error (x^" is not a reference variable",p)
	 | NONE => raise Error ("Unknown variable: "^x,p))

  fun extend [] _ vtable = vtable
    | extend (S100.Val (x,p)::sids) t vtable =
        (case lookup x vtable of
	   NONE => extend sids t ((x,t)::vtable)
	 | SOME _ => raise Error ("Double declaration of "^x,p))
    | extend (S100.Ref (x,p)::sids) t vtable =
        (case lookup x vtable of
	   NONE => extend sids t ((x,Ref t)::vtable)
	 | SOME _ => raise Error ("Double declaration of "^x,p))

  fun checkDecs [] = []
    | checkDecs ((t,sids)::ds) =
        extend (List.rev sids) (convertType t) (checkDecs ds)

  fun checkStat s vtable ftable resultT =
    case s of
      S100.EX e => (checkExp e vtable ftable; ())
    | S100.If (e,s1,p) =>
        if checkExp e vtable ftable = Int
	then checkStat s1 vtable ftable resultT
	else raise Error ("Condition should be integer",p)
    | S100.IfElse (e,s1,s2,p) =>
        if checkExp e vtable ftable = Int
	then (checkStat s1 vtable ftable resultT;
	      checkStat s2 vtable ftable resultT)
	else raise Error ("Condition should be integer",p)
    | S100.While (e,s1,p) =>
        if checkExp e vtable ftable = Int
	then checkStat s1 vtable ftable resultT
	else raise Error ("Condition should be integer",p)
    | S100.Return (e,p) =>
        if checkExp e vtable ftable = promote resultT
	then ()
	else raise Error ("Wrong return type",p)
    | S100.Block (decs,stats,p) =>
        let
	  val vtable1 = checkDecs decs @ vtable
	in
	  List.app (fn s => checkStat s vtable1 ftable resultT) stats
	end

  fun checkReturn s =
    case s of
      S100.EX e => false
    | S100.If (e,s1,p) => false
    | S100.IfElse (e,s1,s2,p) => checkReturn s1 andalso checkReturn s2
    | S100.While (e,s1,p) => false
    | S100.Return (e,p) => true
    | S100.Block (decs,stats,p) => List.exists checkReturn stats

  fun checkFunDec (t,sf,decs,body,p) ftable =
        if checkReturn body
	then checkStat body (checkDecs decs) ftable (getType t sf)
	else raise Error (getName sf ^" can end without return statement",p)

  fun getFuns [] ftable = ftable
    | getFuns ((t,sf,decs,_,p)::fs) ftable =
        case lookup (getName sf) ftable of
	  NONE =>
            let
              val parT = (List.map (#2) (checkDecs decs))
	      val resultT = getType t sf
	    in
              getFuns fs ((getName sf, (parT,resultT)) :: ftable)
	    end
	| SOME _ => raise Error ("Redeclaration of "^ getName sf,p)

  fun checkProg fs =
    let
      val ftable = getFuns fs [("walloc",([Int],Ref Int)),
			       ("balloc",([Int],Ref Char)),
			       ("getint",([],Int)),
			       ("getstring",([Int],Ref Char)),
			       ("putint",([Int],Int)),
			       ("putstring",([Ref Char],Ref Char))]
    in
      List.app (fn f => checkFunDec f ftable) fs;
      case lookup "main" ftable of
	NONE => raise Error ("No main function found",(0,0))
      | SOME ([],Int) => ()
      | _ => raise Error ("main function has illegal type",(0,0))
    end

end
